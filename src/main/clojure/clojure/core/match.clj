(ns clojure.core.match
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match.protocols])
  (:require [clojure.set :as set]
            [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.jvm :as ana-jvm]
            [clojure.tools.analyzer.passes.jvm.annotate-loops :as loops])
  (:import [java.io Writer]
           [clojure.core.match.protocols IExistentialPattern IPseudoPattern]))


;; =============================================================================
;; # Introduction
;;
;; This namespace contains an implementation of closed pattern
;; matching. It uses an algorithm based on Luc Maranget's paper
;; "Compiling Pattern Matching to Good Decision Trees".
;;
;; There are three main steps to this implementation:
;;
;; 1. *Converting Clojure syntax to a Pattern Matrix*:
;;    The function `emit-matrix` does this work.
;;    A Pattern Matrix is represented by PatternMatrix.
;;
;; 2. *Compiling the Pattern Matrix to a Directed Acyclic Graph*:
;;    The function `compile` does this work. This step
;;    is where Maranget's algorithm is implemented.
;;
;; 3. *Converting the DAG to Clojure code*:
;;    This is mostly a 1-1 conversion. See function `executable-form`.
;;

;; # Nomenclature
;;
;; * x and y are called _occurrences_
;; * 1, 2, 3 and 4 are _patterns_
;; * [1 2] and [3 4] are _pattern rows_
;; * :a0 and :a1 are _actions_
;; 

;; ============================================
;; # Debugging tools
;;
;; These debugging aids are most useful in steps 2 and 3 of compilation.
;;

;; TODO allow these to be set dynamically, at macro-expand time.
;; Maybe match macros could take extra metadata? - Ambrose
(def ^{:dynamic true
       :doc "Enable syntax check of match macros"} 
  *syntax-check* (atom true))

(def ^{:dynamic true}
  *clojurescript* false)

(def ^{:dynamic true} *line*)
(def ^{:dynamic true} *locals* nil)
(def ^{:dynamic true} *warned*)

(def ^{:dynamic true
       :doc "Allow map matching syntax to check for IMatchLookup"}
  *match-lookup* false)

(def ^{:dynamic true
       :doc "Default vector type. Can be rebound allowing emission of
             custom inline code for vector patterns, for example
             type-hinted primitive array operations"}
  *vector-type* ::vector)

(def ^{:dynamic true
       :doc "In the presence of recur we cannot apply code size optimizations"}
  *recur-present* false)

(def ^{:dynamic true
       :doc "Flag to optimize performance over code size."}
  *no-backtrack* false)

(def ^{:doc "Pre-allocated exception used for backtracing"}
  backtrack (Exception. "Could not find match."))

(defn backtrack-expr []
  (if *clojurescript*
    `(throw cljs.core.match/backtrack)
    `(throw clojure.core.match/backtrack)))

(defn backtrack-sym []
  (if *clojurescript*
    'cljs.core.match/backtrack
    'clojure.core.match/backtrack))

(def ^{:dynamic true} *backtrack-stack* ())
(def ^{:dynamic true} *root* true)

(defn warn [msg]
  (when (not @*warned*)
    (binding [*out* *err*] 
      (println "WARNING:"
        (str *ns* ", line " *line* ":") 
        msg))
    (reset! *warned* true)))

(defn analyze [form env]
  (binding [ana/macroexpand-1 ana-jvm/macroexpand-1
            ana/create-var    ana-jvm/create-var
            ana/parse         ana-jvm/parse
            ana/var?          var?]
    (ana/analyze form env)))

(defn get-loop-locals []
  (let [LOOP_LOCALS clojure.lang.Compiler/LOOP_LOCALS]
    (mapcat
      (fn [b]
        (let [name (.sym ^clojure.lang.Compiler$LocalBinding b)]
          [name name]))
      (when (bound? LOOP_LOCALS)
        @LOOP_LOCALS))))

;; =============================================================================
;; # Map Pattern Interop

(extend-type clojure.lang.ILookup
  IMatchLookup
  (val-at [this k not-found]
    (.valAt this k not-found)))

(defn val-at*
  ([m k] (val-at m k ::not-found))
  ([m k not-found] (val-at m k not-found)))

(defn val-at-expr [& args]
  (if *clojurescript*
    `(get ~@args)
    ;;If not ClojureScript, defer to val-at*
    `(if (instance? clojure.lang.ILookup ~(first args))
       (get ~@args)
       (val-at* ~@args))))

;; =============================================================================
;; # Vector Pattern Interop
;;
;; Vectors patterns can generate code specialized on type. This is
;; useful for generating optimal code for data like primitive arrays
;; and bytes. Defaults for vector are provided, see
;; clojure.core.match.array and clojure.core.match.bits for
;; experiments involving these types.

(defn vector-type [t & r] t)

(defmulti check-size? identity)
(defmulti tag (fn [t] t))
(defmulti test-inline vector-type)
(defmulti test-with-size-inline vector-type)
(defmulti test-with-min-size-inline vector-type)
(defmulti count-inline vector-type)
(defmulti nth-inline vector-type)
(defmulti nth-offset-inline vector-type)
(defmulti subvec-inline vector-type)
(defmulti nthnext-inline vector-type)

(defmethod check-size? :default
  [_] true)

(defmethod tag :default
  [t] (throw (Exception. (str "No tag specified for vector specialization " t))))

(defmethod tag ::vector
  [_] clojure.lang.IPersistentVector)

(defn with-tag [t ocr]
  (let [the-tag (tag t)
        the-tag (if (and (class? the-tag)
                         (.isArray ^Class the-tag))
                  (.getName ^Class the-tag)
                  the-tag)]
    (vary-meta ocr assoc :tag the-tag)))

(defmethod test-inline ::vector
  [t ocr]
  (let [the-tag (tag t)
        c (cond
            (class? the-tag) the-tag
            (string? the-tag) (Class/forName the-tag)
            (symbol? the-tag) (Class/forName (str the-tag))
            :else (throw (Error. (str "Unsupported tag type" the-tag))))]
    (cond
      (= t ::vector) `(vector? ~ocr)
      (and (.isArray ^Class c) *clojurescript*) `(cljs.core/array? ~ocr)
      :else `(instance? ~c ~ocr))))

(defmethod test-with-size-inline ::vector
  [t ocr size]
  `(and ~(test-inline t ocr)
         (== ~(count-inline t (with-tag t ocr)) ~size)))

(defmethod test-with-min-size-inline ::vector
  [t ocr size]
  `(and ~(test-inline t ocr)
         (>= ~(count-inline t (with-tag t ocr)) ~size)))

(defmethod count-inline ::vector
  [_ ocr] `(count ~ocr))

(defmethod nth-inline ::vector
  [_ ocr i] `(nth ~ocr ~i))

(defmethod nth-offset-inline ::vector
  [t ocr i offset]
  (nth-inline t ocr i))

(defmethod subvec-inline ::vector
  ([_ ocr start] `(subvec ~ocr ~start))
  ([_ ocr start end] `(subvec ~ocr ~start ~end)))

(defmethod nthnext-inline ::vector
  ([_ ocr n] `(nthnext ~ocr ~n)))

;; =============================================================================
;; # Extensions

;; Pattern matrices are represented with persistent
;; vectors. Operations on pattern matrices require us to move
;; something from the middle of the vector to the front - thus prepend
;; and drop-nth. swap will swap the 0th element with the nth element.

(extend-type clojure.lang.IPersistentVector
  IVecMod
  (prepend [this x]
    (into [x] this))
  (drop-nth [this n]
    (into (subvec this 0 n)
          (subvec this (clojure.core/inc n) (count this))))
  (swap [this n]
    (let [x (nth this n)]
      (prepend (drop-nth this n) x))))

;; -----------------------------------------------------------------------------
;; constructor?

(declare wildcard-pattern?)

(defn constructor? [p]
  (not (wildcard-pattern? p)))

;; =============================================================================
;; # Pattern Grouping
;;
;; Used to determine the groupable constructors in a column

(defmulti groupable?
  "Determine if two patterns may be grouped together for simultaneous
   testing."
  (fn [a b] [(::tag a) (::tag b)]))

(defmethod groupable? :default
  [a b] (= a b))

;; =============================================================================
;; # Pattern Rows
;;
;; Pattern rows are one line of a matrix. They correspond to one
;; clause in the in the user's original pattern. patterns, action,
;; bindings are accessors.
;; 

(declare leaf-bind-expr named-wildcard-pattern?)

(deftype PatternRow [ps action bindings]
  Object
  (equals [_ other]
    (and (instance? PatternRow other)
         (= ps (:ps other))
         (= action (:action other))
         (= bindings (:bindings other))))
  
  IVecMod
  (drop-nth [_ n]
    (PatternRow. (drop-nth ps n) action bindings))

  (prepend [_ x]
    (PatternRow. (into [x] ps) action bindings))

  (swap [_ n]
    (PatternRow. (swap ps n) action bindings))

  clojure.lang.Associative
  (assoc [this k v]
    (PatternRow. (assoc ps k v) action bindings))

  clojure.lang.Indexed
  (nth [_ i]
    (nth ps i))
  (nth [_ i x]
    (nth ps i x))

  clojure.lang.ISeq
  (first [_] (first ps))

  (next [_]
    (if-let [nps (next ps)]
      (PatternRow. nps action bindings)
      (PatternRow. [] action bindings)))

  (more [_]
    (if (empty? ps)
      nil
      (let [nps (rest ps)]
        (PatternRow. nps action bindings))))

  (seq [this]
    (seq ps))

  (count [_]
    (count ps))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :ps ps
      :action action
      :bindings bindings
      not-found))

  clojure.lang.IFn
  (invoke [_ n]
    (nth ps n))

  clojure.lang.IPersistentCollection
  (cons [_ x]
    (PatternRow. (conj ps x) action bindings))
  (equiv [this other]
    (.equals this other)))

(defn pattern-row
  ([ps action] 
    (pattern-row ps action []))
  ([ps action bindings]
    (let [ps (if (vector? ps) ps (into [] ps))]
      (PatternRow. ps action bindings))))

;; NOTE: we don't use map destructuring here because PatternRow is
;; both ISeq and ILookup, but in map destructuring seq? is tested
;; first - David

(defn update-pattern [prow i p]
  (pattern-row (assoc (:ps prow) i p) (:action prow) (:bindings prow)))

(defn all-wildcards? [prow]
  (every? wildcard-pattern? (:ps prow)))

(defn drop-nth-bind [prow n ocr]
  (let [ps        (:ps prow)
        p         (ps n)
        action    (:action prow)
        bind-expr (leaf-bind-expr ocr)
        as        (-> p meta :as)
        bindings  (or (:bindings prow) [])
        bindings  (if as
                    (conj bindings [as bind-expr])
                    bindings)
        bindings  (if (named-wildcard-pattern? p)
                    (conj bindings [(:sym p) bind-expr])
                    bindings)]
    (pattern-row (drop-nth ps n) action bindings)))

;; =============================================================================
;; # Compilation Nodes

;; -----------------------------------------------------------------------------
;; ## Leaf Node

(defrecord LeafNode [value bindings]
  INodeCompile
  (n-to-clj [this]
    (if (not (empty? bindings))
      (let [bindings (remove (fn [[sym _]] (= sym '_))
                             bindings)]
       `(let [~@(apply concat bindings)]
          ~value))
      value)))

;; TODO precondition on bindings? see above - Ambrose
(defn leaf-node
  ([value] (LeafNode. value []))
  ([value bindings] (LeafNode. value bindings))) 

(defmulti leaf-bind-expr (fn [ocr] (-> ocr meta :occurrence-type)))

(defmethod leaf-bind-expr :seq
  [ocr] (-> ocr meta :bind-expr))

(defmethod leaf-bind-expr ::vector
  [ocr] (-> ocr meta :bind-expr))

(defmethod leaf-bind-expr :map
  [ocr] (let [m (meta ocr)]
          (val-at-expr (:map-sym m) (:key m))))

(defmethod leaf-bind-expr :default
  [ocr] ocr)

;; -----------------------------------------------------------------------------
;; ## Fail Node

(defrecord FailNode []
  INodeCompile
  (n-to-clj [this]
    (if *recur-present*
      `(throw
         ~(if *clojurescript*
            `(js/Error. (str "No match found."))
            `(Exception. (str "No match found."))))
      (backtrack-expr))))

(defn fail-node []
  (FailNode.))

;; -----------------------------------------------------------------------------
;; ## Bind Node

(defrecord BindNode [bindings node]
  INodeCompile
  (n-to-clj [this]
    `(let [~@bindings]
       ~(n-to-clj node))))

(defn bind-node [bindings node]
  (BindNode. bindings node))

;; -----------------------------------------------------------------------------
;; ## Switch Node

(declare to-source)

(defn dag-clause-to-clj [occurrence pattern action]
  (let [test (if (instance? clojure.core.match.protocols.IPatternCompile pattern)
               (to-source* pattern occurrence) 
               (to-source pattern occurrence))]
    [test (n-to-clj action)]))

(defn catch-error [& body]
  (let [err-sym (if *clojurescript* 'js/Error 'Exception)]
    `(catch ~err-sym e#
       (if (identical? e# ~(backtrack-sym))
         (do
           ~@body)
         (throw e#)))))

(defrecord SwitchNode [occurrence cases default]
  INodeCompile
  (n-to-clj [this]
    (let [clauses (doall
                    (mapcat (partial apply dag-clause-to-clj occurrence)
                      cases))
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr
          (if *recur-present*
            (doall
              (concat
                `(cond ~@clauses)
                `(:else ~(n-to-clj default))))
            (doall
              (concat
                `(cond ~@clauses)
                `(:else
                   ~(backtrack-expr)))))]
      (if *recur-present*
        (if bind-expr
          `~(doall
              (concat
                `(let [~occurrence ~bind-expr])
                (list cond-expr)))
          `~cond-expr)
        (if bind-expr
          `(try
             ~(doall
                (concat
                  `(let [~occurrence ~bind-expr])
                  (list cond-expr)))
             ~(catch-error (n-to-clj default)))
          `(try
             ~cond-expr
             ~(catch-error (n-to-clj default))))))))

(defn switch-node
  ([occurrence cases default]
   {:pre [(sequential? cases)]}
   (SwitchNode. occurrence cases default)))

;; =============================================================================
;; # Pattern Matrix

(defn first-column? [i] (zero? i))

(defn empty-row? [row]
  (let [ps (:ps row)]
    (and (not (nil? ps))
         (empty? ps))))

(defn score-column [i col]
  [i (reduce + 0 col)])

(defn width [{rows :rows}]
  (if (not (empty? rows))
    (count (rows 0))
    0))

(defn height [{rows :rows}]
  (count rows))

(defn dim [pm]
  [(width pm) (height pm)])

(defn empty-matrix? [pm]
  (= (dim pm) [0 0]))

(defn column [{rows :rows} i]
  (vec (map #(nth % i) rows)))

(defn row [{rows :rows} j]
  (nth rows j))

(defn rows [{rows :rows}] rows)

(defn pattern-at [{rows :rows} i j]
  ((rows j) i))

(defn action-for-row [{rows :rows} j]
  (:action (rows j)))

(defn occurrences [pm] (:ocrs pm))

;; Returns bindings usable by leaf-node
(defn row-bindings [f ocrs]
  (concat (:bindings f)
    (->> (map vector (:ps f) ocrs)
      (filter (fn [[p ocr]] (named-wildcard-pattern? p)))
      (map (fn [[p ocr]] [(:sym p) (leaf-bind-expr ocr)])))))

(defn existential-pattern? [x]
  (instance? IExistentialPattern x))

(defn wildcard-or-existential? [x]
  (or (wildcard-pattern? x)
      (existential-pattern? x)))

(defn constructors-above? [pm i j]
  (every?
    (comp not wildcard-or-existential?)
    (take j (column pm i))))

;; based on paper we used to check the following
;; (wildcard-pattern? p) (not (useful? (drop-nth pm i) j))
;; IMPORTANT NOTE: this calculation is very very slow,
;; we should look at this more closely - David

(defn pattern-score [pm i j]
  (let [p (pattern-at pm i j)]
    (cond
      (or (wildcard-pattern? p)
          (not (constructors-above? pm i j))) 0
      (existential-pattern? p) 1
      :else 2)))

;; DEAD CODE for now - David
;; (defn useful? [pm j]
;;   (some #(useful-p? pm % j)
;;         (range (count (row pm j)))))

(defn useful-matrix [pm]
  (->> (for [j (range (height pm))
             i (range (width pm))]
         (pattern-score pm i j))
    (partition (width pm))
    (map vec)
    vec))

(defn necessary-column [pm]
  (->> (apply map vector (useful-matrix pm))
    (map-indexed score-column)
    (reduce
      (fn [[col score :as curr]
           [ocol oscore :as cand]]
        (if (> oscore score) cand curr))
      [0 0])
    first))

(defn select [pm]
  (swap pm (necessary-column pm)))

(declare default-specialize-matrix)

(defn specialize
  ([matrix]
    (specialize matrix (ffirst (rows matrix))))
  ([matrix p]
    (if (satisfies? ISpecializeMatrix p)
      (specialize-matrix p matrix)
      (default-specialize-matrix p matrix))))

(defn pseudo-pattern? [x]
  (instance? IPseudoPattern x))

(defn pseudo-patterns [matrix i]
  (filter pseudo-pattern? (column matrix i)))

(defn column-splitter [col]
  (let [f (first col)
        [top bottom] (split-with #(groupable? f %) (rest col))]
    [(cons f top) bottom]))

(declare pattern-matrix compile)

(defn return-split [S D]
  (if *recur-present*
    (if (and (empty-matrix? D) (seq *backtrack-stack*))
      [S (peek *backtrack-stack*) *backtrack-stack*]
      [S D (conj *backtrack-stack* D)])
    [S D]))

(defn matrix-splitter [matrix]
  (let [rows (rows matrix)
        n    (count (first (column-splitter (map first rows))))
        ocrs (occurrences matrix)
        S    (pattern-matrix (take n rows) ocrs)
        D    (pattern-matrix (drop n rows) ocrs)]
    (return-split S D)))

(defn group-rows [cs rows]
  (reduce
    (fn [res row]
      (let [[c rows] (peek res)
             c'      (first row)]
        (if (groupable? c c')
          (conj (pop res) [c (conj rows row)])
          (conj res [c' [row]]))))
    [[(first cs) [(first rows)]]] (rest rows)))

(declare literal-pattern?)

(defn non-local-literal-pattern? [p]
  (and (literal-pattern? p)
       (not (-> p :l meta :local))))

(defn literal-case-matrix-splitter [matrix]
  (let [ocrs  (occurrences matrix)
        rows  (rows matrix)
        lrows (loop [rows (seq rows) res [] lits #{}]
                ;; a bit hacky but lit patterns hash differently we
                ;; store the literal value directly in lits set
                (if rows
                  (let [[p :as row] (first rows)]
                    (if (and (non-local-literal-pattern? p)
                             (not (contains? lits (:l p))))
                      (recur (next rows) (conj res row)
                        (if (non-local-literal-pattern? p)
                          (conj lits (:l p))
                          lits))
                      res))
                  res))
        S     (->> lrows
                (group-rows (map first lrows))
                (map (fn [[c rows]]
                       [c (pattern-matrix rows ocrs)]))
                vec)
        D     (pattern-matrix (drop (count lrows) rows) ocrs)]
    (return-split S D)))

(defn default-case [matrix]
  (if-not (empty-matrix? matrix)
    (compile matrix)
    (fail-node)))

(defn cases [matrix]
  (if (vector? matrix)
    ;; grouped literal case
    (->> matrix
      (map (fn [[c m]]
             [c (-> m (specialize c) compile)]))
      vec)
    ;; normal case
    (let [rows (rows matrix)
          c    (ffirst rows)]
      [[c (-> matrix (specialize c) compile)]])))

(defn expression? [ocr]
  (contains? (meta ocr) :ocr-expr))

(defn bind-variables [ocrs] 
  (mapcat
    (fn [ocr]
      (let [bind-expr (get (meta ocr) :ocr-expr ::not-found)]
        (if (not= bind-expr ::not-found)
          [ocr bind-expr]
          [ocr ocr])))
    ocrs))

(defn root-bind-node [matrix]
  (let [ocrs (occurrences matrix)
        node (compile matrix)]
    (if (some expression? ocrs)
      (bind-node (bind-variables ocrs) node)
      node)))

;; -----------------------------------------------------------------------------
;; # Compilation Cases
;;
;; These are analogous to Maranget's Compilation Scheme on page 4,
;; respectively case 1, 2, 2 (also), 3a and 3b.
;;

(defn empty-rows-case 
  "Case 1: If there are no pattern rows to match, then matching always fails"
  []
  (fail-node))

(defn first-row-empty-case 
  "Case 2: If the first row is empty then matching always succeeds 
  and yields the first action."
  [rows ocr]
  (let [f (first rows)
        a (:action f)
        bs (:bindings f)]
    ;; FIXME: the first row is an infinite list of nil - David
    (leaf-node a bs)))

(defn first-row-wildcards-case 
  "Case 2: If the first row is constituted by wildcards then matching
  matching always succeeds and yields the first action."
  [rows ocrs]
  (let [f (first rows)
        a (:action f)
        bs (row-bindings f ocrs)]
    (leaf-node a bs)))

;; if the first pattern in the first column is a
;; pseudo-pattern, expand until it isn't, looking at
;; any rows beyond the first causes problems for
;; fn application pattern
;; TODO: col is always ZERO - this is confusing
;; that it takes col as an argument, fix - David

(defn expand-matrix [matrix col]
  (loop [matrix matrix]
    (let [p (first (column matrix col))]
      (if (pseudo-pattern? p)
        (recur (specialize matrix p))
        matrix))))

(defn split-matrix [matrix]
  (matrix-splitter matrix)
  #_(if (non-local-literal-pattern? (ffirst (rows matrix)))
    ;; literal testing based on equality can do w/o
    ;; backtracking for all adjacent literal ctors in a column
    (literal-case-matrix-splitter matrix)
    (matrix-splitter matrix))
  )

(defn first-column-chosen-case 
  "Case 3a: The first column is chosen. Compute and return a
  switch/bind node with a default matrix case"
  [matrix col ocrs]
  (let [expanded        (expand-matrix matrix col)
        ocrs            (occurrences expanded)
        [S D :as split] (split-matrix expanded)]
    (if-not *recur-present*
      (switch-node (ocrs col)
        (cases S)
        (default-case D))
      (let [new-stack (last split)]
        (switch-node (ocrs col)
          (if-not (identical? *backtrack-stack* new-stack)
            (binding [*backtrack-stack* new-stack]
              (cases S))
            (cases S))
          (if (and (seq *backtrack-stack*)
                   (identical? (peek *backtrack-stack*) D))
            (binding [*backtrack-stack* (pop *backtrack-stack*)]
              (default-case D))
            (default-case D)))))))

(defn other-column-chosen-case 
  "Case 3b: A column other than the first is chosen. Swap column 
col with the first column and compile the result"
  [matrix col]
  (compile (swap matrix col)))

;; Return a column number of a column which contains at least
;; one non-wildcard constructor
(defn choose-column [matrix]
  (necessary-column matrix))

(defn compile [{:keys [rows ocrs] :as pm}]
  (cond
    *root*
    (binding [*root* false]
      (root-bind-node pm))

    (empty? rows)
    (empty-rows-case)

    (empty-row? (first rows))
    (first-row-empty-case rows (first ocrs))

    (all-wildcards? (first rows))
    (first-row-wildcards-case rows ocrs)

    :else
    (let [col (choose-column pm)]
      (if (first-column? col)
        (first-column-chosen-case pm col ocrs)
        (other-column-chosen-case pm col)))))

(defrecord PatternMatrix [rows ocrs]
  IVecMod
  (drop-nth [_ i]
    (let [nrows (vec (map #(drop-nth % i) rows))]
      (PatternMatrix. nrows ocrs)))

  ;; Swap column number idx with the first column
  (swap [_ idx]
    (let [nrows (vec (map #(swap % idx) rows))]
      (PatternMatrix. nrows (swap ocrs idx)))))

(defn pattern-matrix [rows ocrs]  
  (let [rows (if (vector? rows) rows (into [] rows))
        ocrs (if (vector? ocrs) ocrs (into [] ocrs))]
    (PatternMatrix. rows ocrs)))

;; =============================================================================
;; ## Default Matrix Specialization

;; NOTE: not sure why we need groupable? here for this to work - David

(defn default-specialize-matrix [p matrix]
  (let [rows (rows matrix)
        ocrs (occurrences matrix)
        focr (first ocrs)
        nrows (->> rows
                (map #(drop-nth-bind % 0 focr))
                vec)
        nocrs (drop-nth ocrs 0)]
    (pattern-matrix nrows nocrs)))

;; =============================================================================
;; # Patterns
;;

;; -----------------------------------------------------------------------------
;; ## Wildcard Pattern
;; 
;; A wildcard pattern accepts any value.
;;
;; In practice, the DAG compilation eliminates any wildcard patterns.

(deftype WildcardPattern [sym named _meta]
  Object
  (equals [_ other]
    (and (instance? WildcardPattern other)
         (if named
           (= sym (:sym other))
           (not (:named other)))))

  clojure.lang.IObj
  (withMeta [_ new-meta]
    (WildcardPattern. sym named new-meta))
  (meta [_]
    _meta)

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :sym sym
      :named named
      not-found)))

(defn wildcard-pattern
  ([] (wildcard-pattern '_))
  ([sym] 
    {:pre [(symbol? sym)]}
    (if (= sym '_)
      (WildcardPattern. (gensym) false nil)
      (WildcardPattern. sym true nil))))

(defn wildcard-pattern? [x]
  (instance? WildcardPattern x))

;; Local bindings in pattern matching are emulated by using named wildcards.
;; See clojure.lang.Symbol dispatch for `emit-pattern` 

(defn named-wildcard-pattern? [x]
  (and (instance? WildcardPattern x) (:named x)))

(defmethod print-method WildcardPattern [p ^Writer writer]
  (.write writer (str "<WildcardPattern: " (:sym p) ">")))

;; -----------------------------------------------------------------------------
;; ## Literal Pattern
;;
;; A literal pattern is not further split into further patterns in the DAG
;; compilation phase.
;;
;; It "literally" matches a given occurrence.

(deftype LiteralPattern [l _meta]
  Object
  (toString [_]
    (if (nil? l)
      "nil"
      (pr-str l)))
  (equals [_ other]
    (and (instance? LiteralPattern other) (= l (:l other))))

  clojure.lang.IObj
  (meta [_] _meta)

  (withMeta [_ new-meta]
    (LiteralPattern. l new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :l l
      ::tag ::literal
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (cond
     (= l ())
     `(empty? ~ocr)

     (and (symbol? l) (not (-> l meta :local)))
     `(= ~ocr '~l)

     (and *clojurescript*
         (or (number? l) (string? l)
             (true? l) (false? l)
             (nil? l)))
     `(identical? ~ocr ~l) 
      
     (and *clojurescript* (keyword? l))
     `(cljs.core/keyword-identical? ~ocr ~l)
     
      :else `(= ~ocr ~l))))

(defn literal-pattern [l] 
  (LiteralPattern. l (meta l)))

(defn literal-pattern? [x]
  (instance? LiteralPattern x))

(defmethod print-method LiteralPattern [p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; # Seq Pattern
;;
;; A Seq Pattern is intended for matching `seq`s.  They are split into
;; multiple patterns, testing each element of the seq in order.

(declare seq-pattern? rest-pattern? seq-pattern)

(defn specialize-seq-pattern-rest-row [focr row]
  (let [p (first row)
        p (if (seq-pattern? p)
            (:p (first (:s p))) ;; unwrap rest pattern
            (wildcard-pattern))]
    (prepend (drop-nth-bind row 0 focr) p)))

(defn specialize-seq-pattern-rest-matrix [rows focr]
  (->> rows
    (map (partial specialize-seq-pattern-rest-row focr))
    vec))

(defn seq-pattern-matrix-rest-ocrs [ocrs focr] ocrs)

(defn specialize-seq-pattern-row [focr row]
  (let [p (first row)
        [h t] (if (seq-pattern? p)
                (let [[h & t] (:s p)
                      t (cond
                          (empty? t) (literal-pattern ())
                          (rest-pattern? (first t)) (:p (first t))
                          :else (seq-pattern t))]
                  [h t])
                [(wildcard-pattern) (wildcard-pattern)])]
    (reduce prepend (drop-nth-bind row 0 focr) [t h])))

(defn specialize-seq-pattern-matrix [rows focr]
  (->> rows
    (map (partial specialize-seq-pattern-row focr))
    vec))

(defn seq-pattern-matrix-ocrs [ocrs focr]
  (let [seq-sym (or (-> focr meta :seq-sym) focr)
        sym-meta {:occurrence-type :seq
                  :seq-sym focr}
        hsym (gensym (str (name seq-sym) "_head__"))
        hsym (with-meta hsym
               (assoc sym-meta :bind-expr `(first ~focr)))
        tsym (gensym (str (name seq-sym) "_tail__"))
        tsym (with-meta tsym
               (assoc sym-meta :bind-expr `(rest ~focr)))]
    (into [hsym tsym] (drop-nth ocrs 0))))

(deftype SeqPattern [s _meta]
  Object
  (toString [_]
    (str s))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (SeqPattern. s new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :s s
      ::tag ::seq
      not-found))
  
  IPatternCompile
  (to-source* [this ocr]
    (if (and (>= (count s) 1)
             (not (rest-pattern? (first s))))
      `(and (or (seq? ~ocr) (sequential? ~ocr)) (seq ~ocr))
      `(or (seq? ~ocr) (sequential? ~ocr))))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          focr  (first ocrs)]
      (if-not (rest-pattern? (first s))
        (let [nrows (specialize-seq-pattern-matrix rows focr)
              nocrs (seq-pattern-matrix-ocrs ocrs focr)]
          (pattern-matrix nrows nocrs))
        (let [nrows (specialize-seq-pattern-rest-matrix rows focr)
              nocrs (seq-pattern-matrix-rest-ocrs ocrs focr)]
          (pattern-matrix nrows nocrs))))))

(defn ^SeqPattern seq-pattern [s]
  {:pre [(sequential? s)
         (not (empty? s))]}
  (SeqPattern. s nil))

(defn seq-pattern? [x]
  (instance? SeqPattern x))

(defmethod print-method SeqPattern [p ^Writer writer]
  (.write writer (str "<SeqPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; # Rest Pattern
;; 
;; A rest pattern represents the case of matching [2 3] in [1 & [2 3]]
;; It is an implementation detail of other patterns, like SeqPattern.
;;

(defrecord RestPattern [p])

(defn rest-pattern [p]
  (assoc (RestPattern. p) ::tag ::rest))

(defn rest-pattern? [x]
  (instance? RestPattern x))

(defmethod print-method RestPattern [p ^Writer writer]
  (.write writer (str "<RestPattern: " (:p p) ">")))

;; -----------------------------------------------------------------------------
;; # Map Pattern
;; 
;; Map patterns match maps, or any object that satisfies IMatchLookup.
;;

(defn specialize-map-key-pattern-matrix [rows]
  (let [p (:p (ffirst rows))]
    (->> rows
      (map #(drop-nth % 0))
      (map #(prepend % p))
      vec)))

(defrecord MapKeyPattern [p]
  IExistentialPattern

  IPatternCompile
  (to-source* [this ocr]
    `(not= ~ocr ::not-found))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          nrows (specialize-map-key-pattern-matrix rows)]
      (pattern-matrix nrows ocrs))))

(defn map-key-pattern [p]
  (assoc (MapKeyPattern. p) ::tag ::map-key))

(defn map-key-pattern? [x]
  (instance? MapKeyPattern x))

(defmethod print-method MapKeyPattern [p ^Writer writer]
  (.write writer (str "<MapKeyPattern: " (:p p) ">")))

(declare map-pattern? guard-pattern)

(defn row-keys [row env]
  (let [p    (first row)
        only (-> p meta :only)]
    (when (and (not @(:only? env))
               (seq only))
      (reset! (:only? env) true))
    [(set (keys (:m p)))
     (set only)]))

(defn get-all-keys [rows env]
  (->> rows
    (remove (comp wildcard-pattern? first))
    (map #(row-keys % env))
    (reduce concat)
    (reduce set/union #{})))

(defn wrap-values [m]
  (->> m
    (map (fn [[k v]]
           [k (if (wildcard-pattern? v)
                (map-key-pattern v) v)]))
    (into {})))

(defn get-ocr-map
  [p {:keys [only all-keys wc-map]}]
  (if (map-pattern? p)
    (merge
      (when only
        (zipmap all-keys
          (repeat (literal-pattern ::not-found))))
      wc-map (wrap-values (:m p)))
    wc-map))

(defn specialize-map-pattern-row
  [row {:keys [all-keys only? focr] :as env}]
  (let [p       (first row)
        only    (seq (-> p meta :only))
        ocr-map (get-ocr-map p (assoc env :only only))
        ps      (doall (map ocr-map all-keys))
        ps      (if @only?
                  (if only
                    (let [a (with-meta (gensym) {:tag 'java.util.Map})]
                      (cons
                        (guard-pattern (wildcard-pattern)
                          (set [(if *clojurescript*
                                  `(fn [~a] (= (set (keys ~a)) #{~@only}))
                                  `(fn [~a] (= (.keySet ~a) #{~@only})))]))
                        ps))
                    (cons (wildcard-pattern) ps))
                  ps)]
    (reduce prepend (drop-nth-bind row 0 focr) (reverse ps))))

(defn specialize-map-pattern-matrix [rows env]
  (vec (map #(specialize-map-pattern-row % env) rows)))

(defn gen-map-pattern-ocr [ocr k]
  (gensym (str (name ocr) "_" (.replace (name k) "." "_DOT_") "__")))

(defn map-pattern-matrix-ocr-sym [k env]
  (let [focr (:focr env)
        ocr  (get-in env [:ocrs-map k])]
    (with-meta ocr
      {:occurrence-type :map
       :key k
       :map-sym focr
       :bind-expr (val-at-expr focr k ::not-found)})))

(defn map-pattern-matrix-ocrs [ocrs env]
  (let [focr  (:focr env)
        mocrs (map #(map-pattern-matrix-ocr-sym % env)
                (:all-keys env))
        mocrs (vec
                (if @(:only? env)
                  (cons focr mocrs)
                  mocrs))]
    (into mocrs (drop-nth ocrs 0))))

(deftype MapPattern [m _meta]
  Object
  (toString [_]
    (str m " :only " (or (:only _meta) [])))
  (equals [_ other]
    (and (instance? MapPattern other) (= m (:m other))))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (MapPattern. m new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :m m
      ::tag ::map
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (cond
      *clojurescript* `(satisfies? cljs.core/ILookup ~ocr)
      *match-lookup*  `(or (instance? clojure.lang.ILookup ~ocr) (satisfies? IMatchLookup ~ocr))
      :else `(instance? clojure.lang.ILookup ~ocr)))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows     (rows matrix)
          ocrs     (occurrences matrix)
          focr     (first ocrs)
          env      {:focr focr
                    :only? (atom false)}
          all-keys (get-all-keys rows env)
          env'     (assoc env
                     :all-keys all-keys
                     :wc-map (zipmap all-keys (repeatedly wildcard-pattern))
                     :ocrs-map (zipmap all-keys
                                 (map #(gen-map-pattern-ocr focr %)
                                   all-keys)))
          nrows    (specialize-map-pattern-matrix rows env')
          nocrs    (map-pattern-matrix-ocrs ocrs env')]
      (pattern-matrix nrows nocrs))))

(defn map-pattern
  ([] (MapPattern. {} nil))
  ([m] {:pre [(map? m)]}
     (MapPattern. m nil)))

(defn map-pattern? [x]
  (instance? MapPattern x))

(defmethod print-method MapPattern [p ^Writer writer]
  (.write writer (str "<MapPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; Vector Pattern
;;
;; Vector patterns match any Sequential data structure. Note this means that
;; the lazy semantics may mean poorer performance for sequences.

(declare vector-pattern?)

(defn calc-rest?-and-min-size [rows env]
  (reduce
    (fn [[rest? min-size] [p & ps]]
      (if (vector-pattern? p)
        [(or rest? (:rest? p))
         (min min-size (:size p))]
        [rest? min-size]))
    [false (-> env :fp :size)] rows))

(defn specialize-vector-pattern-row
  [row {:keys [focr min-size]}]
  (let [p  (first row)
        ps (cond
             (vector-pattern? p) (split p min-size)
             :else [(wildcard-pattern) (wildcard-pattern)])]
    (reduce prepend (drop-nth-bind row 0 focr) (reverse ps))))

(defn specialize-vector-pattern-row-non-rest
  [row {:keys [focr min-size]}]
  (let [p  (first row)
        ps (if (vector-pattern? p)
             (reverse (:v p))
             (repeatedly min-size wildcard-pattern))]
    (reduce prepend (drop-nth-bind row 0 focr) ps)))

(defn specialize-vector-pattern-matrix [rows env]
  (if (:rest? env)
    (vec (map #(specialize-vector-pattern-row % env) rows))
    (vec (map #(specialize-vector-pattern-row-non-rest % env) rows))))

(defn vector-pattern-ocr-sym
  [{:keys [pat focr tag]} i]
  (let [ocr (gensym (str (name focr) "_" i "__"))]
    (with-meta ocr
      {:occurrence-type tag
       :vec-sym focr
       :index i
       :bind-expr
       (if-let [offset (:offset pat)]
         (nth-offset-inline tag (with-tag tag focr) i offset)
         (nth-inline tag (with-tag tag focr) i))})))

(defn vector-pattern-matrix-ocrs
  [ocrs {:keys [focr tag min-size rest?] :as env}]
  (if rest?
    (let [ocr-meta {:occurrence-type tag
                    :vec-sym focr}
          vl-ocr (gensym (str (name focr) "_left__"))
          vl-ocr (with-meta vl-ocr
                   (assoc ocr-meta :bind-expr
                     (subvec-inline tag (with-tag tag focr) 0 min-size )))
          vr-ocr (gensym (str (name focr) "_right__"))
          vr-ocr (with-meta vr-ocr
                   (assoc ocr-meta :bind-expr
                     (subvec-inline tag (with-tag tag focr) min-size)))]
      (into [vl-ocr vr-ocr] (drop-nth ocrs 0)))
    (concat
      (map (partial vector-pattern-ocr-sym env) (range min-size))
      (drop-nth ocrs 0))))

(defn array-tag [x]
  (get '{bytes   ::array
         shorts  ::shorts
         ints    ::ints
         longs   ::longs
         doubles ::doubles
         objects ::objects}
    (-> x meta :tag)))

;; v - the patterns
;; t - the type, for optimizing via specialization
;; size - size of the pattern if known
;; rest? - contains a rest pattern

(deftype VectorPattern [v t size offset rest? _meta]
  Object
  (toString [_]
    (str v " " t))
  (equals [_ other]
    (and (instance? VectorPattern other)
         (= [v t size offset rest?]
            (map #(% other) [:v :t :size :offset :rest?]))))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (VectorPattern. v t size offset rest? new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :v v
      :t t
      :size size
      :offset offset
      :rest? rest?
      ::tag ::vector
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (let [t (or (array-tag ocr) t)]
      (if (check-size? t)
        (if rest?
          (test-with-min-size-inline t ocr size)
          (test-with-size-inline t ocr size))
        (test-inline t ocr))))

  IContainsRestPattern
  (contains-rest-pattern? [_] rest?)

  IVectorPattern
  (split [this n]
    (let [lv (subvec v 0 n)
          rv (subvec v n)
          pl (VectorPattern. lv t n offset false _meta)
          pr (if (rest-pattern? (first rv))
               (:p (first rv))
               (let [rest? (some rest-pattern? rv)
                     rvc (count rv)
                     size (if rest? (dec rvc) rvc)]
                 (VectorPattern. rv t size n rest? _meta)))]
      [pl pr]))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          env  {:focr focr
                :fp   (ffirst rows)
                :pat  this}
          [rest? min-size] (calc-rest?-and-min-size rows env)
          env' (assoc env
                 :rest? rest? :min-size min-size
                 :tag (or (array-tag focr) (:t this)))
          nrows (specialize-vector-pattern-matrix rows env')
          nocrs (vector-pattern-matrix-ocrs ocrs env')]
      (pattern-matrix nrows nocrs))))

(defn vector-pattern
  ([] (vector-pattern [] ::vector nil nil))
  ([v] (vector-pattern v ::vector nil nil))
  ([v t] (vector-pattern v t nil nil))
  ([v t offset] (vector-pattern v t offset nil))
  ([v t offset rest?]
    {:pre [(vector? v)]}
    (let [c    (count v)
          size (if rest? (dec c) c)]
      (VectorPattern. v t size offset rest? nil))))

(defn vector-pattern? [x]
  (instance? VectorPattern x))

(defmethod print-method VectorPattern [p ^Writer writer]
  (.write writer (str "<VectorPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; Or Patterns

(defn specialize-or-pattern-row [row pat ps]
  (let [p (first row)]
    ;; NOTE: hmm why can't we remove this - David
    (if (and (groupable? pat p)
             (not (wildcard-pattern? p)))
      (map (fn [p] (update-pattern row 0 p)) ps) [row])))

(defn specialize-or-pattern-matrix [rows pat ps]
  (vec (apply concat
         (map #(specialize-or-pattern-row % pat ps) rows))))

(deftype OrPattern [ps _meta]
  IPseudoPattern

  Object
  (toString [this]
    (str ps))
  (equals [_ other]
    (and (instance? OrPattern other) (= ps (:ps other))))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (OrPattern. ps new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :ps ps
      ::tag ::or
      not-found))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          nrows (specialize-or-pattern-matrix rows this ps)]
      (pattern-matrix nrows ocrs))))

(defn or-pattern [p]
  {:pre [(vector? p)]}
  (OrPattern. p nil))

(defn or-pattern? [x]
  (instance? OrPattern x))

(defmethod print-method OrPattern [p ^Writer writer]
  (.write writer (str "<OrPattern: " (:ps p) ">")))

;; -----------------------------------------------------------------------------
;; ## Guard Patterns
;;
;; Guard patterns are used to represent guards on patterns, for example
;;   `(1 :guard even?)`
;;

(declare guard-pattern?)

(defn specialize-guard-pattern-matrix [rows]
  (->> rows
    (map (fn [[p :as row]]
           (if (guard-pattern? p)
             (update-pattern row 0 (:p p))
             row)))
    vec))

(deftype GuardPattern [p gs _meta]
  Object
  (toString [this]
    (str p " :guard " gs))
  (equals [_ other]
    (and (instance? GuardPattern other)
         (= p (:p other))
         (= gs (:gs other))))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (GuardPattern. p gs new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :p p
      :gs gs
      ::tag ::guard
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          nrows (specialize-guard-pattern-matrix rows)]
      (pattern-matrix nrows ocrs))))

(defn guard-pattern [p gs]
  {:pre [(set? gs)]}
  (GuardPattern. p gs nil))

(defn guard-pattern? [x]
  (instance? GuardPattern x))

(defmethod print-method GuardPattern [p ^Writer writer]
  (.write writer (str "<GuardPattern " (:p p) " :guard " (:gs p) ">")))

;; -----------------------------------------------------------------------------
;; ## Function Application Pattern
;;
;; Function Application patterns are used to represent function application on
;; occurrences. Pattern matching will continue on the result of the application.
;;    `(3 :<< inc)`

(declare app-pattern?)

;; take the original occurence and replace it with a wildcard in each
;; row that has a compatible application create & new occurrence whose
;; binding is the old occurence with the function applied

(defn app-pattern-matrix-ocrs [[focr :as ocrs] form]
  (into
    [(with-meta
       (gensym (str "app_" focr))
       {:bind-expr `(~form ~(or (-> focr meta :bind-expr) focr))})]
    ocrs))

(defn specialize-app-pattern-matrix [rows form]
  (let [[matched-rows rest-rows]
        (split-with
          (fn [[pat :as row]]
            (and (app-pattern? pat)
                 (= (:form pat) form)))
          rows)]
    (vec
      (concat
        (map
          (fn [row]
            (prepend
              (update-pattern row 0 (wildcard-pattern))
              (:p (first row))))
          matched-rows)
        (map
          (fn [row]
            (prepend row (wildcard-pattern)))
          rest-rows)))))

(deftype AppPattern [p form _meta]
  IPseudoPattern

  Object
  (toString [this]
  (str p " :<< " form))
  (equals [_ other]
    (and (instance? AppPattern other)
         (= p (:p other))
         (= form (:form other))))

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (AppPattern. p form new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :p p
      :form form
      ::tag ::app
      not-found))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          nocrs (app-pattern-matrix-ocrs ocrs form)
          nrows (specialize-app-pattern-matrix rows form)]
      (pattern-matrix nrows nocrs))))

(defn app-pattern [p form]
  (AppPattern. p form nil))

(defn app-pattern? [x]
  (instance? AppPattern x))

(defmethod print-method AppPattern [p ^Writer writer]
  (.write writer (str "<AppPattern " (:p p) " :app " (:form p) ">")))

;; -----------------------------------------------------------------------------
;; ## Predicate Patterns
;;
;; Predicate patterns are used to represent simple guards on patterns,
;; for example
;;   `(1 :when even?)`
;;
;; The predicates in predicate patterns should not overlap.  That is,
;; no two predicates should return the same answer given the same
;; input.  For example, in the unlikely case that there was a function
;; named `four?` and defined as `(defn four? [x] (= 4 x))`, then using
;; `x :when four?` and `y :when even?` in the same match expression
;; will yield an incorrect decision tree.  In cases where overlapping
;; predicates are desired, use guard patterns.
;;

(def preds (atom {}))

(defmacro defpred
  ([name]
     (swap! preds assoc name name))
  ([name f]
     (swap! preds assoc name f)))

(declare predicate-pattern?)

(defn specialize-predicate-pattern-matrix [rows]
  (->> rows
    (map (fn [[p :as row]]
           (if (predicate-pattern? p)
             (update-pattern row 0 (:p p))
             row)))
    vec))

(deftype PredicatePattern [p gs _meta]
  Object
  (toString [this]
    (str p " :when " gs))
  (equals [_ other]
    (and (instance? PredicatePattern other)
         (= p (:p other))
         (= gs (:gs other))))  

  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (PredicatePattern. p gs new-meta))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (case k
      :p p
      :gs gs
      ::tag ::predicate
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))

  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows  (rows matrix)
          ocrs  (occurrences matrix)
          nrows (specialize-predicate-pattern-matrix rows)]
      (pattern-matrix nrows ocrs))))

(defn predicate-pattern [p gs]
  {:pre [(set? gs)]}
  (PredicatePattern. p gs nil))

(defn predicate-pattern? [x]
  (instance? PredicatePattern x))

(defmethod print-method PredicatePattern [p ^Writer writer]
  (.write writer (str "<PredicatePattern " (:p p) " :when " (:gs p) ">")))

;; -----------------------------------------------------------------------------
;; Pattern Comparisons

(defmethod groupable? [::literal ::literal]
  [a b] (= (:l a) (:l b)))

(defmethod groupable? [::guard ::guard]
  [a b] (= (:gs a) (:gs b)))

(defmethod groupable? [::predice ::predicate]
  [a b] (= (:gs a) (:gs b)))

(defmethod groupable? [::map ::map]
  [a b]
  (= (-> a meta :only) (-> b meta :only)))

(defmethod groupable? [::or ::or]
  [a b]
  (let [as (:ps a)
        bs (:ps b)]
    (and (= (count as) (count bs))
         (every? identity (map groupable? as bs)))))

(defmethod groupable? [::vector ::vector]
  [a b]
  (and (= (:rest? a) (:rest? b))
       (= (:size a) (:size b))))

(defmethod groupable? [::app ::app]
  [a b]
  (and (= (:form a) (:form b))))

;; =============================================================================
;; # Interface

(defmulti to-source 
  "Returns a Clojure form that, when executed, is truthy if the
  pattern matches the occurrence. Dispatches on the `type` of the
  pattern. For instance, a literal pattern might return `(= ~(:pattern
  pattern) ~ocr)`, using `=` to test for a match."
  (fn [pattern ocr] (::tag pattern)))

(defmulti emit-pattern 
  "Returns the corresponding pattern for the given syntax. Dispatches
  on the class of its argument. For example, `[(:or 1 2) 2]` is dispatched
  as clojure.lang.IPersistentVector"
  (fn [pattern] (syntax-tag pattern)))

(extend-protocol ISyntaxTag
  clojure.lang.IPersistentVector
  (syntax-tag [_] ::vector)
  clojure.lang.ISeq
  (syntax-tag [_] ::seq)
  clojure.lang.IPersistentMap
  (syntax-tag [_] ::map)
  clojure.lang.Symbol
  (syntax-tag [_] ::symbol)

  Object
  (syntax-tag [_] :default)
  nil
  (syntax-tag [_] :default))

;; ============================================================================
;; # emit-pattern Methods

(defn emit-patterns
  ([ps t] (emit-patterns ps t []))
  ([ps t v]
     (if (empty? ps)
       v
       (let [p (first ps)]
         (cond
          (= p '&)
          (let [p (second ps)
                rp (if (and (vector? p) (= t :seq))
                     (seq-pattern (emit-patterns p t))
                     (emit-pattern p))]
            (recur (nnext ps) t (conj v (rest-pattern rp)))) 

          :else
          (recur (next ps) t (conj v (emit-pattern (first ps)))))))))

(defmethod emit-pattern ::vector
  [pat]
  (let [ps (emit-patterns pat :vector)]
    (vector-pattern ps *vector-type* 0 (some rest-pattern? ps))))

(defmethod emit-pattern ::map
  [pat]
  (map-pattern
    (->> pat
      (map (fn [[k v]]
             [k (emit-pattern v)]))
      (remove nil?)
      (into {}))))

(defmethod emit-pattern ::symbol
  [pat]
  (if (not= (get *locals* pat ::not-found) ::not-found)
    (literal-pattern (with-meta pat (assoc (meta pat) :local true)))
    (wildcard-pattern pat)))

(defmethod emit-pattern :default
  [pat]
  (literal-pattern pat))

(declare emit-pattern-for-syntax or-pattern as-pattern guard-pattern
         predicate-pattern vector-pattern)

(defmethod emit-pattern ::seq
  [pat] (if (and (= (count pat) 2)
                 (= (first pat) 'quote)
                 (or (symbol? (second pat))
                     (keyword? (second pat))))
          (literal-pattern (second pat))
          (emit-pattern-for-syntax pat)))

(defmulti emit-pattern-for-syntax 
  "Handles patterns wrapped in the special list syntax. Dispatches
  on the first or second keyword in the list. For example, the pattern 
  `(:or 1 ...) is dispatches as :or, and `(1 :as a)` is dispatched by :as."
  (fn [[f s]]
    (if (keyword? f)
      [f :default]
      [:default s])))

(defmethod emit-pattern-for-syntax [:or :default]
  [pat] (or-pattern
          (->> (rest pat)
            (map emit-pattern)
            (into []))))

(defmethod emit-pattern-for-syntax [:default :as]
  [[p _ sym]] (with-meta (emit-pattern p) {:as sym}))

(defmethod emit-pattern-for-syntax [:default :<<]
  [[p _ form]] (app-pattern (emit-pattern p) form))

(defmethod emit-pattern-for-syntax [:default :when]
  [[p _ gs]]
  (let [gs (if (not (vector? gs)) [gs] gs)]
    (assert (every? symbol? gs) (str "Invalid predicate expression " gs))
    (assert (every? #(contains? @preds %) gs) (str "Unknown predicate in " gs))
    (predicate-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax [:default :guard]
  [[p _ gs]] (let [gs (if (not (vector? gs)) [gs] gs)]
              (guard-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax [:default :seq]
  [pat]
  (let [p (first pat)]
    (if (empty? p)
      (literal-pattern ())
      (seq-pattern (emit-patterns p :seq)))))

(defmethod emit-pattern-for-syntax [:default ::vector]
  [[p t offset-key offset]]
  (let [ps (emit-patterns p :vector)]
    (vector-pattern ps t offset (some rest-pattern? ps))))

(defmethod emit-pattern-for-syntax [:default :only]
  [[p _ only]] (with-meta (emit-pattern p) {:only only}))

(defmethod emit-pattern-for-syntax :default
  [[_ s :as l]]
  (throw
    (AssertionError.
      (str "Invalid list syntax " s " in " l ". "
        "Valid syntax: "
        (vec
          (remove #(= % :default)
            (keys
              (.getMethodTable
                ^clojure.lang.MultiFn emit-pattern-for-syntax))))))))


(let [void (Object.)
      void? #(identical? void %)
      infix-keyword? #(#{:when :as :guard} %)]
  ;; void is a unique placeholder for nothing -- we can't use nil
  ;; because that's a legal symbol in a pattern row
  (defn regroup-keywords [pattern]
    (cond
      (vector? pattern)
      (first
        (reduce
          (fn [[result p q] r]
            (cond
              (void? p) [result q r]
              (and (not (void? r)) (infix-keyword? q))
              [(conj result (list (regroup-keywords p) q r)) void void]
              :else [(conj result (regroup-keywords p)) q r]))
          [[] void void]
          (conj pattern void void)))

      (seq? pattern)
      (cons (regroup-keywords (first pattern)) (rest pattern))

      :else pattern)))

 (defn group-keywords 
   "Returns a pattern with pattern-keywords (:when and :as) properly
    grouped.  The original pattern may use the 'flattened' syntax.
    For example, a 'flattened' pattern row like [a b :when even?] is
    grouped as [a (b :when even?)]."
  [pattern]
  (if (vector? pattern) (regroup-keywords pattern) pattern))

(defn to-pattern-row
  "Take an unprocessed pattern expression and an action expression and return
   a pattern row of the processed pattern expression plus the action epxression."
  [pat action]
  (let [ps (map emit-pattern (group-keywords pat))]
    (pattern-row ps action)))

(defn wildcards-and-duplicates
  "Returns a vector of two elements: the set of all wildcards and the 
   set of duplicate wildcards.  The underbar _ is excluded from both."
  [patterns]
  (loop [remaining patterns seen #{} dups #{}]
    (if-let [patterns (seq remaining)]
      (let [pat (first patterns)
            pats (rest patterns)]
        (cond
          (or (= pat '_) (= pat '&))
          (recur pats seen dups)

          (symbol? pat)
          (if (contains? seen pat)
            (recur pats seen (conj dups pat))
            (recur pats (conj seen pat) dups))
          
          (vector? pat)
          (recur (concat pats pat) seen dups)

          (map? pat)
          (recur (concat pats (vals pat)) seen dups)

          (seq? pat)
          (cond
            (= (first pat) 'quote)
            (recur pats seen dups)

            (= (first pat) :or)
            (let [wds (map wildcards-and-duplicates
                        (map list (take-nth 2 pat)))
                   mseen (apply set/union (map first wds))]
              (recur pats (set/union seen mseen)
                (apply set/union dups
                  (set/intersection seen mseen)
                  (map second wds))))
            
            (= (second pat) :as)
            (recur (concat pats (take-nth 2 pat)) seen dups)

            :else
            (recur (conj pats (first pat)) seen dups))
          :else
          (recur pats seen dups)))
      [seen dups])))

(defn find-duplicate-wildcards [pattern]
  (second (wildcards-and-duplicates pattern)))

(defn check-pattern [pat vars nvars rownum]
  (let [pat (group-keywords pat)]
    (when (not (vector? pat))
      (throw
        (AssertionError. 
          (str "Pattern row " rownum
            ": Pattern rows must be wrapped in []."
            " Try changing " pat " to [" pat "]." 
            (when (list? pat)
              (str " Note: pattern rows are not patterns."
                " They cannot be wrapped in a :when guard, for example"))))))
    (when (not= (count pat) nvars)
      (throw
        (AssertionError.
          (str "Pattern row " rownum
            ": Pattern row has differing number of patterns. "
            pat " has " (count pat) " pattern/s, expecting "
            nvars " for occurrences " vars))))
    (when-let [duplicates (seq (find-duplicate-wildcards pat))]
      (throw
        (AssertionError.
          (str "Pattern row " rownum
            ": Pattern row reuses wildcards in " pat
            ".  The following wildcards are ambiguous: "
            (apply str (interpose ", " (sort duplicates)))
            ".  There's no guarantee that the matched values will be same."
            "  Rename the occurrences uniquely."))))))

;; This could be scattered around in other functions to be more efficient
;; Turn off *syntax-check* to disable

(defn check-matrix-args [vars clauses]
  (when (symbol? vars)
    (throw
      (AssertionError.
        (str "Occurrences must be in a vector."
          " Try changing " vars " to [" vars "]"))))
  (when (not (vector? vars))
    (throw
      (AssertionError.
        (str "Occurrences must be in a vector. "
          vars " is not a vector"))))
  (let [nvars (count vars)
        cls   (partition 2 clauses)]
    (doseq [[[pat _] rownum] (map vector (butlast cls) (rest (range)))]
      (when (= :else pat)
        (throw
          (AssertionError.
            (str "Pattern row " rownum
              ": :else form only allowed on final pattern row"))))
      (check-pattern pat vars nvars rownum))
    (when-let [[pat _] (last cls)]
      (when-not (= :else pat)
        (check-pattern pat vars nvars (count cls)))))
  (when (odd? (count clauses)) 
    (throw
      (AssertionError.
        (str "Uneven number of Pattern Rows. The last form `"
          (last clauses) "` seems out of place.")))))

(defn process-vars
  "Process the vars for the pattern matrix. If user provides an
   expression, create a var and annotate via metadata with the
   original expression."
  [vars]
  (letfn [(process-var [var]
            (if-not (symbol? var)
              (let [nsym (gensym "ocr-")]
                (with-meta nsym {:ocr-expr var}))
              var))]
    (vec (map process-var vars))))

(defn emit-matrix
  "Take the list of vars and sequence of unprocessed clauses and
   return the pattern matrix. The pattern matrix contains the processed
   pattern rows and the list of vars originally specified. Inserts
   a last match - :else if provided by the user or a default match that
   throws."
  ([vars clauses]
    (emit-matrix vars clauses true))
  ([vars clauses default]
    (let [cs (partition 2 clauses)
          vs (process-vars vars)
          cs (let [[p a] (last cs)
                   last-match (vec (repeat (count vars) '_))]
               (if (= :else p)
                 (conj (vec (butlast cs)) [last-match a])
                 ;; TODO: throw an exception if :else line not provided - David
                 (if default
                   (conj (vec cs)
                     [last-match
                       (if *clojurescript*
                         `(throw
                            (js/Error.
                              (str "No matching clause: " ~@(interpose " " vs))))
                         `(throw
                            (IllegalArgumentException.
                              (str "No matching clause: " ~@(interpose " " vs)))))])
                   cs)))]
      (pattern-matrix
        (vec (map #(apply to-pattern-row %) cs))
        (process-vars vs)))))

(defn executable-form [node]
  (n-to-clj node))

;; TODO: more sophisticated analysis that actually checks that recur is
;; not being used as a local binding when it occurs - David

(defn recur-present? [actions]
  (letfn [(analyze-action [action]
            (if (and (sequential? action)
                     (some '#{recur} (flatten action)))
              {:recur-present true} {}))]
    (some :recur-present (map analyze-action actions))))

(defn clj-form [vars clauses]
  (when @*syntax-check* (check-matrix-args vars clauses))
  (let [actions (map second (partition 2 clauses))
        recur-present? (recur-present? actions)]
    ;; TODO: this is naive, recur-present? need ignore
    ;; recur internal to an action - David
    (assert (not (and *no-backtrack* recur-present?))
      "Recur form present yet *no-backtrack* set to true")
    (binding [*recur-present* (or *recur-present*
                                  recur-present?
                                  *no-backtrack*)]
      (-> (emit-matrix vars clauses)
        compile
        executable-form))))

;; ============================================================================
;; # Match macros

(defmacro match 
  "Pattern match a row of occurrences. Take a vector of occurrences, vars.
  Clause question-answer syntax is like `cond`. Questions must be
  wrapped in a vector, with same arity as vars. Last question can be :else,
  which expands to a row of wildcards. Optionally may take a single
  var not wrapped in a vector, questions then need not be wrapped in a
  vector.
  
  Example:
  (let [x 1
        y 2]
    (match [x y 3]
      [1 2 3] :answer1
      :else :default-answer))"
  [vars & clauses]
  (let [[vars clauses]
        (if (vector? vars)
          [vars clauses]
          [(vector vars)
            (mapcat (fn [[c a]]
                      [(if (not= c :else) (vector c) c) a])
              (partition 2 clauses))])]
   (binding [*line* (-> &form meta :line)
             *locals* (dissoc &env '_)
             *warned* (atom false)]
     `~(clj-form vars clauses))))

(defmacro matchv [type vars & clauses]
  (binding [*vector-type* type
            *line* (-> &form meta :line)
            *locals* (dissoc &env '_)
            *warned* (atom false)]
    `~(clj-form vars clauses)))

(defmacro matchm
  "Same as match but supports IMatchLookup when
  matching maps."
  [vars & clauses]
  (let [[vars clauses]
        (if (vector? vars)
          [vars clauses]
          [(vector vars)
           (mapcat (fn [[c a]]
                     [(if (not= c :else) (vector c) c) a])
             (partition 2 clauses))])]
    (binding [*match-lookup* true
              *line* (-> &form meta :line)
              *locals* (dissoc &env '_)
              *warned* (atom false)]
      `~(clj-form vars clauses))))

(defmacro match-let [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (match [~@bindvars#]
         ~@body))))

