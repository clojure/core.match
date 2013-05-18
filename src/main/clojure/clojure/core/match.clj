(ns clojure.core.match
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

;; =============================================================================
;; # Introduction
;;
;; This namespace contains an implementation of closed pattern matching. It uses
;; an algorithm based on Luc Maranget's paper "Compiling Pattern Matching to Good Decision Trees".
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

(def ^{:dynamic true
       :doc "Enable breadcrumb diagnostics with fail nodes"} 
  *breadcrumbs* (atom true))

(def ^{:dynamic true
       :doc "Enable pattern compile time tracing"} 
  *trace* (atom false))

(def ^{:dynamic true
       :doc "Enable backtracking diagnostics"}
  *backtrack-with-errors* (atom false))

(def ^{:dynamic true}
  *clojurescript* false)

(def ^{:dynamic true} *line*)
(def ^{:dynamic true} *locals* nil)
(def ^{:dynamic true} *warned*)

(def ^{:dynamic true
       :doc "Default vector type. Can be rebound allowing emission of custom
             inline code for vector patterns, for example type-hinted primitive 
             array operations"}
  *vector-type* ::vector)

(def ^{:dynamic true} *match-breadcrumbs* [])
(def ^{:dynamic true
       :doc "In the presence of recur we cannot apply code size optimizations"}
  *recur-present* false)

(defn set-trace! [b]
  (reset! *trace* b))

(defn set-breadcrumbs! [b]
  (reset! *breadcrumbs* b))

(def ^{:doc "Pre-allocated exception used for backtracing"}
  backtrack (Exception. "Could not find match."))

(defn backtrack-expr []
  `(throw clojure.core.match/backtrack))

(defn warn [msg]
  (if (not @*warned*)
    (do
      (binding [*out* *err*] 
        (println "WARNING:"
                 (str *ns* ", line " *line* ":") 
                 msg))
      (reset! *warned* true))))

(defn trace-matrix [& p]
  (when @*trace*
    (apply println "TRACE: MATRIX:" p)
    (flush)))

(defn trace-dag [& p]
  (when @*trace*
    (apply println "TRACE: DAG:" p)
    (flush)))

;; =============================================================================
;; # Protocols

(defprotocol ISpecializeMatrix
  (specialize-matrix [this rows ocrs]))

(defprotocol IContainsRestPattern
  (contains-rest-pattern? [this]))

(defprotocol IMatchLookup
  "Allows arbitrary objects to act like a map-like object when pattern
  matched. Avoid extending this directly for Java Beans, see
  `match.java/bean-match`."
  (val-at [this k not-found]))

;; =============================================================================
;; # Map Pattern Interop

(extend-type clojure.lang.ILookup
  IMatchLookup
  (val-at [this k not-found]
    (.valAt this k not-found)))

(defn val-at*
  ([m k] (let [val (val-at m k ::not-found)]
           (if (= val ::not-found)
             (throw backtrack)
             val)))
  ([m k not-found] (val-at m k not-found)))

(defn val-at-expr [& args]
  (if *clojurescript* ;;then we need to inline the correct behavior
    (if (= 3 (count args))
      `(get ~@args)
      (let [[m k] args]
        `(let [val# (get ~m ~k ::not-found)]
           (if (= val# ::not-found)
             (throw clojure.core.match/backtrack)
             val#))))
    ;;If not ClojureScript, defer to val-at*
    `(val-at* ~@args)))

;; =============================================================================
;; # Vector Pattern Interop
;;
;; Vectors patterns can generate code specialized on type. This is useful for
;; generating optimal code for data like primitive arrays and bytes. Defaults for
;; vector are provided, see clojure.core.match.array and clojure.core.match.bits
;; for experiments involving these types.

(defn vector-type [t & r] t)

(defmulti check-size? identity)
(defmulti tag (fn [t] t))
(defmulti test-inline vector-type)
(defmulti test-with-size-inline vector-type)
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
    (with-meta ocr (assoc (ocr meta) :tag the-tag))))

(defmethod test-inline ::vector
  [t ocr]
  (let [the-tag (tag t)
        c (cond
            (class? the-tag) the-tag
            (string? the-tag) (Class/forName the-tag)
            (symbol? the-tag) (Class/forName (str the-tag))
            :else (throw (Error. (str "Unsupported tag type" the-tag))))]
    (if (= t ::vector)
      `(vector? ~ocr)
      `(instance? ~c ~ocr))))

(defmethod test-with-size-inline ::vector
  [t ocr size] `(and ~(test-inline t ocr) (== ~(count-inline t (with-tag t ocr)) ~size)))

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
;; # Extensions and Protocols

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (n-to-clj [this]))

(defprotocol IPatternCompile
  (to-source* [this ocr]))

;; Pattern matrices are represented with persistent vectors. Operations
;; on pattern matrices require us to move something from the middle of the
;; vector to the front - thus prepend and drop-nth. swap will swap the 0th
;; element with the nth element.

(defprotocol IVecMod
  (prepend [this x])
  (drop-nth [this n])
  (swap [this n]))

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
;; # Pattern Comparison
;;
;; Used to determine the set of constructors presents in a column and the
;; order which they should be considered

;; FIXME: we use 1 instead of -1, this means we probably have a reverse
;; somehwere - David

(defmulti pattern-compare
  "Like `clojure.core/compare` but for comparing patterns"
  (fn [a b] [(type a) (type b)]))

(defn pattern-equals [a b]
  (zero? (pattern-compare a b)))

(defmethod pattern-compare :default
  [a b] (if (= (class a) (class b)) 0 1))

(defmulti safe-pattern-compare
  "Like pattern-compare but not affected by *recur-present*"
  (fn [a b] [(type a) (type b)]))

(defmethod safe-pattern-compare :default
  [a b] (pattern-compare a b))

(defmulti comparable?
  "Returns true if it is possible to tell at compile time whether two
   different versions of the same object can never match the same
   object."
  type)

(defmethod comparable? :default
  [x]
  true)

;; =============================================================================
;; # Pattern Rows
;;
;; Pattern rows are one line of a matrix. They correspond to one clause in the
;; in the user's original pattern. patterns, action, bindings are accessors.
;; 

(declare leaf-bind-expr named-wildcard-pattern?)

(deftype PatternRow [ps action bindings]
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
    (PatternRow. (conj ps x) action bindings)))

(defn pattern-row
  ([ps action] 
   {:pre [(vector? ps)]}
   (PatternRow. ps action nil))
  ([ps action bindings]
   {:pre [(vector? ps)]} ;; TODO: what can we expect bindings? (or (nil? bindings) (list? bindings))  ? - Ambrose
   (PatternRow. ps action bindings)))

;; NOTE: we don't use map destructuring here because PatternRow is both
;; ISeq and ILookup, but in map destructuring seq? is tested first - David

(defn update-pattern [prow i p]
  (pattern-row (assoc (:ps prow) i p) (:action prow) (:bindings prow)))

(defn all-wildcards? [prow]
  (every? wildcard-pattern? (:ps prow)))

(defn drop-nth-bind [prow n ocr]
  (let [ps        (:ps prow)
        bindings  (:bindings prow)
        action    (:action prow)
        p         (ps n)
        bind-expr (leaf-bind-expr ocr)
        bindings  (or bindings [])
        bindings  (if-let [sym (-> p meta :as)]
                    (conj bindings [sym bind-expr])
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

(defn leaf-node
  ([value] (LeafNode. value []))
  ([value bindings] (LeafNode. value bindings))) ;; TODO precondition on bindings? see above - Ambrose

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

(defmacro error [& body]
  (if *clojurescript*
    `(js/Error. ~@body)
    `(Exception. ~@body)))

(defrecord FailNode []
  INodeCompile
  (n-to-clj [this]
    (if *recur-present*
      (if @*breadcrumbs*
        `(throw (error (str "No match found. " 
                            "Followed " ~(count *match-breadcrumbs*)  " branches."
                            " Breadcrumbs: " '~*match-breadcrumbs*)))
        `(throw (error (str "No match found."))))
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
  (let [test (if (instance? clojure.core.match.IPatternCompile pattern)
               (to-source* pattern occurrence) 
               (to-source pattern occurrence))]
    (if @*breadcrumbs*
      (binding [*match-breadcrumbs* (conj *match-breadcrumbs* test)]
        [test (n-to-clj action)])
      [test (n-to-clj action)])))

(defn catch-error [& body]
  (let [err-sym (if *clojurescript* 'js/Error 'Exception)]
    `(catch ~err-sym e#
       (if (identical? e# clojure.core.match/backtrack)
         (do
           ~@body)
         (throw e#)))))

(defrecord SwitchNode [occurrence cases default]
  INodeCompile
  (n-to-clj [this]
    (let [clauses (doall (mapcat (partial apply dag-clause-to-clj occurrence) cases))
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr (if *recur-present*
                      (doall
                        (concat
                          `(cond ~@clauses)
                          `(:else ~(n-to-clj default))))
                      (doall
                        (concat
                          `(cond ~@clauses)
                          `(:else
                             ~(if @*backtrack-with-errors*
                                `(throw
                                   (Exception. (str "Could not match" ~occurrence)))
                                (backtrack-expr))))))]
      (if *recur-present*
        (if bind-expr
          `~(doall (concat `(let [~occurrence ~bind-expr]) (list cond-expr)))
          `~cond-expr)
        (if bind-expr
          `(try
             ~(doall (concat `(let [~occurrence ~bind-expr]) (list cond-expr)))
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
  [i (reduce
       (fn [score useful]
         (if useful
           (clojure.core/inc score)
           score))
       0 col)])

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

(defn seq-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :seq))

(defn map-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :map))

;; Returns bindings usable by leaf-node
(defn row-bindings [f ocrs]
  (let [ps (:ps f)
        wc-syms (map :sym ps)
        wc-bindings (map vector wc-syms
                      (map leaf-bind-expr ocrs))]
    (concat (:bindings f) wc-bindings)))

(defn useful-p? [pm i j]
  (let [p (pattern-at pm i j)]
   (cond
    (constructor? p) (every? #(not (wildcard-pattern? %))
                             (take j (column pm i)))
    ;;(wildcard-pattern? p) (not (useful? (drop-nth pm i) j))
    ;;IMPORTANT NOTE: this calculation is very very slow,
    ;;we should look at this more closely - David
    :else false)))

;; DEAD CODE for now - David
;; (defn useful? [pm j]
;;   (some #(useful-p? pm % j)
;;         (range (count (row pm j)))))

(defn useful-matrix [pm]
  (->> (for [j (range (height pm))
             i (range (width pm))]
         (useful-p? pm i j))
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

(declare default-specialize-matrix pseudo-pattern?)

(defn specialize [pm p rows* ocrs*]
  (if (satisfies? ISpecializeMatrix p)
    (specialize-matrix p rows* ocrs*)
    (default-specialize-matrix p rows* ocrs*)))

(defn pseudo-patterns [matrix i]
  (filter pseudo-pattern? (column matrix i)))

(defn matrix-splitter [rows]
  (let [f (first rows)
        [x y] (split-with
                #(if (comparable? f)
                   (comparable? %)
                   (pattern-equals f %))
                (rest rows))]
    [(cons f x) y]))

(declare pattern-matrix compile)

(defn default-matrix [matrix]
  (let [rs (rows matrix)
        m (pattern-matrix
            (into []
              (drop (count (first (matrix-splitter (map first rs)))) rs))
            (occurrences matrix))]
    (if-not (empty-matrix? m)
      (do
        (trace-dag
          (str "Add specialized matrix on row of "
            "wildcards as default matrix for next node"))
        (compile m))
      (do 
        (trace-dag
          (str "Add fail-node as default matrix for next "
            "node (specialized matrix empty)"))
        (fail-node)))))

;; if the user interleaves patterns we want to make them adjacent
;; up until the point that the first wildcard pattern appears in a
;; column. everything including and after a wildcard pattern is always
;; the default matrix
(defn group-rows [rows]
  (let [[s-m-1 s-m-2] (map count (matrix-splitter (map first rows)))
        [l r] [(take s-m-1 rows) (drop s-m-1 rows)]]
    (letfn [(group [[r & rs :as rows]]
              (if (seq rows)
                (let [[fd rd] ((juxt filter remove)
                                #(pattern-equals (first r) (first %))
                                rs)]
                  (concat (cons r fd) (group rd)))))]
      (into [] (concat (group l) r)))))

(declare vector-pattern?)

;; analyze vector patterns, if a vector-pattern containing a rest pattern
;; occurs, drop all previous vector patterns that it subsumes. note this
;; is a bit hard coding that should be removed when get a better sense
;; how to abstract a protocol for this.
(defn group-vector-patterns [ps]
  (reverse
    (reduce
      (fn [ps p]
        (if (and (vector-pattern? p)
              (contains-rest-pattern? p))
          (conj (drop-while #(pattern-equals p %) ps) p)
          (conj ps p)))
      () ps)))

(defn collapse [ps]
  (reduce
    (fn [a b]
      (if (pattern-equals (first (rseq a)) b)
        a
        (conj a b)))
    [] ps))

;; Returns a vector of relevant constructors in column i of matrix
(defn column-constructors [matrix i]
  (let [cs (group-vector-patterns (column matrix i))]
    (collapse (first (matrix-splitter cs)))))

;; Compile a decision trees for each constructor cs and returns a clause list
;; usable by a switch node
(defn switch-clauses [matrix cs]
  (into []
    (map (fn [c rows]
           (let [s (-> matrix
                     (specialize c rows (occurrences matrix)) 
                     compile)]
             [c s]))
      cs (loop [[c :as cs] (seq cs) grouped [] rows (rows matrix)]
           (if (nil? cs)
             grouped
             (let [[l r] (split-with #(pattern-equals c (first %)) rows)]
               (recur (next cs) (conj grouped l) r)))))))

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

(defn switch-or-bind-node [col ocrs clauses default]
  (if (some expression? ocrs)
    (let [b (bind-variables ocrs)
          o (ocrs col)
          n (switch-node o clauses default)
          _ (trace-dag "Add bind-node on occurrence " o ", bindings" b)]
      (bind-node b n))
    (let [o (ocrs col)
          _ (trace-dag "Add switch-node on occurrence " o)]
      (switch-node o clauses default))))

;; -----------------------------------------------------------------------------
;; # Compilation Cases
;;
;; These are analogous to Maranget's Compilation Scheme on page 4, respectively
;; case 1, 2, 2 (also), 3a and 3b.
;;

(defn empty-rows-case 
  "Case 1: If there are no pattern rows to match, then matching always fails"
  []
  (let [_ (trace-dag "No rows left, add fail-node")]
    (fail-node)))

(defn first-row-empty-case 
  "Case 2: If the first row is empty then matching always succeeds 
  and yields the first action."
  [rows ocr]
  (let [f (first rows)
        a (:action f)
        bs (:bindings f)
        _ (trace-dag "Empty row, add leaf-node."
                     "Could not find match for: " ocr
                     "Action:" a
                     "Bindings:" bs)]
    ;; FIXME: the first row is an infinite list of nil - David
    (leaf-node a bs)))

(defn first-row-wildcards-case 
  "Case 2: If the first row is constituted by wildcards then matching
  matching always succeeds and yields the first action."
  [rows ocrs]
  (let [f (first rows)
        a (:action f)
        bs (row-bindings f ocrs)
        _ (trace-dag (str "First row all wildcards, add leaf-node." a bs))]
    (leaf-node a bs)))

(defn first-column-chosen-case 
  "Case 3a: The first column is chosen. Compute and return a switch/bind node
  with a default matrix case"
  [this col ocrs]
  (let [exp-matrix (reduce
                     (fn [matrix p]
                       (specialize matrix p
                         (rows matrix) (occurrences matrix)))
                     this (pseudo-patterns this col))
        new-matrix (pattern-matrix
                     (group-rows (rows exp-matrix))
                     (occurrences exp-matrix))
        constrs (column-constructors new-matrix col)
        clauses (switch-clauses new-matrix constrs)
        default (default-matrix new-matrix)
        _       (trace-dag "Column" col ":" constrs)]
    (switch-or-bind-node col ocrs clauses default)))

(defn other-column-chosen-case 
  "Case 3b: A column other than the first is chosen. Swap column col with the first column
  and compile the result"
  [this col]
  (let [_ (trace-dag "Swap column " col)]
    (compile (swap this col))))

;; Return a column number of a column which contains at least
;; one non-wildcard constructor
(defn choose-column [this]
  (let [col (necessary-column this)
        _ (trace-dag "Pick column" col "as necessary column.")]
    col))

(defn compile [{:keys [rows ocrs] :as pm}]
  (cond
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
  {:pre [(vector rows) 
         (vector ocrs)]}
  (PatternMatrix. rows ocrs))

;; =============================================================================
;; ## Default Matrix Specialization

;; NOTE: not sure why we need pattern-equals here for this to work - David

(defn default-specialize-matrix [p rows ocrs]
  (let [focr (first ocrs)
        nrows (->> rows
                (filter #(pattern-equals p (first %)))
                (map #(drop-nth-bind % 0 focr))
                vec)
        nocrs (drop-nth ocrs 0)
        _ (trace-dag "Perform default matrix specialization on ocr" focr
                     ", new num ocrs: " 
                     (count ocrs) "->" (count nocrs))]
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

(defrecord WildcardPattern [sym])

(defn wildcard-pattern
  ([] (WildcardPattern. '_))
  ([sym] 
    {:pre [(symbol? sym)]}
    (if (= sym '_)
      (WildcardPattern. (gensym))
      (WildcardPattern. sym))))

(defn wildcard-pattern? [x]
  (instance? WildcardPattern x))

;; Local bindings in pattern matching are emulated by using named wildcards.
;; See clojure.lang.Symbol dispatch for `emit-pattern` 

(defn named-wildcard-pattern? [x]
  (when (instance? WildcardPattern x)
    (not= (:sym x) '_)))

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
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (cond
     (= l ()) `(empty? ~ocr)
     (and (symbol? l) (not (-> l meta :local))) `(= ~ocr '~l)
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
;; A Seq Pattern is intended for matching `seq`s. 
;; They are split into multiple patterns, testing each element of the seq in order.
;;

(declare seq-pattern? rest-pattern? seq-pattern)

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
      not-found))
  
  IPatternCompile
  (to-source* [this ocr]
    `(or (seq? ~ocr) (sequential? ~ocr)))

  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [focr (first ocrs)
          nrows (specialize-seq-pattern-matrix rows focr)
          nocrs (seq-pattern-matrix-ocrs ocrs focr)
          _ (trace-dag "SeqPattern specialization on ocr " focr
                       ", new num ocrs" 
                       (count ocrs) "->" (count nocrs))]
      (pattern-matrix nrows nocrs))))

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
  (RestPattern. p))

(defn rest-pattern? [x]
  (instance? RestPattern x))

(defmethod print-method RestPattern [p ^Writer writer]
  (.write writer (str "<RestPattern: " (:p p) ">")))

;; -----------------------------------------------------------------------------
;; # Map Pattern
;; 
;; Map patterns match maps, or any object that satisfies IMatchLookup.
;;

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

(defn get-ocr-map [p env]
  (if (map-pattern? p)
    (let [m (:m p)
          wcs (repeatedly wildcard-pattern)
          [not-found-map wc-map]
          (if-let [only (:only env)]
            [(zipmap (:all-keys env)
               (repeat (literal-pattern ::not-found)))
             (zipmap only wcs)]
            [{} (:wc-map env)])]
      (merge not-found-map (:wc-map env) m))
    (:wc-map env)))

(defn specialize-map-pattern-row [row env]
  (let [p       (first row)
        only    (seq (-> p meta :only))
        ocr-map (get-ocr-map p (assoc env :only only))
        ps      (doall (map ocr-map (:all-keys env)))
        ps      (if @(:only? env)
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
    (reduce prepend (drop-nth-bind row 0 (:focr env))
      (reverse ps))))

(defn specialize-map-pattern-matrix [rows env]
  (vec (map #(specialize-map-pattern-row % env) rows)))

(defn map-pattern-matrix-ocr-sym [k env]
  (let [focr (:focr env)
        ocr  (gensym (str (name focr) "_" (name k) "__"))]
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
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (if *clojurescript*
      `(satisfies? cljs.core/ILookup ~ocr)
      `(or (instance? clojure.lang.ILookup ~ocr) (satisfies? IMatchLookup ~ocr))))

  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [focr     (first ocrs)
          env      {:focr focr
                    :only? (atom false)}
          all-keys (get-all-keys rows env)
          env'     (assoc env
                     :all-keys all-keys
                     :wc-map (zipmap all-keys (repeatedly wildcard-pattern)))
          nrows    (specialize-map-pattern-matrix rows env')
          nocrs    (map-pattern-matrix-ocrs ocrs env')
         _ (trace-dag "MapPattern specialization")]
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

(defprotocol IVectorPattern
  (split [this n]))

(defn touched? [vp]
  (-> vp meta :touched))

(defn touch [vp]
  (let [meta (meta vp)]
    (with-meta vp (assoc meta :touched true))))

(defn touch-all-first [rows]
  (->> rows
       (map (fn [[p & ps :as row]]
              (if (not (touched? p))
                (assoc row 0 (touch p))
                row)))
       (into [])))

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

(defn vector-pattern-maxtrix-ocrs
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
    (into (into [] (map (partial vector-pattern-ocr-sym env) (range min-size)))
      (drop-nth ocrs 0))))

(deftype VectorPattern [v t size offset rest? _meta]
  Object
  (toString [_]
    (str v " " t))

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
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    (if (and (touched? this) (not rest?) size (check-size? t))
      (test-with-size-inline t ocr size)
      (test-inline t ocr)))

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
  (specialize-matrix [this rows ocrs]
    (if (not (touched? (ffirst rows)))
      (pattern-matrix (touch-all-first rows) ocrs)
      (let [focr (first ocrs)
            env {:focr focr
                 :fp   (ffirst rows)
                 :pat  this}
            [rest? min-size] (calc-rest?-and-min-size rows env)
            env' (assoc env
                   :rest? rest? :min-size min-size :tag (:t this))
            nrows (specialize-vector-pattern-matrix rows env')
            nocrs (vector-pattern-maxtrix-ocrs ocrs env')]
        (pattern-matrix nrows nocrs)))))

(defn vector-pattern
  ([] (vector-pattern [] ::vector nil nil))
  ([v] (vector-pattern v ::vector nil nil))
  ([v t] (vector-pattern v t nil nil nil))
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
    (if (and (pattern-equals pat p)
             (not (wildcard-pattern? p)))
      (map (fn [p] (update-pattern row 0 p)) ps) [row])))

(defn specialize-or-pattern-matrix [rows pat ps]
  (vec (apply concat
         (map #(specialize-or-pattern-row % pat ps) rows))))

(deftype OrPattern [ps _meta]
  Object
  (toString [this]
    (str ps))

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
      not-found))

  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [nrows (specialize-or-pattern-matrix rows this ps)
          _     (trace-dag "OrPattern specialization")]
      (pattern-matrix nrows ocrs))))

(defn or-pattern [p]
  {:pre [(vector? p)]}
  (OrPattern. p nil))

(defn or-pattern? [x]
  (instance? OrPattern x))

(defmethod print-method OrPattern [p ^Writer writer]
  (.write writer (str "<OrPattern: " (:ps p) ">")))

;; -----------------------------------------------------------------------------
;; Pseudo-patterns
;;
;; Pseudo-patterns like OrPatterns are not real patterns. OrPatterns are much
;; like a macro, they just expands into a simpler form. This expansion is
;; dealt with specially in first-column-chosen-case.

(defmulti pseudo-pattern? type)

(defmethod pseudo-pattern? OrPattern
  [x] true)

(defmethod pseudo-pattern? :default
  [x] false)

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
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))

  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [nrows (specialize-guard-pattern-matrix rows)
          _ (trace-dag "GuardPattern specialization")]
      (pattern-matrix nrows ocrs))))

(defn guard-pattern [p gs]
  {:pre [(set? gs)]}
  (GuardPattern. p gs nil))

(defn guard-pattern? [x]
  (instance? GuardPattern x))

(defmethod print-method GuardPattern [p ^Writer writer]
  (.write writer (str "<GuardPattern " (:p p) " :guard " (:gs p) ">")))

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
      not-found))

  IPatternCompile
  (to-source* [this ocr]
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))

  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [nrows (specialize-predicate-pattern-matrix rows) 
          _ (trace-dag "PredicatePattern specialization")]
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

(defmethod pattern-compare [WildcardPattern WildcardPattern]
  [a b] 1)

(defmethod comparable? WildcardPattern
  [x] false)

;; NOTE: if recur is present we want all objects to equal wildcards, this is
;; because we push the wildcard matches along as well in the matrix specialization
;; since we don't have backtracking in this case - David

(defmethod pattern-compare [Object WildcardPattern]
  [a b] (if *recur-present* 0 1))

(defmethod safe-pattern-compare [Object WildcardPattern]
  [a b] 1)

(prefer-method pattern-compare [Object WildcardPattern] [LiteralPattern Object])

(defmethod pattern-compare [LiteralPattern Object]
  [a b] 1)

(defmethod pattern-compare [Object LiteralPattern]
  [a b] 1)

(defmethod pattern-compare [LiteralPattern LiteralPattern]
  [a b]
  (cond
    (= (:l a) (:l b)) 0
    :else 1))

(defmethod comparable? LiteralPattern
  [x]
  (not (-> x meta :local)))

(defmethod pattern-compare [GuardPattern GuardPattern]
  [a b] (if (= (:gs a) (:gs b)) 0 1))

(defmethod comparable? GuardPattern
  [x]
  false)

(defmethod pattern-compare [GuardPattern WildcardPattern]
  [a b]
  (let [p (:p a)]
    (if (wildcard-pattern? p)
      (pattern-compare p b) 1)))

(defmethod pattern-compare [PredicatePattern PredicatePattern]
  [a b] (if (= (:gs a) (:gs b)) 0 1))

(defmethod pattern-compare [PredicatePattern WildcardPattern]
  [a b]
  (let [p (:p a)]
    (if (wildcard-pattern? p)
      (pattern-compare p b) 1)))

(defmethod pattern-compare [OrPattern OrPattern]
  [a b]
  (let [as (:ps a)
        bs (:ps b)]
    (if (and (= (count as) (count bs))
             (every? identity (map pattern-equals as bs)))
      0 1)))

(defmethod pattern-compare [VectorPattern VectorPattern]
  [a b]
  (cond
    (not (touched? b)) 0
    (= (:size a) (:size b)) 0
    (and (:rest? a) (<= (:size a) (:size b))) 0
    (and (:rest? b) (<= (:size b) (:size a))) 0
    :else 1))

;; =============================================================================
;; # Interface

(defmulti to-source 
  "Returns a Clojure form that, when executed, is truthy if the pattern matches
  the occurrence. Dispatches on the `type` of the pattern. For instance, a literal pattern 
  might return `(= ~(:pattern pattern) ~ocr)`, using `=` to test for a match."
  (fn [pattern ocr] (type pattern)))

(defmulti emit-pattern 
  "Returns the corresponding pattern for the given syntax. Dispatches
  on the class of its argument. For example, `[(:or 1 2) 2]` is dispatched
  as clojure.lang.IPersistentVector"
  class)

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

(defmethod emit-pattern clojure.lang.IPersistentVector
  [pat]
  (let [ps (emit-patterns pat :vector)]
    (vector-pattern ps *vector-type* 0 (some rest-pattern? ps))))

(defmethod emit-pattern clojure.lang.IPersistentMap
  [pat]
  (map-pattern
    (->> pat
      (map (fn [[k v]]
             [k (emit-pattern v)]))
      (remove nil?)
      (into {}))))

(defmethod emit-pattern clojure.lang.Symbol
  [pat]
  (if (get *locals* pat)
    (literal-pattern (with-meta pat (assoc (meta pat) :local true)))
    (wildcard-pattern pat)))

(defmethod emit-pattern :default
  [pat]
  (literal-pattern pat))

(declare emit-pattern-for-syntax or-pattern as-pattern guard-pattern
         predicate-pattern vector-pattern)

(defmethod emit-pattern clojure.lang.ISeq
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
      [f (type s)]
      [(type f) s])))

(defmethod emit-pattern-for-syntax [:or Object]
  [pat] (or-pattern
         (->> (rest pat)
              (map emit-pattern)
              (into []))))

(defmethod emit-pattern-for-syntax [Object :as]
  [[p _ sym]] (with-meta (emit-pattern p) {:as sym}))

(defmethod emit-pattern-for-syntax [Object :when]
  [[p _ gs]]
  (let [gs (if (not (vector? gs)) [gs] gs)]
    (assert (every? symbol? gs) (str "Invalid predicate expression " gs))
    (assert (every? #(contains? @preds %) gs) (str "Unknown predicate in " gs))
    (predicate-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax [Object :guard]
  [[p _ gs]] (let [gs (if (not (vector? gs)) [gs] gs)]
              (guard-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax [Object :seq]
  [pat]
  (let [p (first pat)]
    (if (empty? p)
      (literal-pattern ())
      (seq-pattern (emit-patterns p :seq)))))

(defmethod emit-pattern-for-syntax [Object ::vector]
  [[p t offset-key offset]]
  (let [ps (emit-patterns p :vector)]
    (vector-pattern ps t offset (some rest-pattern? ps))))

(defmethod emit-pattern-for-syntax [Object :only]
  [[p _ only]] (with-meta (emit-pattern p) {:only only}))

(defmethod emit-pattern-for-syntax :default
  [[_ s :as l]]
  (throw
    (AssertionError.
      (str "Invalid list syntax " s " in " l ". "
        "Valid syntax: "
        (vec
          (remove #(= % :default)
            (keys (.getMethodTable ^clojure.lang.MultiFn emit-pattern-for-syntax))))))))


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
   "Returns a pattern with pattern-keywords (:when and :as) properly grouped.  
    The original pattern may use the 'flattened' syntax.  For example, a 'flattened' 
    pattern row like [a b :when even?] is grouped as [a (b :when even?)]."
  [pattern]
  (if (vector? pattern) (regroup-keywords pattern) pattern))

(defn to-pattern-row
  "Take an unprocessed pattern expression and an action expression and return
   a pattern row of the processed pattern expression plus the action epxression."
  [pat action]
  (let [p (into [] (map emit-pattern (group-keywords pat)))]
    (pattern-row p action)))

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
            ".  The following wildcards are ambiguous: " (apply str (interpose ", " duplicates))
            ".  There's no guarantee that the matched values will be same.  Rename the occurrences uniquely."))))))

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

;; TODO: more sophisticated analysis that actually checks that recur is
;; not being used as a local binding when it occurs - David

(defn analyze-actions [actions]
  (letfn [(analyze-action [action]
            (if (and (sequential? action)
                     (some '#{recur} (flatten action)))
              {:recur-present true} {}))]
    (map analyze-action actions)))

(defn process-vars
  "Process the vars for the pattern matrix. If user provides an
   expression, create a var and bind the expression to the var."
  [vars]
  (letfn [(process-var [var]
            (if-not (symbol? var)
              (let [nsym (gensym "ocr-")
                     _ (trace-dag "Bind ocr" var "to" nsym)]
                (with-meta nsym {:ocr-expr var}))
              var))]
    (vec (map process-var vars))))

(defn emit-matrix
  "Take the list of vars and sequence of unprocessed clauses and
   return the pattern matrix. The pattern matrix contains the processed
   pattern rows and the list of vars originally specified. Inserts
   a last match - :else if provided by the user or a default match that
   throws."
  [vars clauses]
  (let [cs (partition 2 clauses)
        cs (let [[p a] (last cs)
                 last-match (vec (map (fn [_] '_) vars))]
             (if (= :else p)
               (do (trace-matrix "Convert :else clause to row of wildcards")
                   (conj (vec (butlast cs)) [last-match a]))
               ;; TODO: throw an exception if :else line not provided - David
               (conj (vec cs) [last-match nil])))]
    (pattern-matrix
      (vec (map #(apply to-pattern-row %) cs))
      (process-vars vars))))

(defn executable-form [node]
  (n-to-clj node))

(defn clj-form [vars clauses]
  (when @*syntax-check* (check-matrix-args vars clauses))
  (let [actions (map second (partition 2 clauses))
        recur-present (some :recur-present
                        (analyze-actions actions))]
    (binding [*recur-present* recur-present]
      (-> (emit-matrix vars clauses)
        compile
        executable-form))))

;; ============================================================================
;; # Match macros

(defmacro match 
  "Pattern match a row of occurrences. Take a vector of occurrences, vars.
  Clause question-answer syntax is like `cond`. Questions must be
  wrapped in a vector, with same arity as vars. Last question can be :else,
  which expands to a row of wildcards.
  
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

(defmacro match-let [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (match [~@bindvars#]
         ~@body))))
