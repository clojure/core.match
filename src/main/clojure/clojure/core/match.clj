(ns clojure.core.match
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

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

(comment
 (match [x y]
        [1 2] :a0 
        [3 4] :a1))

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
(def ^{:dynamic true} *vector-type* ::vector)
(def ^{:dynamic true} *match-breadcrumbs* [])
(def ^{:dynamic true} *recur-present* false)

(defn set-trace! [b]
  (reset! *trace* b))

(defn set-breadcrumbs! [b]
  (reset! *breadcrumbs* b))

(def backtrack (Exception. "Could not find match."))

(defn backtrack-expr []
  (if *clojurescript*
    `(throw 0)
    `(throw clojure.core.match/backtrack)))

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

(defprotocol IPatternContainer
  (pattern [this]))

(defprotocol IContainsRestPattern
  (contains-rest-pattern? [this]))

(defprotocol IMatchLookup
  "Allows arbitrary objects to act like a map-like object when pattern
  matched. Avoid extending this directly for Java Beans, see
  `match.java/bean-match`."
  (val-at* [this k not-found]))

;; =============================================================================
;; # Map Pattern Interop

(extend-type clojure.lang.ILookup
  IMatchLookup
  (val-at* [this k not-found]
    (.valAt this k not-found)))

(defn val-at
  ([m k] (val-at* m k nil))
  ([m k not-found] (val-at* m k not-found)))

(defn val-at-expr [& args]
  (if *clojurescript*
    `(get ~@args)
    `(val-at ~@args)))

;; =============================================================================
;; # Vector Pattern Interop

(defmulti check-size? identity)
(defmulti tag (fn [t] t))
(defmulti test-inline (fn [t & r] t))
(defmulti test-with-size-inline (fn [t & r] t))
(defmulti count-inline (fn [t & r] t))
(defmulti nth-inline (fn [t & r] t))
(defmulti nth-offset-inline (fn [t & r] t))
(defmulti subvec-inline (fn ([t & r] t)))

(defmethod check-size? :default
  [_] true)

(defmethod tag :default
  [t] (throw (Exception. (str "No tag specified for vector specialization " t))))

(defmethod tag ::vector
  [_] clojure.lang.IPersistentVector)

(defn with-tag [t ocr]
  (let [the-tag (tag t)
        the-tag (if (.isArray ^Class the-tag)
                  (.getName ^Class the-tag)
                  the-tag)]
    (with-meta ocr (assoc (ocr meta) :tag the-tag))))

(defmethod test-inline ::vector
  [t ocr] (if (= t ::vector)
           `(vector? ~ocr)
           `(instance? ~(tag t) ~ocr)))

(defmethod test-with-size-inline ::vector
  [t ocr size] `(and ~(test-inline t ocr) (= ~(count-inline t (with-tag t ocr)) ~size)))

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

;; =============================================================================
;; # Extensions and Protocols

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (n-to-clj [this]))

(defprotocol IPatternCompile
  (to-source* [this ocr]))

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

;; =============================================================================
;; # Pattern Rows

(defprotocol IPatternRow
  (action [this])
  (patterns [this])
  (update-pattern [this i p])
  (bindings [this])
  (all-wildcards? [this])
  (drop-nth-bind [this n bind-expr])) ;; TODO: needs better name - David

(declare leaf-bind-expr)
(declare named-wildcard-pattern?)
(declare sym)

(deftype PatternRow [ps action bindings]
  IPatternRow
  (action [_] action)
  (patterns [_] ps)
  (update-pattern [_ i p]
    (PatternRow. (assoc ps i p) action bindings))
  (bindings [_] bindings)
  (all-wildcards? [this]
    (every? wildcard-pattern? ps))
  (drop-nth-bind [this n ocr]
    (let [p (ps n)
          bind-expr (leaf-bind-expr ocr)
          bindings (or bindings [])
          bindings (if-let [sym (-> p meta :as)]
                     (conj bindings [sym bind-expr])
                     bindings)
          bindings (if (named-wildcard-pattern? p)
                       (conj bindings [(sym p) bind-expr])
                       bindings)]
      (PatternRow. (drop-nth ps n) action
                   bindings)))
  IVecMod
  (drop-nth [_ n]
    (PatternRow. (drop-nth ps n) action bindings))
  (prepend [_ x]
    (PatternRow. (into [x] ps) action bindings))
  (swap [_ n]
    (PatternRow. (swap ps n) action bindings))
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
  clojure.lang.IFn
  (invoke [_ n]
    (nth ps n))
  clojure.lang.IPersistentCollection
  (cons [_ x]
    (PatternRow. (conj ps x) action bindings)))

(defn ^PatternRow pattern-row
  ([ps action] 
   {:pre [(vector? ps)]}
   (PatternRow. ps action nil))
  ([ps action bindings]
   {:pre [(vector? ps)]} ;; TODO: what can we expect bindings? (or (nil? bindings) (list? bindings))  ? - Ambrose
   (PatternRow. ps action bindings)))

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

(defn ^LeafNode leaf-node
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

(defn ^FailNode fail-node []
  (FailNode.))

;; -----------------------------------------------------------------------------
;; ## Bind Node

(defrecord BindNode [bindings node]
  INodeCompile
  (n-to-clj [this]
    `(let [~@bindings]
       ~(n-to-clj node))))

(defn ^BindNode bind-node [bindings node]
  (BindNode. bindings node))

;; -----------------------------------------------------------------------------
;; ## Switch Node

(declare to-source)

(defn dag-clause-to-clj [occurrence pattern action]
  (let [test (if (extends? IPatternCompile (class pattern))
               (to-source* pattern occurrence) 
               (to-source pattern occurrence))]
    (if @*breadcrumbs*
      (binding [*match-breadcrumbs* (conj *match-breadcrumbs* test)]
        [test (n-to-clj action)])
      [test (n-to-clj action)])))

(defn catch-error [& body]
  (if *clojurescript*
    `(catch e#
       (if (identical? e# 0)
         (do
           ~@body)
         (throw e#)))
    `(catch Exception e#
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
                      (doall (concat `(cond ~@clauses)
                                     `(:else ~(n-to-clj default))))
                      (doall (concat `(cond ~@clauses)
                                     `(:else ~(if @*backtrack-with-errors*
                                                `(throw (Exception. (str "Could not match" ~occurrence)))
                                                (backtrack-expr))))))]
      (if *recur-present*
        (if bind-expr
          `~(doall (concat `(let [~occurrence ~bind-expr]) (list cond-expr)))
          `~cond-expr)
        (if bind-expr
          `(try ~(doall (concat `(let [~occurrence ~bind-expr]) (list cond-expr)))
                ~(catch-error (n-to-clj default)))
          `(try ~cond-expr
                ~(catch-error (n-to-clj default))))))))

(defn ^SwitchNode switch-node
  ([occurrence cases default]
   {:pre [(sequential? cases)]}
   (SwitchNode. occurrence cases default)))

;; =============================================================================
;; # Pattern Matrix

(defn seq-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :seq))

(defn map-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :map))

(defprotocol IPatternMatrix
  (width [this])
  (height [this])
  (dim [this])
  (specialize [this c rows ocrs])
  (compile [this])
  (pattern-at [this i j])
  (column [this i])
  (row [this j])
  (rows [this])
  (insert-row [this i row])
  (insert-rows [this i rows])
  (necessary-column [this])
  (useful-matrix [this])
  (select [this])
  (occurrences [this])
  (action-for-row [this j]))

(declare empty-matrix?)
(declare useful-p?)
(declare useful?)

;; # Compilation Cases
;;
;; These are analogous to Maranget's Compilation Scheme on page 4, respectively
;; case 1, 2, 2 (also), 3a and 3b.
;;

(defn- empty-rows-case 
  "Case 1: If there are no pattern rows to match, then matching always fails"
  []
  (let [_ (trace-dag "No rows left, add fail-node")]
    (fail-node)))

(defn- first-row-empty-case 
  "Case 2: If the first row is empty then matching always succeeds 
  and yields the first action."
  [rows ocr]
  (let [^PatternRow f (first rows)
        a (action f)
        bs (bindings f)
        _ (trace-dag "Empty row, add leaf-node."
                     "Could not find match for: " ocr
                     "Action:" a
                     "Bindings:" bs)]
    ;; FIXME: wtf f, the first row is an infinite list of nil - David
    (leaf-node a bs)))

(defn- first-row-wildcards-case 
  "Case 2: If the first row is constituted by wildcards then matching
  matching always succeeds and yields the first action."
  [rows ocrs]
  (letfn [(row-bindings 
            ;; Returns bindings usable by leaf-node
            [f ocrs]
            (let [ps (.ps ^PatternRow f)
                  wc-syms (map #(sym %) ps)
                  wc-bindings (map vector wc-syms
                                   (map leaf-bind-expr ocrs))]
              (concat (bindings f)
                      wc-bindings)))]
    (let [f (first rows)
          a (action f)
          bs (row-bindings f ocrs)
          _ (trace-dag (str "First row all wildcards, add leaf-node." a bs))]
      (leaf-node a bs))))

(declare pseudo-pattern?)
(declare wildcard-pattern)
(declare vector-pattern?)
(declare pattern-matrix)

(defn- first-column-chosen-case 
  "Case 3a: The first column is chosen. Compute and return a switch/bind node
  with a default matrix case"
  [this col ocrs]
  (letfn [(pseudo-patterns [this i]
            (->> (column this i)
              (filter pseudo-pattern?)))
          
          (default-matrix 
            [this]
            (let [m (pattern-matrix (into [] (drop-while #(not (wildcard-pattern? (first %)))
                                                         (rows this)))
                                    (occurrences this))]
              (if-not (empty-matrix? m)
                (do (trace-dag "Add specialized matrix on row of wildcards as default matrix for next node")
                  (compile m))
                (do 
                  (trace-dag "Add fail-node as default matrix for next node (specialized matrix empty)")
                  (fail-node)))))

          ;; analyze vector patterns, if a vector-pattern containing a rest pattern
          ;; occurs, drop all previous vector patterns that it subsumes. note this
          ;; is a bit hard coding that should be removed when get a better sense
          ;; how to abstract a protocol for this.
          (group-vector-patterns [ps]
            (-> (reduce (fn [ps p]
                          (if (and (vector-pattern? p)
                                   (contains-rest-pattern? p))
                            (conj (drop-while #(pattern-equals p %) ps) p)
                            (conj ps p)))
                        () ps)
                reverse))

          (collapse [ps]
            (reduce (fn [a b]
                      (if (pattern-equals (first (rseq a)) b)
                        a
                        (conj a b)))
                    [] ps))

          (column-constructors 
            ;; Returns a vector of relevant constructors in column i of matrix this
            [this i]
            (let [ps (group-vector-patterns (column this i))
                  ps (take-while (comp not wildcard-pattern?) ps)]
              (collapse ps)))

          (switch-clauses 
            ;; Compile a decision trees for each constructor cs and returns a clause list
            ;; usable by a switch node
            [this cs]
            (into []
                  (map (fn [c rows]
                         (let [s (-> this 
                                   (specialize c rows (occurrences this)) 
                                   compile)]
                           [c s]))
                       cs (loop [[c :as cs] (seq cs) grouped [] rows (rows this)]
                            (if (nil? cs)
                              grouped
                              (let [[l r] (split-with #(pattern-equals c (first %)) rows)]
                                (recur (next cs) (conj grouped l) r)))))))

          (switch-or-bind-node [col ocrs clauses default]
            (letfn [(expression? 
                      ;; Returns true if occurrence ocr is an expression
                      [ocr] 
                      (contains? (meta ocr) :ocr-expr))
                    (bind-variables 
                      ;; Return bindings usable by bind-node
                      [ocrs] 
                      (mapcat (fn [ocr]
                                (let [bind-expr (get (meta ocr) :ocr-expr ::not-found)]
                                  (if (not= bind-expr ::not-found)
                                    [ocr bind-expr]
                                    [ocr ocr])))
                              ocrs))]
              (if (some expression? ocrs)
                (let [b (bind-variables ocrs)
                      o (ocrs col)
                      n (switch-node o clauses default)
                      _ (trace-dag "Add bind-node on occurrence " o ", bindings" b)]
                  (bind-node b n))
                (let [o (ocrs col)
                      _ (trace-dag "Add switch-node on occurrence " o)]
                  (switch-node o clauses default)))))]
    (let [this (reduce (fn [matrix p]
                         (specialize matrix p (rows matrix) (occurrences matrix)))
                       this (pseudo-patterns this col))
          constrs (column-constructors this col)
          clauses (switch-clauses this constrs)
          default (default-matrix this)
          _ (trace-dag "Column" col ":" constrs)]
      (switch-or-bind-node col ocrs clauses default))))

(defn- other-column-chosen-case 
  "Case 3b: A column other than the first is chosen. Swap column col with the first column
  and compile the result"
  [this col]
  (let [_ (trace-dag "Swap column " col)]
    (compile (swap this col))))

;; # Pattern Matrix definition

(declare default-specialize-matrix)

(deftype PatternMatrix [rows ocrs _meta]
  clojure.lang.IObj
  (meta [_] _meta)

  (withMeta [_ new-meta]
    (PatternMatrix. rows ocrs new-meta))

  IPatternMatrix
  (width [_] (if (not (empty? rows))
               (count (rows 0))
               0))

  (height [_] (count rows))

  (dim [this] [(width this) (height this)])

  (specialize [this p rows* ocrs*]
    (if (satisfies? ISpecializeMatrix p)
     (specialize-matrix p rows* ocrs*)
     (default-specialize-matrix p rows* ocrs*)))

  (column [_ i] (vec (map #(nth % i) rows)))

  (compile [this]
    (letfn [(choose-column 
              ;; Return a column number of a column which contains at least
              ;; one non-wildcard constructor
              [this]
              (let [col (necessary-column this)
                    _ (trace-dag "Pick column" col "as necessary column.")]
                col))
            
            (first-column? [i]
              (zero? i))
            
            (empty-row? [row]
              (let [ps (patterns row)]
                (and (not (nil? ps))
                     (empty? ps))))]
      (cond
        (empty? rows) (empty-rows-case)

        (empty-row? (first rows)) (first-row-empty-case rows (first ocrs))

        (all-wildcards? (first rows)) (first-row-wildcards-case rows ocrs)

        :else (let [col (choose-column this)]
                (if (first-column? col)
                  (first-column-chosen-case this col ocrs)
                  (other-column-chosen-case this col))))))

  (pattern-at [_ i j] ((rows j) i))

  (row [_ j] (nth rows j))

  (necessary-column [this]
    (letfn [(score-column [i col]
              [i (reduce (fn [score useful]
                           (if useful
                             (clojure.core/inc score)
                             score))
                         0 col)])]
      (first
       (->> (apply map vector (useful-matrix this))
            (map-indexed score-column)
            (reduce (fn [[col score :as curr]
                         [ocol oscore :as cand]]
                      (if (> oscore score) cand curr))
                    [0 0])))))

  (useful-matrix [this]
    (vec (->> (for [j (range (height this))
                    i (range (width this))]
                (useful-p? this i j))
              (partition (width this))
              (map vec))))

  (select [this]
    (swap this (necessary-column this)))

  (rows [_] rows)

  (insert-row [_ i row]
    (PatternMatrix. (into (conj (subvec rows 0 i) row) (subvec rows i))
                    ocrs
                    _meta))

  (insert-rows [_ i rows]
    (PatternMatrix. (into (into (subvec rows 0 i) rows) (subvec rows i))
                    ocrs
                    _meta))

  (occurrences [_] ocrs)

  (action-for-row [_ j]
    (action (rows j)))

  IVecMod
  (drop-nth [_ i]
    (PatternMatrix. (vec (map #(drop-nth % i) rows)) ocrs _meta))

  ;; Swap column number idx with the first column
  (swap [_ idx]
    (PatternMatrix. (vec (map #(swap % idx) rows))
                    (swap ocrs idx)
                    _meta)))

(defn ^PatternMatrix pattern-matrix [rows ocrs]
  {:pre [(vector rows) 
         (vector ocrs)]}
  (PatternMatrix. rows ocrs nil))

(defn empty-matrix? [pm]
  (= (dim pm) [0 0]))

(defn useful-p? [pm i j]
  (let [p (pattern-at pm i j)]
   (cond
    (constructor? p) (every? #(not (wildcard-pattern? %))
                             (take j (column pm i)))
    ;;(wildcard-pattern? p) (not (useful? (drop-nth pm i) j))
    ;;IMPORTANT NOTE: this calculation is very very slow,
    ;;we should look at this more closely - David
    :else false)))

(defn useful? [pm j]
  (some #(useful-p? pm % j)
        (range (count (row pm j)))))


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

(defprotocol IWildcardPattern
  (sym [this]))

(deftype WildcardPattern [sym _meta]
  IWildcardPattern
  (sym [_] sym)
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (WildcardPattern. sym new-meta))
  Object
  (toString [_]
    (str sym)))

(defn ^WildcardPattern wildcard-pattern
  ([] (WildcardPattern. '_ nil))
  ([sym] 
   {:pre [(symbol? sym)]}
   (WildcardPattern. sym nil)))

(defn wildcard-pattern? [x]
  (instance? WildcardPattern x))

;; Local bindings in pattern matching are emulated by using named wildcards.
;; See clojure.lang.Symbol dispatch for `emit-pattern` 

(defn named-wildcard-pattern? [x]
  (when (instance? WildcardPattern x)
    (not= (.sym ^WildcardPattern x) '_)))

(defmethod print-method WildcardPattern [^WildcardPattern p ^Writer writer]
  (.write writer (str "<WildcardPattern: " (.sym p) ">")))

;; -----------------------------------------------------------------------------
;; ## Literal Pattern
;;
;; A literal pattern is not further split into further patterns in the DAG
;; compilation phase.
;;
;; It "literally" matches a given occurrence.

(deftype LiteralPattern [l _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (LiteralPattern. l new-meta))
  IPatternCompile
  (to-source* [this ocr]
    (cond
     (= l ()) `(empty? ~ocr)
     (and (symbol? l) (not (-> l meta :local))) `(= ~ocr '~l)
     :else `(= ~ocr ~l)))
  Object
  (toString [_]
    (if (nil? l)
      "nil"
      (pr-str l))))

(defn ^LiteralPattern literal-pattern [l] 
  (LiteralPattern. l nil))

(defn literal-pattern? [x]
  (instance? LiteralPattern x))

(defmethod print-method LiteralPattern [^LiteralPattern p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; ## Seq Pattern
;;
;; A Seq Pattern is intended for matching `seq`s. 
;;
;; They are split into multiple patterns, testing each element of the seq in order.
;;

(declare seq-pattern?)
(declare rest-pattern?)
(declare seq-pattern)

(deftype SeqPattern [s _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (SeqPattern. s new-meta))
  IPatternCompile
  (to-source* [this ocr]
    `(or (seq? ~ocr) (sequential? ~ocr)))
  Object
  (toString [_]
    (str s))
  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [focr (first ocrs)
          nrows (->> rows
                     (map (fn [row]
                            (let [p (first row)
                                  [h t] (if (seq-pattern? p)
                                          (let [^SeqPattern p p
                                                [h & t] (.s p)
                                                t (cond
                                                   (empty? t) (literal-pattern ())
                                                   (rest-pattern? (first t)) (pattern (first t))
                                                   :else (seq-pattern t))]
                                            [h t])
                                          [(wildcard-pattern) (wildcard-pattern)])]
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      [t h]))))
                     vec)
          nocrs (let [seq-ocr focr
                      seq-sym (or (-> seq-ocr meta :seq-sym) seq-ocr)
                      sym-meta {:occurrence-type :seq
                                :seq-sym seq-ocr}
                      hsym (gensym (str (name seq-sym) "_head__"))
                      hsym (with-meta hsym
                             (assoc sym-meta :bind-expr `(first ~seq-ocr)))
                      tsym (gensym (str (name seq-sym) "_tail__"))
                      tsym (with-meta tsym
                             (assoc sym-meta :bind-expr `(rest ~seq-ocr)))]
                  (into [hsym tsym] (drop-nth ocrs 0)))
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

(defmethod print-method SeqPattern [^SeqPattern p ^Writer writer]
  (.write writer (str "<SeqPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; ### Rest Pattern
;; 
;; A rest pattern represents the case of matching [2 3] in [1 & [2 3]]
;;
;; It is an implementation detail of other patterns, like SeqPattern.

(deftype RestPattern [p _meta]
  IPatternContainer
  (pattern [_] p)
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (RestPattern. p new-meta))
  Object
  (toString [_]
    p))

(defn ^RestPattern rest-pattern [p]
  (RestPattern. p nil))

(defn rest-pattern? [x]
  (instance? RestPattern x))

(defmethod print-method RestPattern [^RestPattern p ^Writer writer]
  (.write writer (str "<RestPattern: " (.p p) ">")))

;; -----------------------------------------------------------------------------
;; # Map Pattern
;; 
;; Map patterns match maps, or any object that satisfies IMatchLookup.

(declare map-pattern?)
(declare guard-pattern)

(deftype MapPattern [m _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (MapPattern. m new-meta))
  IPatternCompile
  (to-source* [this ocr]
    (if *clojurescript*
      `(or (satisfies? cljs.core.ILookup ~ocr))
      `(or (instance? clojure.lang.ILookup ~ocr) (satisfies? IMatchLookup ~ocr))))
  Object
  (toString [_]
    (str m " :only " (or (:only _meta) [])))
  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [focr (first ocrs)
          only? (atom false)
          all-keys (->> rows
                        (remove (comp wildcard-pattern? first))
                        (map (fn [row]
                               (let [^MapPattern p (first row)
                                     only (-> p meta :only)]
                                 (when (and (not @only?) (seq only))
                                   (reset! only? true))
                                 [(set (keys (.m p)))
                                  (set only)])))
                        (reduce concat)
                        (reduce set/union #{}))
          wcs (repeatedly wildcard-pattern)
          wc-map (zipmap all-keys wcs)
          nrows (->> rows
                     (map (fn [row]
                            (let [p (first row)
                                  only (seq (-> p meta :only))
                                  ocr-map (if (map-pattern? p)
                                            (let [^MapPattern p p
                                                  m (.m p)
                                                  [not-found-map wc-map] (if only
                                                                           [(zipmap all-keys
                                                                                    (repeat (literal-pattern ::not-found)))
                                                                            (zipmap only wcs)]
                                                                           [{} wc-map])]
                                              (merge not-found-map wc-map m))
                                            wc-map)
                                  ps (map ocr-map all-keys)
                                  ps (if @only?
                                       (if only
                                         (let [a (with-meta (gensym) {:tag 'java.util.Map})]
                                           (cons (guard-pattern (wildcard-pattern)
                                                                (set [`(fn [~a] (= (.keySet ~a) #{~@only}))]))
                                                 ps))
                                         (cons (wildcard-pattern) ps))
                                       ps)]
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      (reverse ps)))))
                     vec)
          nocrs (let [map-ocr focr
                      ocr-sym (fn ocr-sym [k]
                                (let [ocr (gensym (str (name map-ocr) "_" (name k) "__"))]
                                  (with-meta ocr
                                    {:occurrence-type :map
                                     :key k
                                     :map-sym map-ocr
                                     :bind-expr (val-at-expr map-ocr k ::not-found)})))
                      mocrs (map ocr-sym all-keys)
                      mocrs (if @only?
                              (cons map-ocr mocrs)
                              mocrs)]
                  (into (into [] mocrs)
                        (drop-nth ocrs 0)))
          _ (trace-dag "MapPattern specialization")]
      (pattern-matrix nrows nocrs))))

(defn ^MapPattern map-pattern
  ([] (MapPattern. {} nil))
  ([m] {:pre [(map? m)]}
     (MapPattern. m nil)))

(defn map-pattern? [x]
  (instance? MapPattern x))

(defmethod print-method MapPattern [^MapPattern p ^Writer writer]
  (.write writer (str "<MapPattern: " p ">")))

;; -----------------------------------------------------------------------------

(defprotocol IVectorPattern
  (split [this n]))

(declare vector-pattern?)

(deftype VectorPattern [v t size offset rest? _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (VectorPattern. v t size offset rest? new-meta))
  IPatternCompile
  (to-source* [_ ocr]
    (if (and (not rest?) size (check-size? t))
      (test-with-size-inline t ocr size)
      (test-inline t ocr)))
  Object
  (toString [_]
    (str v " " t))
  IContainsRestPattern
  (contains-rest-pattern? [_] rest?)
  IVectorPattern
  (split [this n]
    (let [lv (subvec v 0 n)
          rv (subvec v n)
          pl (VectorPattern. lv t n offset false _meta)
          pr (if (rest-pattern? (first rv))
               (let [^RestPattern p (first rv)] (.p p))
               (let [rest? (some rest-pattern? rv)
                     rvc (count rv)
                     size (if rest? (dec rvc) rvc)]
                (VectorPattern. rv t size n rest? _meta)))]
      [pl pr]))
  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [focr (first ocrs)
          ^VectorPattern fp (ffirst rows)
          [rest? min-size] (->> rows
                                (reduce (fn [[rest? min-size] [p & ps]]
                                          (if (vector-pattern? p)
                                            [(or rest? (.rest? ^VectorPattern p))
                                             (min min-size (.size ^VectorPattern p))]
                                            [rest? min-size]))
                                        [false (.size ^VectorPattern fp)]))
          [nrows nocrs] (if rest?
                          [(->> rows
                                (map (fn [row]
                                       (let [p (first row)
                                             ps (cond
                                                 (vector-pattern? p) (split p min-size)
                                                 :else [(wildcard-pattern) (wildcard-pattern)])]
                                         (reduce prepend (drop-nth-bind row 0 focr) (reverse ps)))))
                                vec)
                           (let [vec-ocr focr
                                 t (.t this)
                                 ocr-meta {:occurrence-type t
                                           :vec-sym vec-ocr}
                                 vl-ocr (gensym (str (name vec-ocr) "_left__"))
                                 vl-ocr (with-meta vl-ocr
                                          (assoc ocr-meta :bind-expr (subvec-inline t (with-tag t vec-ocr) 0 min-size )))
                                 vr-ocr (gensym (str (name vec-ocr) "_right__"))
                                 vr-ocr (with-meta vr-ocr
                                          (assoc ocr-meta :bind-expr (subvec-inline t (with-tag t vec-ocr) min-size)))]
                             (into [vl-ocr vr-ocr] (drop-nth ocrs 0)))]
                          [(->> rows
                                (map (fn [row]
                                       (let [p (first row)
                                             ps (if (vector-pattern? p)
                                                  (reverse (.v ^VectorPattern p))
                                                  (repeatedly min-size wildcard-pattern))]
                                         (reduce prepend (drop-nth-bind row 0 focr) ps))))
                                vec)
                           (let [vec-ocr focr
                                 ocr-sym (fn [i]
                                           (let [ocr (gensym (str (name vec-ocr) "_" i "__"))
                                                 t (.t this)]
                                             (with-meta ocr
                                               {:occurrence-type t
                                                :vec-sym vec-ocr
                                                :index i
                                                :bind-expr (if-let [offset (.offset this)]
                                                             (nth-offset-inline t (with-tag t vec-ocr) i offset)
                                                             (nth-inline t (with-tag t vec-ocr) i))})))]
                             (into (into [] (map ocr-sym (range min-size)))
                                   (drop-nth ocrs 0)))])
          matrix (pattern-matrix nrows nocrs)]
      matrix)))

(defn ^VectorPattern vector-pattern
  ([] (vector-pattern [] ::vector nil nil))
  ([v]
     (vector-pattern v ::vector nil nil))
  ([v t]
     (vector-pattern v t nil nil nil))
  ([v t offset]
     (vector-pattern v t offset nil))
  ([v t offset rest?] {:pre [(vector? v)]}
     (let [c (count v)
           size (if rest? (dec c) c)]
      (VectorPattern. v t size offset rest? nil))))

(defn vector-pattern? [x]
  (instance? VectorPattern x))

(defmethod print-method VectorPattern [^VectorPattern p ^Writer writer]
  (.write writer (str "<VectorPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; Or Patterns

(deftype OrPattern [ps _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (OrPattern. ps new-meta))
  Object
  (toString [this]
    (str ps))
  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [ps (.ps this)
          nrows (->> rows
                     (map (fn [row]
                            (let [p (first row)]
                              ;; NOTE: hmm why can't we remove this - David
                              (if (and (pattern-equals this p)
                                       (not (wildcard-pattern? p)))
                                (map (fn [p]
                                       (update-pattern row 0 p)) ps)
                                [row]))))
                     (apply concat)
                     vec)
          _ (trace-dag "OrPattern specialization")]
      (pattern-matrix nrows ocrs))))

(defn ^OrPattern or-pattern [p]
  {:pre [(vector? p)]}
  (OrPattern. p nil))

(defn or-pattern? [x]
  (instance? OrPattern x))

(defmethod print-method OrPattern [^OrPattern p ^Writer writer]
  (.write writer (str "<OrPattern: " (.ps p) ">")))

;; -----------------------------------------------------------------------------
;; Pseudo-patterns

(defmulti pseudo-pattern? type)

(defmethod pseudo-pattern? OrPattern
  [x] true)

(defmethod pseudo-pattern? :default
  [x] false)

;; -----------------------------------------------------------------------------
;; ## Guard Patterns
;;
;; Guard patterns are used to represent guards on patterns, for example
;;   `(1 :when even?)`
;;

(declare guard-pattern?)

(deftype GuardPattern [p gs _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (GuardPattern. p gs new-meta))
  IPatternCompile
  (to-source* [this ocr]
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))
  Object
  (toString [this]
    (str p " :when " gs))
  ISpecializeMatrix
  (specialize-matrix [this rows ocrs]
    (let [nrows (->> rows
                     (map (fn [row]
                            (let [p (first row)]
                              (if (guard-pattern? p)
                                (let [^GuardPattern p p]
                                  (update-pattern row 0 (.p p)))
                                row))))
                     vec)
          _ (trace-dag "GuardPattern specialization")]
      (pattern-matrix nrows ocrs))))

(defn ^GuardPattern guard-pattern [p gs]
  {:pre [(set? gs)]}
  (GuardPattern. p gs nil))

(defn guard-pattern? [x]
  (instance? GuardPattern x))

(defmethod print-method GuardPattern [^GuardPattern p ^Writer writer]
  (.write writer (str "<GuardPattern " (.p p) " :when " (.gs p) ">")))

;; -----------------------------------------------------------------------------
;; Pattern Comparisons

(defmethod pattern-compare [WildcardPattern WildcardPattern]
  [a b] 0)

;; NOTE: if recur is present we want all objects to equal wildcards, this is
;; because we push the wildcard matches along as well in the matrix specialization
;; since we don't have backtracking in this case - David

(defmethod pattern-compare [Object WildcardPattern]
  [a b] (if *recur-present* 0 1))

(prefer-method pattern-compare [Object WildcardPattern] [LiteralPattern Object])

(defmethod pattern-compare [LiteralPattern Object]
  [a b] 1)

(defmethod pattern-compare [Object LiteralPattern]
  [a b] 1)

(defmethod pattern-compare [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b]
  (let [la (.l a)
        lb (.l b)]
    (cond
     (= la lb) 0
     :else 1)))

(defmethod pattern-compare [GuardPattern GuardPattern]
  [^GuardPattern a ^GuardPattern b] (if (= (.gs a) (.gs b)) 0 1))

(defmethod pattern-compare [GuardPattern WildcardPattern]
  [^GuardPattern a ^WildcardPattern b]
  (let [p (.p a)]
    (if (wildcard-pattern? p)
      (pattern-compare p b) 1)))

(defmethod pattern-compare [OrPattern OrPattern]
  [^OrPattern a ^OrPattern b]
  (let [as (.ps a)
        bs (.ps b)]
    (if (and (= (count as) (count bs))
             (every? identity (map pattern-equals as bs)))
      0 1)))

(defmethod pattern-compare [VectorPattern VectorPattern]
  [^VectorPattern a ^VectorPattern b]
  (if (or (= (.size a) (.size b))
          (and (.rest? a) (<= (.size a) (.size b)))
          (and (.rest? b) (<= (.size b) (.size a))))
    0 1))

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
          (= p '&) (let [p (second ps)
                         rp (if (and (vector? p) (= t :seq))
                              (seq-pattern (emit-patterns p t))
                              (emit-pattern p))]
                     (recur (nnext ps) t (conj v (rest-pattern rp)))) 
          :else (recur (next ps) t (conj v (emit-pattern (first ps)))))))))

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

(declare emit-pattern-for-syntax)
(declare or-pattern)
(declare as-pattern)
(declare guard-pattern)
(declare vector-pattern)

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
         (->> pat
              (map emit-pattern)
              (into []))))

(defmethod emit-pattern-for-syntax [Object :as]
  [[p _ sym]] (with-meta (emit-pattern p) {:as sym}))

(defmethod emit-pattern-for-syntax [Object :when]
  [[p _ gs]] (let [gs (if (not (vector? gs)) [gs] gs)]
              (guard-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax [Object :seq]
  [pat]
  (let [p (first pat)]
    (if (empty? p)
      (literal-pattern ())
      (seq-pattern (emit-patterns p :seq)))))

(defmethod emit-pattern-for-syntax [Object ::vector]
  [[p t offset-key offset]] (let [ps (emit-patterns p :vector)]
                              (vector-pattern ps t offset (some rest-pattern? ps))))

(defmethod emit-pattern-for-syntax [Object :only]
  [[p _ only]] (with-meta (emit-pattern p) {:only only}))

(defmethod emit-pattern-for-syntax :default
  [[_ s :as l]]
  (throw (AssertionError.
          (str "Invalid list syntax " s " in " l ". "
               "Valid syntax: "
               (vec (remove #(= % :default)
                            (keys (.getMethodTable ^clojure.lang.MultiFn emit-pattern-for-syntax))))))))


(let [void (Object.)
      void? #(identical? void %)
      infix-keyword? #(#{:when :as} %)]
  ;; void is a unique placeholder for nothing -- we can't use nil
  ;; because that's a legal symbol in a pattern row
  (defn- regroup-keywords [pattern]
    (cond (vector? pattern)
          (first (reduce (fn [[result p q] r]
                           (cond
                            (void? p) [result q r]
                            (and (not (void? r)) (infix-keyword? q))
                              [(conj result (list (regroup-keywords p) q r)) void void]
                            :else [(conj result (regroup-keywords p)) q r]))
                         [[] void void]
                         (conj pattern void void)))
          (seq? pattern) (cons (regroup-keywords (first pattern)) (rest pattern))
          :else pattern)))

 (defn- group-keywords 
   "Returns a pattern with pattern-keywords (:when and :as) properly grouped.  
    The original pattern may use the 'flattened' syntax.  For example, a 'flattened' 
    pattern row like [a b :when even?] is grouped as [a (b :when even?)]."
  [pattern]
  (if (vector? pattern) (regroup-keywords pattern) pattern))


(defn emit-clause [[pat action]]
  (let [p (into [] (map emit-pattern (group-keywords pat)))]
    (pattern-row p action)))

(defn- wildcards-and-duplicates
  "Returns a vector of two elements: the set of all wildcards and the 
   set of duplicate wildcards.  The underbar _ is excluded from both."
  [patterns]
  (loop [remaining patterns seen #{} dups #{}]
    (if-let [patterns (seq remaining)]
      (let [pat (first patterns)
            pats (rest patterns)]
        (cond (or (= pat '_) (= pat '&)) (recur pats seen dups)
              (symbol? pat) (if (contains? seen pat)
                              (recur pats seen (conj dups pat))
                              (recur pats (conj seen pat) dups))
              (vector? pat) (recur (concat pats pat) seen dups)
              (map? pat) (recur (concat pats (vals pat)) seen dups)
              (seq? pat) (cond
                          (= (first pat) 'quote) (recur pats seen dups)
                          (= (first pat) :or) (let [wds (map wildcards-and-duplicates
                                                             (map list (take-nth 2 pat)))
                                                    mseen (apply set/union (map first wds))]
                                                (recur pats (set/union seen mseen)
                                                       (apply set/union dups
                                                              (set/intersection seen mseen)
                                                              (map second wds))))
                          (= (second pat) :as) (recur (concat pats (take-nth 2 pat)) seen dups)
                          :else (recur (conj pats (first pat)) seen dups))
              :else (recur pats seen dups)))
      [seen dups])))

(defn- find-duplicate-wildcards [pattern]
  (second (wildcards-and-duplicates pattern)))

;; This could be scattered around in other functions to be more efficient
;; Turn off *syntax-check* to disable
(defn- check-matrix-args [vars clauses]
  (cond
   (symbol? vars) (throw (AssertionError.
                          (str "Occurrences must be in a vector."
                               " Try changing " vars " to [" vars "]")))
   (not (vector? vars)) (throw (AssertionError.
                                (str "Occurrences must be in a vector. "
                                     vars " is not a vector"))))

  (letfn [(check-pattern [pat nvars rownum]
           (let [pat (group-keywords pat)]
            (cond 
             (not (vector? pat)) (throw (AssertionError. 
                                         (str "Pattern row " rownum
                                              ": Pattern rows must be wrapped in []."
                                              " Try changing " pat " to [" pat "]." 
                                              (when (list? pat)
                                                (str " Note: pattern rows are not patterns."
                                                     " They cannot be wrapped in a :when guard, for example")))))
             (not= (count pat) nvars)
             (throw (AssertionError.
                     (str "Pattern row " rownum
                          ": Pattern row has differing number of patterns. "
                          pat " has " (count pat) " pattern/s, expecting "
                          nvars " for occurrences " vars))))
            (when-let [duplicates (seq (find-duplicate-wildcards pat))]
              (throw (AssertionError.
                     (str "Pattern row " rownum
                          ": Pattern row reuses wildcards in " pat
                          ".  The following wildcards are ambiguous: " (apply str (interpose ", " duplicates))
                          ".  There's no guarantee that the matched values will be same.  Rename the occurrences uniquely."))))))]
    (let [nvars (count vars)
          cls (partition 2 clauses)]
      (doseq [[[pat _] rownum] (map vector (butlast cls) (rest (range)))]
        (cond
         (= :else pat) (throw (AssertionError.
                               (str "Pattern row " rownum
                                    ": :else form only allowed on final pattern row")))
         :else (check-pattern pat nvars rownum)))
      (when-let [[pat _] (last cls)]
        (when-not (= :else pat)
          (check-pattern pat nvars (count cls))))))

  (when (odd? (count clauses)) 
    (throw (AssertionError. (str "Uneven number of Pattern Rows. The last form `"
                                 (last clauses) "` seems out of place.")))))

;; TODO: more sophisticated analysis that actually checks that recur is
;; not being used as a local binding when it occurs - David

(defn analyze-actions [actions]
  (letfn [(analyze-action [action]
            (if (and (sequential? action)
                     (some '#{recur} (flatten action)))
              {:recur-present true} {}))]
    (map analyze-action actions)))

(defn emit-matrix [vars clauses]
  (let [cs (partition 2 clauses)
        cs (let [[p a] (last cs)]
             (if (= :else p)
               (do (trace-matrix "Convert :else clause to row of wildcards")
                   (conj (vec (butlast cs)) [(->> vars (map (fn [_] '_)) vec) a]))
               (conj (vec cs) [(->> vars (map (fn [_] '_)) vec) nil])))
        clause-sources (into [] (map emit-clause cs))
        vars (vec (map (fn [var]
                         (if (not (symbol? var))
                           (let [nsym (gensym "ocr-")
                                 _ (trace-dag "Bind ocr" var "to" nsym)]
                             (with-meta nsym {:ocr-expr var}))
                           var))
                     vars))]
    (pattern-matrix clause-sources vars)))

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
  (let [[vars clauses] (if (vector? vars)
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
