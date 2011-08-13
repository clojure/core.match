(ns match.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

(defprotocol IMatchLookup
  (val-at* [this k not-found]))

(extend-type clojure.lang.ILookup
  IMatchLookup
  (val-at* [this k not-found]
    (.valAt this k not-found)))

(defn val-at
  ([m k] (val-at* m k nil))
  ([m k not-found] (val-at* m k not-found)))

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (n-to-clj [this]))

(defprotocol IPatternCompile
  (p-to-clj [this ocr]))

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

;; =============================================================================
;; Patterns

(defmulti pattern-equals (fn [a b] [(type a) (type b)]))
(defmulti pattern-compare (fn [a b] [(type a) (type b)]))

;; -----------------------------------------------------------------------------
;; Wildcard Pattern

(deftype WildcardPattern [sym _meta]
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

(def wildcard-pattern? (partial instance? WildcardPattern))

(defn named-wildcard-pattern? [x]
  (when (instance? WildcardPattern x)
    (not= (.sym ^WildcardPattern x) '_)))

(defmethod print-method WildcardPattern [^WildcardPattern p ^Writer writer]
  (.write writer (str "<WildcardPattern: " (.sym p) ">")))

;; -----------------------------------------------------------------------------
;; Literal Pattern

(deftype LiteralPattern [l _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (LiteralPattern. l new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    (cond
     (= l ()) `(empty? ~ocr)
     :else `(= ~ocr ~l)))
  Object
  (toString [_]
    (if (nil? l)
      "nil"
      (str l))))

(defn ^LiteralPattern literal-pattern [l] 
  (LiteralPattern. l nil))

(def literal-pattern? (partial instance? LiteralPattern))

(defmethod print-method LiteralPattern [^LiteralPattern p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; Seq Pattern

(deftype SeqPattern [s _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (SeqPattern. s new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    `(or (seq? ~ocr) (sequential? ~ocr)))
  Object
  (toString [_]
    (str s)))

(defn ^SeqPattern seq-pattern [s]
  {:pre [(sequential? s)
         (not (empty? s))]}
  (SeqPattern. s nil))

(def seq-pattern? (partial instance? SeqPattern))

(defmethod print-method SeqPattern [^SeqPattern p ^Writer writer]
  (.write writer (str "<SeqPattern: " p ">")))

;; -----------------------------------------------------------------------------
;; Rest Pattern

(deftype RestPattern [p _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (RestPattern. p new-meta))
  Object
  (toString [_]
    p))

(defn ^RestPattern rest-pattern [p]
  (RestPattern. p nil))

(def rest-pattern? (partial instance? RestPattern))

(defmethod print-method RestPattern [^RestPattern p ^Writer writer]
  (.write writer (str "<RestPattern: " (.p p) ">")))

;; -----------------------------------------------------------------------------
;; Map Pattern

(deftype MapPattern [m only _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (MapPattern. m only new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    `(or (map? ~ocr) (satisfies? IMatchLookup ~ocr)))
  Object
  (toString [_]
    (str m " :only " (or only []))))

(defn ^MapPattern map-pattern
  ([] (MapPattern. {} nil nil))
  ([m] {:pre [(map? m)]}
     (MapPattern. m nil nil))
  ([m only] {:pre [(map? m)]}
     (MapPattern. m only nil)))

(def map-pattern? (partial instance? MapPattern))

(defmethod print-method MapPattern [^MapPattern p ^Writer writer]
  (.write writer (str "<MapPattern: " p ">")))

(deftype MapCrashPattern [only _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (MapCrashPattern. only new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    (let [map-sym (-> ocr meta :map-sym)]
      `(= (.keySet ~(with-meta map-sym {:tag java.util.Map})) #{~@only})))
  Object
  (toString [_]
    "CRASH"))

(defn ^MapCrashPattern map-crash-pattern [only]
  (MapCrashPattern. only nil))

(defmethod print-method MapCrashPattern [^MapCrashPattern p ^Writer writer]
  (.write writer (str "<MapCrashPattern>")))

;; -----------------------------------------------------------------------------
;; Or Patterns

(deftype OrPattern [ps _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (OrPattern. ps new-meta))
  Object
  (toString [this]
    (str ps)))

(defn ^OrPattern or-pattern [p]
  {:pre [(vector? p)]}
  (OrPattern. p nil))

(def or-pattern? (partial instance? OrPattern))

(defmethod pattern-equals [OrPattern OrPattern]
  [^OrPattern a ^OrPattern b] (= (.ps a) (.ps b)))

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
;; Guard Patterns

(deftype GuardPattern [p gs _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (GuardPattern. p gs new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    `(and ~@(map list gs (repeat ocr))))
  Object
  (toString [this]
    (str p " :when " gs)))

(defn ^GuardPattern guard-pattern [p gs]
  {:pre [(set? gs)]}
  (GuardPattern. p gs nil))

(def guard-pattern? (partial instance? GuardPattern))

(defmethod pattern-equals [GuardPattern GuardPattern]
  [^GuardPattern a ^GuardPattern b] (= (.gs a) (.gs b)))

(defmethod print-method GuardPattern [^GuardPattern p ^Writer writer]
  (.write writer (str "<GuardPattern " (.p p) " :when " (.gs p) ">")))

;; -----------------------------------------------------------------------------
;; Crash Patterns

(defmulti crash-pattern? type)

(defmethod crash-pattern? MapCrashPattern
  [x] true)

(defmethod crash-pattern? :default
  [x] false)

;; -----------------------------------------------------------------------------
;; constructor?

(defn constructor? [p]
  (not (wildcard-pattern? p)))

;; =============================================================================
;; Pattern Comparison

(defmethod pattern-compare [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b] (compare (.l a) (.l b)))

(defmethod pattern-compare [LiteralPattern Object]
  [a b] -1)

(defmethod pattern-compare [Object LiteralPattern]
  [a b] 1)

(defmethod pattern-compare [GuardPattern GuardPattern]
  [^GuardPattern a ^GuardPattern b] (if (= (.gs a) (.gs b)) 0 -1))

(defmethod pattern-compare :default
  [a b] (if (= (class a) (class b)) 0 -1))

;; =============================================================================
;; Pattern Equality

(defmethod pattern-equals [Object WildcardPattern]
  [a b] true)

(defmethod pattern-equals [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b] (= (.l a) (.l b)))

(defmethod pattern-equals [SeqPattern SeqPattern]
  [a b] true)

(defmethod pattern-equals [RestPattern RestPattern]
  [a b] true)

(defmethod pattern-equals [MapPattern MapPattern]
  [a b] true)

(defmethod pattern-equals [MapCrashPattern MapCrashPattern]
  [a b] true)

(defmethod pattern-equals [OrPattern OrPattern]
  [^OrPattern a ^OrPattern b] (let [as (.ps a)
                                    bs (.ps b)]
                                (and (= (count as) (count bs))
                                     (every? identity (map pattern-equals as bs)))))

(defmethod pattern-equals :default 
  [a b] false)

;; =============================================================================
;; Pattern Rows

(defprotocol IPatternRow
  (action [this])
  (patterns [this])
  (update-pattern [this i p])
  (bindings [this])
  (all-wildcards? [this])
  (drop-nth-bind [this n bind-expr])) ;; TODO: needs better name - David

(declare leaf-bind-expr)

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
                       (conj bindings [(.sym ^WildcardPattern p) bind-expr])
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
    this)
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
;; Compilation Nodes

;; -----------------------------------------------------------------------------
;; Leaf Node

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
  [ocr] (concat (-> ocr meta :bind-expr) `(~ocr)))

(defmethod leaf-bind-expr :map
  [ocr] (let [m (meta ocr)]
            `(val-at ~(:map-sym m) ~(:key m))))

(defmethod leaf-bind-expr :default
  [ocr] ocr)

;; -----------------------------------------------------------------------------
;; Fail Node

(defrecord FailNode []
  INodeCompile
  (n-to-clj [this]
    `(throw (Exception. "Found FailNode"))))

(defn ^FailNode fail-node []
  (FailNode.))

;; -----------------------------------------------------------------------------
;; Switch Node

(defn dag-clause-to-clj [occurrence pattern action]
  (vector (p-to-clj pattern occurrence) 
          (n-to-clj action)))

(defrecord SwitchNode [occurrence cases default]
  INodeCompile
  (n-to-clj [this]
    (let [clauses (mapcat (partial apply dag-clause-to-clj occurrence) cases)
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr (concat `(cond ~@clauses)
                            `(:else ~(n-to-clj default)))]
      (if bind-expr
        (concat bind-expr (list cond-expr))
        cond-expr))))

(defn ^SwitchNode switch-node
  ([occurrence cases default]
   {:pre [(symbol? occurrence)
          (seq? cases)]}
   (SwitchNode. occurrence cases default)))

;; =============================================================================
;; Pattern Matrix

(defn seq-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :seq))

(defn map-occurrence? [ocr]
  (= (-> ocr meta :occurrence-type) :map))

(defprotocol IPatternMatrix
  (width [this])
  (height [this])
  (dim [this])
  (specialize [this c])
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

(defprotocol ISpecializeMatrix
  (specialize-matrix [this matrix]))

(declare empty-matrix?)
(declare useful-p?)
(declare useful?)

(deftype PatternMatrix [rows ocrs]
  IPatternMatrix
  (width [_] (if (not (empty? rows))
               (count (rows 0))
               0))

  (height [_] (count rows))

  (dim [this] [(width this) (height this)])

  (specialize [this p]
    (specialize-matrix p this))

  (column [_ i] (vec (map #(nth % i) rows)))

  (compile [this]
    (letfn [(column-constructors [this i]
              (->> (column this i)
                   (filter (comp not wildcard-pattern?))
                   (apply sorted-set-by (fn [a b] (pattern-compare a b)))))
            (pseudo-patterns [this i]
              (->> (column this i)
                   (filter pseudo-pattern?)))
            (empty-row? [row]
              (let [ps (patterns row)] ;; TODO: cleanup
                (and (not (nil? ps))
                     (empty? ps))))]
      (cond
       (empty? rows) (fail-node)
       (empty-row? (first rows)) (let [f (first rows)]
                                   (leaf-node (action f) (bindings f)))
       (all-wildcards? (first rows)) (let [^PatternRow f (first rows)
                                           ps (.ps f)
                                           wc-syms (map #(.sym ^WildcardPattern %) ps)
                                           wc-bindings (map vector wc-syms
                                                            (map leaf-bind-expr ocrs))]
                                       (leaf-node (action f)
                                                  (concat (bindings f)
                                                          wc-bindings)))
       :else (let [col (cond
                        (seq-occurrence? ocrs) 0 ;; TODO: don't hardcode - David
                        :else (necessary-column this))]
                (if (= col 0)
                  (let [this (reduce specialize this (pseudo-patterns this col))
                        constrs (column-constructors this col)
                        default (let [m (specialize this (wildcard-pattern))]
                                  (if-not (empty-matrix? m)
                                    (compile m)
                                    (fail-node)))]
                    (switch-node
                     (ocrs col)
                     (map (fn [c]
                            (let [s (-> this 
                                        (specialize c) 
                                        compile)]
                              [c s]))
                          constrs)
                     default))
                  (compile (swap this col)))))))

  (pattern-at [_ i j] ((rows j) i))

  (row [_ j] (nth rows j))

  (necessary-column [this]
    (letfn [(score-column [i col]
              (cond
               (some #{::crash} col) [i -1]
               :else [i (reduce (fn [score useful]
                                  (if useful
                                    (clojure.core/inc score)
                                    score))
                                0 col)]))]
      (first
       (->> (apply map vector (useful-matrix this))
            (map-indexed score-column)
            (reduce (fn [[col score :as curr]
                         [ocol oscore :as cand]]
                      (if (> oscore score) cand curr))
                    [0 -2]))))) ;; NOTE: -2 because -1 is for crash columns - David

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
                    ocrs))

  (insert-rows [_ i rows]
    (PatternMatrix. (into (into (subvec rows 0 i) rows) (subvec rows i))
                    ocrs))

  (occurrences [_] ocrs)

  (action-for-row [_ j]
    (action (rows j)))

  IVecMod
  (drop-nth [_ i]
    (PatternMatrix. (vec (map #(drop-nth % i) rows)) ocrs))

  (swap [_ idx]
    (PatternMatrix. (vec (map #(swap % idx) rows))
                    (swap ocrs idx))))

(defn ^PatternMatrix pattern-matrix [rows ocrs]
  {:pre [(vector rows) 
         (vector ocrs)]}
  (PatternMatrix. rows ocrs))

(defn empty-matrix? [pm]
  (= (dim pm) [0 0]))

(defn useful-p? [pm i j]
  (let [p (pattern-at pm i j)]
   (cond
    (crash-pattern? p) ::crash
    (constructor? p) (every? #(not (wildcard-pattern? %))
                             (take j (column pm i)))
    (wildcard-pattern? p) (not (useful? (drop-nth pm i) j))
    :else false)))

(defn useful? [pm j]
  (some #(useful-p? pm % j)
        (range (count (row pm j)))))

;; =============================================================================
;; Default Matrix Specialization

(extend-type Object
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          nrows (->> rows
                     (filter #(pattern-equals this (first %)))
                     (map #(drop-nth-bind % 0 focr))
                     vec)
          nocrs (drop-nth ocrs 0)]
      (pattern-matrix nrows nocrs))))

;; =============================================================================
;; Seq Pattern Matrix Specialization

;; NOTE: we can handle degenerate (& rest) pattern in the emit-pattern logic - David

(extend-type SeqPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          srows (filter #(pattern-equals this (first %)) rows)
          nrows (->> srows
                     (map (fn [row]
                            (let [^SeqPattern p (first row)
                                  [h & t] (.s p)
                                  t (cond
                                     (empty? t) (literal-pattern ())
                                     (rest-pattern? (first t)) (.p ^RestPattern (first t))
                                     :else (seq-pattern t))]
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      [t h]))))
                     vec)
          nocrs (let [seq-ocr focr
                      seq-sym (or (-> seq-ocr meta :seq-sym) seq-ocr)
                      sym-meta {:occurrence-type :seq
                                :seq-sym seq-ocr}
                      hsym (gensym (str (name seq-sym) "-head-"))
                      hsym (with-meta hsym
                             (assoc sym-meta :bind-expr `(let [~hsym (first ~seq-ocr)])))
                      tsym (gensym (str (name seq-sym) "-tail-"))
                      tsym (with-meta tsym
                             (assoc sym-meta :bind-expr `(let [~tsym (rest ~seq-ocr)])))]
                  (into [hsym tsym] (drop-nth ocrs 0)))]
      (pattern-matrix nrows nocrs))))

;; =============================================================================
;; Map Pattern Matrix Specialization

(extend-type MapPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          srows (filter #(pattern-equals this (first %)) rows)
          all-keys (->> srows
                        (map (fn [row]
                               (let [^MapPattern p (first row)]
                                 [(set (keys (set/map-invert (.m p))))
                                  (set (.only p))])))
                        (reduce concat)
                        (reduce set/union #{})
                        sort) ;; NOTE: this assumes keys are of a heterogenous type, can't sort #{1 :a} - David
          wcs (repeatedly wildcard-pattern)
          wc-map (zipmap all-keys wcs)
          nrows (->> srows
                     (map (fn [row]
                            (let [^MapPattern p (first row)
                                  m (set/map-invert (.m p))
                                  [crash-map wc-map] (if-let [only (.only p)]
                                                       [(zipmap all-keys
                                                                (repeat (map-crash-pattern only)))
                                                        (zipmap only wcs)]
                                                       [{} wc-map])]
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      (reverse
                                       (map second
                                            (sort (merge crash-map wc-map m))))))))
                     vec)
          nocrs (let [map-ocr focr
                      ocr-sym (fn ocr-sym [k]
                                (let [ocr (gensym (str (name map-ocr) "-" (name k)))]
                                  (with-meta ocr
                                    {:occurrence-type :map
                                     :key k
                                     :map-sym map-ocr
                                     :bind-expr `(let [~ocr (val-at ~map-ocr ~k)])})))]
                  (into (into [] (map ocr-sym all-keys))
                        (drop-nth ocrs 0)))]
      (pattern-matrix nrows nocrs))))


(extend-type MapCrashPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          nrows (->> rows
                     (filter #(pattern-equals this (first %)))
                     (map #(drop-nth % 0))
                     vec)]
      (if (empty? nrows)
        (pattern-matrix [] [])
        (let [row (first nrows)]
         (pattern-matrix [(pattern-row [] (action row) (bindings row))] []))))))

;; ==============================================================================
;; Or Pattern Specialization

(extend-type OrPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [ps (.ps this)
          nrows (->> (rows matrix)
                     (map (fn [row]
                            (if (pattern-equals this (first row))
                              (map (fn [p]
                                     (update-pattern row 0 p)) ps)
                              [row])))
                     (apply concat)
                     vec)]
      (pattern-matrix nrows (occurrences matrix)))))

;; =============================================================================
;; Guard Pattern Specialization

(extend-type GuardPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [nrows (->> (rows matrix)
                     (filter #(pattern-equals this (first %)))
                     (map (fn [row]
                            (let [^GuardPattern p (first row)]
                             (update-pattern row 0 (.p p)))))
                     vec)]
      (pattern-matrix nrows (occurrences matrix)))))

;; =============================================================================
;; Interface

(defmulti emit-pattern class)

(defmethod emit-pattern clojure.lang.IPersistentVector
  [pat]
  (if (empty? pat)
    (literal-pattern ())
    (seq-pattern
      (loop [ps pat v []]
        (if (nil? ps)
          v
          (let [p (first ps)]
            (cond
              (= p '&) (let [p (second ps)]
                         (recur (nnext ps) (conj v (rest-pattern (emit-pattern p)))))
              :else (recur (next ps) (conj v (emit-pattern (first ps)))))))))))

(defmethod emit-pattern clojure.lang.IPersistentMap
  [pat]
  (map-pattern
    (->> pat
      (map (fn [[k v]]
             (when (not= k :only)
               [(emit-pattern k) v])))
      (remove nil?)
      (into {}))
    (:only pat)))

(defmethod emit-pattern clojure.lang.Symbol
  [pat]
  (wildcard-pattern pat))

(defmethod emit-pattern :default
  [pat]
  (literal-pattern pat))

(declare emit-pattern-for-syntax)
(declare or-pattern)
(declare as-pattern)
(declare guard-pattern)
(declare vector-pattern)

(defmethod emit-pattern clojure.lang.ISeq
  [pat] (emit-pattern-for-syntax pat))

(defmulti emit-pattern-for-syntax (fn [syn] (second syn)))

(defmethod emit-pattern-for-syntax '|
  [pat] (or-pattern
         (->> pat
              (remove '#{|})
              (map emit-pattern)
              (into []))))

(defmethod emit-pattern-for-syntax :as
  [[p _ sym]] (with-meta (emit-pattern p) {:as sym}))

(defmethod emit-pattern-for-syntax :when
  [[p _ gs]] (let [gs (if (not (vector? gs)) [gs] gs)]
              (guard-pattern (emit-pattern p) (set gs))))

(defmethod emit-pattern-for-syntax :vector
  [p])

(defn emit-clause [[pat action]]
  (let [p (into [] (map emit-pattern pat))]
    (pattern-row p action)))

(defn emit-matrix [vars clauses]
  (let [cs (partition 2 clauses)
        clause-sources (into [] (map emit-clause cs))]
    (pattern-matrix clause-sources vars)))

(defmacro defmatch [name vars & clauses]
  (let [clj-form (-> (emit-matrix vars clauses)
                   compile
                   n-to-clj)]
    `(defn ~name ~vars 
       ~clj-form)))

(defmacro match [vars & clauses]
  `~(-> (emit-matrix vars clauses)
      compile
      n-to-clj))