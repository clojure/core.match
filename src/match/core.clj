(ns match.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

;; ============================================
;; Debugging tools

(def ^{:dynamic true} *syntax-check* true)
(def ^{:dynamic true} *line*)
(def ^{:dynamic true} *locals*)
(def ^{:dynamic true} *warned*)
(def ^{:dynamic true} *trace* (atom false))

(defn set-trace! []
  (reset! *trace* true))
(defn no-trace! []
  (reset! *trace* nil))

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

;; ==================================
;; Protocols

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
     (and (symbol? l) (not (-> l meta :local))) `(= ~ocr '~l)
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

(deftype MapPattern [m _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (MapPattern. m new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    `(or (instance? clojure.lang.ILookup ~ocr) (satisfies? IMatchLookup ~ocr)))
  Object
  (toString [_]
    (str m " :only " (or (:only _meta) []))))

(defn ^MapPattern map-pattern
  ([] (MapPattern. {} nil))
  ([m] {:pre [(map? m)]}
     (MapPattern. m nil)))

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
    `(and ~@(map (fn [expr ocr]
                   (list expr ocr))
                 gs (repeat ocr))))
  Object
  (toString [this]
    (str p " :when " gs)))

(defn ^GuardPattern guard-pattern [p gs]
  {:pre [(set? gs)]}
  (GuardPattern. p gs nil))

(def guard-pattern? (partial instance? GuardPattern))

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

(defn pattern-equals [a b]
  (zero? (pattern-compare a b)))

(defmethod pattern-compare [Object WildcardPattern]
  [a b] 0)

(defmethod pattern-compare [LiteralPattern Object]
  [a b] -1)

(prefer-method pattern-compare [Object WildcardPattern] [LiteralPattern Object])

(defmethod pattern-compare [Object LiteralPattern]
  [a b] 1)

(defmethod pattern-compare [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b]
  (let [la (.l a)
        lb (.l b)]
    (cond
     (= la lb) 0
     (symbol? la) 1
     (symbol? lb) -1
     :else (compare la lb))))

(defmethod pattern-compare [GuardPattern GuardPattern]
  [^GuardPattern a ^GuardPattern b] (if (= (.gs a) (.gs b)) 0 -1))

(defmethod pattern-compare [OrPattern OrPattern]
  [^OrPattern a ^OrPattern b] (let [as (.ps a)
                                    bs (.ps b)]
                                (if (and (= (count as) (count bs))
                                         (every? identity (map pattern-equals as bs)))
                                  0 -1)))

(defmethod pattern-compare :default
  [a b] (if (= (class a) (class b)) 0 -1))

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
  [ocr] (doall (concat (-> ocr meta :bind-expr) `(~ocr))))

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
    (if @*trace*
      `(throw (Exception. (str "No match found. " 
                               "Followed " @*rt-branches* " branches."
                               " Breadcrumbs: " @*rt-breadcrumbs*)))
      `(throw (Exception. (str "No match found."))))))

(defn ^FailNode fail-node []
  (FailNode.))

;; -----------------------------------------------------------------------------
;; Bind Node

(defrecord BindNode [bindings node]
  INodeCompile
  (n-to-clj [this]
    `(let [~@bindings]
       ~(n-to-clj node))))

(defn ^BindNode bind-node [bindings node]
  (BindNode. bindings node))

;; -----------------------------------------------------------------------------
;; Switch Node

(def ^{:dynamic true} *rt-branches*)
(def ^{:dynamic true} *rt-breadcrumbs*)
(declare to-source)

(defn rt-branches [test]
  (if @*trace*
   `(if ~test
      (do (swap! *rt-branches* clojure.core/inc)
          (swap! *rt-breadcrumbs* #(conj % '~test))
          true)
      false)
   test))

(defn dag-clause-to-clj [occurrence pattern action]
  (vector (rt-branches
            (if (extends? IPatternCompile (class pattern))
              (p-to-clj pattern occurrence) 
              (to-source pattern occurrence)))
          (n-to-clj action)))


(defrecord SwitchNode [occurrence cases default]
  INodeCompile
  (n-to-clj [this]
    (let [clauses (doall (mapcat (partial apply dag-clause-to-clj occurrence) cases))
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr (doall (concat `(cond ~@clauses)
                                   `(:else ~(n-to-clj default))))]
      (if bind-expr
        (doall (concat bind-expr (list cond-expr)))
        cond-expr))))

(defn ^SwitchNode switch-node
  ([occurrence cases default]
   {:pre [(seq? cases)]}
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
       (empty? rows) (do (warn "Non-exhaustive pattern matrix, consider adding :else clause")
                         (trace-dag "No rows left, add fail-node")
                         (fail-node))
       (empty-row? (first rows)) (let [f (first rows)
                                       a (action f)
                                       b (bindings f)
                                       _ (trace-dag "Empty row, add leaf-node."
                                                    "Action:" a
                                                    "Bindings:" b)]
                                   (leaf-node a b))
       (all-wildcards? (first rows)) (let [^PatternRow f (first rows)
                                           ps (.ps f)
                                           wc-syms (map #(.sym ^WildcardPattern %) ps)
                                           wc-bindings (map vector wc-syms
                                                            (map leaf-bind-expr ocrs))
                                           a (action f)
                                           b (concat (bindings f)
                                                     wc-bindings)
                                           _ (trace-dag "First row all wildcards, add leaf-node.")]
                                       (leaf-node a b))
       :else (let [col (necessary-column this)
                   _ (trace-dag "Pick column" col "as necessary column.")]
               (if (= col 0)
                 (let [this (reduce specialize this (pseudo-patterns this col))
                       constrs (column-constructors this col)
                       _ (trace-dag "Column" col ":" constrs)
                       clauses (map (fn [c]
                                      (let [s (-> this 
                                                  (specialize c) 
                                                  compile)]
                                        [c s]))
                                    constrs)
                       default (let [m (specialize this (wildcard-pattern))]
                                 (if-not (empty-matrix? m)
                                   (do (trace-dag "Add specialized matrix on row of wildcards as default matrix for next node")
                                       (compile m))
                                   (do (warn (str "Non-exhaustive pattern matrix, " 
                                                  "consider adding :else clause"))
                                       (trace-dag "Add fail-node as default matrix for next node (specialized matrix empty)")
                                       (fail-node))))]
                   (if (some (fn [ocr] (-> ocr meta :ocr-expr)) ocrs)
                     (let [b (mapcat (fn [ocr]
                                       (let [bind-expr (get (meta ocr) :ocr-expr ::not-found)]
                                         (if (not= bind-expr ::not-found)
                                           [ocr bind-expr]
                                           [ocr ocr])))
                                     ocrs)
                           o (ocrs col)
                           n (switch-node o clauses default)
                           _ (trace-dag "Add bind-node on occurance " o ", bindings" b)]
                       (bind-node b n))
                     (let [o (ocrs col)
                           _ (trace-dag "Add switch-node on occurance " o)]
                       (switch-node o clauses default))))
                 (do (trace-dag "Swap column " col)
                     (compile (swap this col))))))))

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
          nocrs (drop-nth ocrs 0)
          _ (trace-dag "Perform default matrix specialization on ocr" focr
                       ", new num ocrs: " 
                       (count ocrs) "->" (count nocrs))]
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
                            (let [p (first row)
                                  [h t] (if (seq-pattern? p)
                                          (let [^SeqPattern p p
                                                [h & t] (.s p)
                                                t (cond
                                                   (empty? t) (literal-pattern ())
                                                   (rest-pattern? (first t)) (.p ^RestPattern (first t))
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
                      hsym (gensym (str (name seq-sym) "-head-"))
                      hsym (with-meta hsym
                             (assoc sym-meta :bind-expr `(let [~hsym (first ~seq-ocr)])))
                      tsym (gensym (str (name seq-sym) "-tail-"))
                      tsym (with-meta tsym
                             (assoc sym-meta :bind-expr `(let [~tsym (rest ~seq-ocr)])))]
                  (into [hsym tsym] (drop-nth ocrs 0)))
          _ (trace-dag "SeqPattern specialization on ocr " focr
                       ", new num ocrs" 
                       (count ocrs) "->" (count nocrs))]
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
                        (remove (comp wildcard-pattern? first))
                        (map (fn [row]
                               (let [^MapPattern p (first row)]
                                 [(set (keys (.m p)))
                                  (set (-> p meta :only))])))
                        (reduce concat)
                        (reduce set/union #{})
                        sort) ;; NOTE: this assumes keys are of a homogenous type, can't sort #{1 :a} - David
          wcs (repeatedly wildcard-pattern)
          wc-map (zipmap all-keys wcs)
          nrows (->> srows
                     (map (fn [row]
                            (let [p (first row)
                                  ocr-map (if (map-pattern? p)
                                            (let [^MapPattern p p
                                                  m (.m p)
                                                  [crash-map wc-map] (if-let [only (-> p meta :only)]
                                                                       [(zipmap all-keys
                                                                                (repeat (map-crash-pattern only)))
                                                                        (zipmap only wcs)]
                                                                       [{} wc-map])]
                                              (merge crash-map wc-map m))
                                            wc-map)]
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      (reverse (map second (sort ocr-map)))))))
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
                        (drop-nth ocrs 0)))
          _ (trace-dag "MapPattern specialization")]
      (pattern-matrix nrows nocrs))))


(extend-type MapCrashPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          nrows (->> rows
                     (filter #(pattern-equals this (first %)))
                     (map #(drop-nth % 0))
                     vec)
          _ (trace-dag "MapCrashPattern specialization")]
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
                            (let [p (first row)]
                              (if (and (pattern-equals this p)
                                       (not (wildcard-pattern? p)))
                                (map (fn [p]
                                       (update-pattern row 0 p)) ps)
                                [row]))))
                     (apply concat)
                     vec)
          _ (trace-dag "OrPattern specialization")]
      (pattern-matrix nrows (occurrences matrix)))))

;; =============================================================================
;; Guard Pattern Specialization

(extend-type GuardPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [nrows (->> (rows matrix)
                     (filter #(pattern-equals this (first %)))
                     (map (fn [row]
                            (let [p (first row)]
                              (if (guard-pattern? p)
                                (let [^GuardPattern p p]
                                  (update-pattern row 0 (.p p)))
                                row))))
                     vec)
          _ (trace-dag "GuardPattern specialization")]
      (pattern-matrix nrows (occurrences matrix)))))

;; =============================================================================
;; Interface

(defmulti to-source (fn [pattern ocr] (type pattern)))

(defmulti emit-pattern class)

;; ============================================================================
;; emit-pattern Methods

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
             [k (emit-pattern v)]))
      (remove nil?)
      (into {}))))

(defmethod emit-pattern clojure.lang.Symbol
  [pat]
  (if (get *locals* pat)
    (literal-pattern (vary-meta pat assoc :local true))
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
                 (symbol? (second pat)))
          (literal-pattern (second pat))
          (emit-pattern-for-syntax pat)))

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

(defmethod emit-pattern-for-syntax :only
  [[p _ only]] (with-meta (emit-pattern p) {:only only}))

(defmethod emit-pattern-for-syntax :default
  [[_ s :as l]]
  (throw (AssertionError.
          (str "Invalid list syntax " s " in " l ". "
               "Valid syntax: "
               (vec (remove #(= % :default)
                            (keys (.getMethodTable emit-pattern-for-syntax))))))))

(defn emit-clause [[pat action]]
  (let [p (into [] (map emit-pattern pat))]
    (pattern-row p action)))

;; This could be scattered around in other functions to be more efficient
;; Turn off *syntax-check* to disable
(defn- check-matrix-args [vars clauses]
  (cond
   (symbol? vars) (throw (AssertionError.
                          (str "Occurances must be in a vector."
                               " Try changing " vars " to [" vars "]")))
   (not (vector? vars)) (throw (AssertionError.
                                (str "Occurances must be in a vector. "
                                     vars " is not a vector"))))

  (letfn [(check-pattern [pat nvars rownum]
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
                          nvars " for occurances " vars)))))]

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


(defn emit-matrix [vars clauses]
  (when *syntax-check* (check-matrix-args vars clauses))
  (let [cs (partition 2 clauses)
        cs (let [[p a] (last cs)]
             (if (= :else p)
               (do (trace-matrix "Convert :else clause to row of wildcards")
                   (conj (vec (butlast cs)) [(->> vars (map (fn [_] '_)) vec) a]))
               cs))
        clause-sources (into [] (map emit-clause cs))
        vars (vec (map (fn [var]
                         (if (not (symbol? var))
                           (let [nsym (gensym "ocr-")
                                 _ (trace-dag "Bind ocr" var "to" nsym)]
                             (with-meta nsym {:ocr-expr var}))
                           var))
                     vars))]
    (pattern-matrix clause-sources vars)))

(defn add-prefix [form]
  (if @*trace*
   `(binding [*rt-branches* (atom 0)
              *rt-breadcrumbs* (atom [])]
      ~form)
   form))

(defn executable-form [node]
  (-> (n-to-clj node)
      add-prefix))

(defn clj-form [vars clauses]
  (-> (emit-matrix vars clauses)
      compile
      executable-form))

;; ============================================================================
;; Match macros

(defmacro defmatch [name vars & clauses]
  `(defn ~name ~vars 
     ~(clj-form vars clauses)))

(defmacro match-1 [vars & clauses]
  "Pattern match a single value."
  (binding [*line* (-> &form meta :line)
            *locals* &env
            *warned* (atom false)]
    (let [[vars clauses] [[vars] (mapcat (fn [[row action]]
                                           (if (not= row :else)
                                             [[row] action]
                                             [row action]))
                                         (partition 2 clauses))]]
      `~(clj-form vars clauses))))

(defmacro match [vars & clauses]
  "Pattern match multiple values."
  (binding [*line* (-> &form meta :line)
            *locals* &env
            *warned* (atom false)]
    (let [src (clj-form vars clauses)]
      `~src)))
