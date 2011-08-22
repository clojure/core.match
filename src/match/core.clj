(ns match.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.set :as set])
  (:import [java.io Writer]))

(set! *warn-on-reflection* true)

(def ^:dynamic *syntax-check* true)
(def ^:dynamic *line*)
(def ^:dynamic *warned* (atom false))

(prefer-method print-method clojure.lang.IType clojure.lang.ISeq)

(defn warn [msg]
  (if (not @*warned*)
    (do
      (binding [*out* *err*] 
        (println "WARNING:"
                 (str *ns* ", line " *line* ":") 
                 msg))
      (reset! *warned* true))))

(defprotocol IMatchLookup
  (val-at* [this k not-found]))

;; =============================================================================
;; Map Pattern Interop

(extend-type clojure.lang.ILookup
  IMatchLookup
  (val-at* [this k not-found]
    (.valAt this k not-found)))

(defn val-at
  ([m k] (val-at* m k nil))
  ([m k not-found] (val-at* m k not-found)))

;; =============================================================================
;; Vector Pattern Interop

;; NOTE: we might need coercing / wrapper types when we get to
;; open dispatch - David

(definterface IMatchVector
  (^int vcount [])
  (vnth [^int i])
  (vsubvec [^int start ^int end])
  (unwrap []))

(deftype MatchVector [v]
  IMatchVector
  (vcount [this] (count this))
  (vnth [this i] (nth v i))
  (vsubvec [this start end] (subvec this start end))
  (unwrap [_] v))

(defprotocol IMatchVectorType
  (mvector? [this])
  (mvector-coerce* [this]))

(extend-type clojure.lang.IPersistentVector
  IMatchVectorType
  (mvector? [_] true)
  (mvector-coerce* [this] (MatchVector. this)))

(defn ^IMatchVector mvector-coerce [x]
  (mvector-coerce* x))

(extend-type Object
  IMatchVectorType
  (mvector? [_] false))

(defmulti coerce? identity)
(defmulti coerce-element? identity)
(defmulti coerce-element (fn [t & r] t))
(defmulti vtest-inline (fn [t & r] t))
(defmulti vtest-and-size-inline (fn [t & r] t))
(defmulti vcount-inline (fn [t & r] t))
(defmulti vnth-inline (fn [t & r] t))
(defmulti vnth-offset-inline (fn [t & r] t))
(defmulti vsubvec-inline (fn [t & r] t))

(defmethod coerce? :default
  [_] false)
(defmethod coerce-element? :default
  [_] false)
(defmethod vtest-inline ::vector
  [_ ocr] `(vector? ~ocr))
(defmethod vtest-and-size-inline ::vector
  [_ ocr size] `(and (vector? ~ocr) (= (count ~ocr) ~size)))
(defmethod vcount-inline ::vector
  [_ ocr] `(count ~ocr))
(defmethod vnth-inline ::vector
  [_ ocr i] `(nth ~ocr ~i))
(defmethod vnth-offset-inline ::vector
  [_ ocr i offset] `(nth ~ocr (unchecked-add ~i ~offset)))
(defmethod vsubvec-inline ::vector
  [_ ocr start end] `(subvec ~ocr ~start ~end))

;; =============================================================================
;; Extensions and Protocols

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (n-to-clj [this]))

(defprotocol IPatternCompile
  (to-source [this ocr]))

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
  (to-source [this ocr]
    (cond
     (= l ()) `(empty? ~ocr)
     (symbol? l) `(= ~ocr '~l)
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
  (to-source [this ocr]
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
  (to-source [this ocr]
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
  (to-source [this ocr]
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

(deftype VectorPattern [v t size offset rest? _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (VectorPattern. v t size offset rest? new-meta))
  IPatternCompile
  (to-source [_ ocr]
    (if size
      (vtest-and-size-inline t ocr size)
      (vtest-inline t ocr)))
  Object
  (toString [_]
    (str v ":" t)))

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

(def vector-pattern? (partial instance? VectorPattern))

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
  (to-source [this ocr]
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

(defmethod pattern-equals [VectorPattern VectorPattern]
  [^VectorPattern a ^VectorPattern b] (and (= (.t a)  (.t b))
                                           (let [sa (.size a) sb (.size b)]
                                             (or (= sa sb)
                                                 (and (>= sa sb) (.rest? b))))
                                           (= (.offset a) (.offset b))))

(defmethod pattern-equals [MapCrashPattern MapCrashPattern]
  [a b] true)

(defmethod pattern-equals [OrPattern OrPattern]
  [^OrPattern a ^OrPattern b] (let [as (.ps a)
                                    bs (.ps b)]
                                (and (= (count as) (count bs)) ;; TODO: use sets - David
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
    `(throw (Exception. "No match found."))))

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

(defn dag-clause-to-clj [occurrence pattern action]
  (vector (to-source pattern occurrence) 
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
                     (empty? ps))))
            (has-ocr-expr? [ocrs]
              (some (fn [ocr]
                      (-> ocr meta :ocr-expr))
                    ocrs))
            (coerce? [matrix]
              (-> matrix meta :coerce-bind))]
      (cond
       (empty? rows) (do (warn "Non-exhaustive pattern matrix, consider adding :else clause")
                         (fail-node))
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
       :else (let [col (necessary-column this)]
               (if (= col 0)
                 (let [this (reduce specialize this (pseudo-patterns this col))
                       constrs (column-constructors this col)
                       clauses (map (fn [c]
                                      (let [s (-> this 
                                                  (specialize c) 
                                                  compile)]
                                        [c s]))
                                    constrs)
                       default (let [m (specialize this (wildcard-pattern))]
                                 (if-not (empty-matrix? m)
                                   (compile m)
                                   (do (warn (str "Non-exhaustive pattern matrix, " 
                                                  "consider adding :else clause"))
                                       (fail-node))))
                       node (switch-node (ocrs col) clauses default)]
                   (cond
                    (has-ocr-expr? ocrs) (bind-node (mapcat (fn [ocr]
                                                               (if-let [bind-expr (-> ocr meta :ocr-expr)]
                                                                 [ocr bind-expr]
                                                                 [ocr ocr]))
                                                             ocrs)
                                                     node)
                    (coerce? this) (bind-node (-> this meta :coerce-bind)
                                              node)
                    :else node))
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
                      hsym (gensym (str (name seq-sym) "_head__"))
                      hsym (with-meta hsym
                             (assoc sym-meta :bind-expr `(let [~hsym (first ~seq-ocr)])))
                      tsym (gensym (str (name seq-sym) "_tail__"))
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
                                (let [ocr (gensym (str (name map-ocr) "_" (name k) "__"))]
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

;; =============================================================================
;; Vector Pattern Specialization

(extend-type VectorPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          srows (filter #(pattern-equals this (first %)) rows)
          width (reduce (fn [w row]
                          (let [p (first row)]
                            (if (vector-pattern? p)
                              (let [^VectorPattern p p
                                    c (count (.v p))]
                                (min (or w c) c))
                              w))) ;; TODO: smallest width before rest pattern - David
                        nil srows)
          nrows (->> srows
                     (map (fn [row]
                            (let [p (first row)
                                  ps (if (vector-pattern? p)
                                       (reverse (.v ^VectorPattern p))
                                       (repeatedly width wildcard-pattern))]
                              (reduce prepend (drop-nth row 0) ps))))
                     vec)
          nocrs (let [vec-ocr focr
                      ocr-sym (fn [i]
                                (let [ocr (gensym (str (name vec-ocr) "_" i "__"))
                                      t (.t this)]
                                  (with-meta ocr
                                    {:occurrence-type t
                                     :vec-sym vec-ocr
                                     :index i
                                     :bind-expr `(let [~ocr ~(if-let [offset (.offset this)]
                                                               (vnth-offset-inline t focr i offset)
                                                               (vnth-inline t focr i))])})))]
                  (into (into [] (map ocr-sym (range width)))
                        (drop-nth ocrs 0)))
          matrix (pattern-matrix nrows nocrs)]
      (if (coerce? (.t this))
        (with-meta matrix
          {:coerce-bind [focr `(mvector-coerce ~focr)]})
        matrix))))

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
                            (let [p (first row)]
                              (if (guard-pattern? p)
                                (let [^GuardPattern p p]
                                  (update-pattern row 0 (.p p)))
                                row))))
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
             [k (emit-pattern v)]))
      (remove nil?)
      (into {}))))

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

(defmethod emit-pattern-for-syntax :vec
  [[p _ t _ offset]] (vector-pattern (vec (map emit-pattern p))
                                     (or t ::vector) offset))

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
               (conj (vec (butlast cs)) [(->> vars (map (fn [_] '_)) vec) a])
               cs))
        clause-sources (into [] (map emit-clause cs))
        vars (vec (map (fn [var]
                       (if (not (symbol? var))
                         (with-meta (gensym "ocr-") {:ocr-expr var})
                         var))
                     vars))]
    (pattern-matrix clause-sources vars)))

(defmacro defmatch [name vars & clauses]
  (let [clj-form (-> (emit-matrix vars clauses)
                   compile
                   n-to-clj)]
    `(defn ~name ~vars 
       ~clj-form)))

(defmacro match-1 [vars & clauses]
  "Pattern match a single value."
  (binding [*line* (-> &form meta :line)
            *warned* (atom false)]
    (let [[vars clauses] [[vars] (mapcat (fn [[row action]]
                                           (if (not= row :else)
                                             [[row] action]
                                             [row action]))
                                         (partition 2 clauses))]]
      `~(-> (emit-matrix vars clauses)
          compile
          n-to-clj))))

(defmacro match [vars & clauses]
  "Pattern match multiple values."
  (binding [*line* (-> &form meta :line)
            *warned* (atom false)]
    `~(-> (emit-matrix vars clauses)
        compile
        n-to-clj)))

(comment
  ;; how do we deal with rest patterns?, concat is the constructor
  (emit-pattern '([1 2 3] :vector))

  ;; FIXME
  (let [x [1 2 2]
        y 1]
    (match [x y]
      [([_ _ 2] :vector) 1] :a0
      [([1 1 3] :vector) 2] :a1
      [([1 2 3] :vector) 3] :a2
      :else :a3))

  ;; FIXME
  (let [x [1 2 2]
        y 1]
    (match [x]
      [([_ _ 2] :vector)] :a0
      [([1 1 3] :vector)] :a1
      [([1 2 3] :vector)] :a2
      :else :a3))
  )
