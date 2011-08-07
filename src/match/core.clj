(ns match.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.logic.minikanren :exclude [swap]]
        [clojure.core.logic prelude])
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.pprint :as pp]
            [clojure.set :as set])
  (:import [java.io Writer]))

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (to-clj [this]))

(defprotocol IPatternCompile
  (p-to-clj [this ocr]))

(defn source-pprint [source]
  (binding [pp/*print-pprint-dispatch* pp/code-dispatch
            pp/*print-suppress-namespaces* true]
    (pprint source)))

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

(deftype WildcardPattern [sym]
  java.lang.Comparable
  (compareTo [this that]
    1000)
  Object
  (toString [_]
    (str sym)))

(deftype LiteralPattern [l]
  IPatternCompile
  (p-to-clj [this ocr]
    (let [m (-> ocr meta)]
      (if (:seq-occurrence m)
        (let [seq-sym (-> m :seq-sym
                          name
                          symbol)]
         `(and (not (nil? ~seq-sym))
               (= ~ocr ~l)))
            `(= ~ocr ~l))))
  java.lang.Comparable
  (compareTo [this that] ;; TODO clean up this garbage, implements comparable so we can give to (sorted-set)
    (if (instance? LiteralPattern that)
      (let [tl (.l ^LiteralPattern that)]
        (cond 
         (and (not= l tl)) -1
         (and (nil? l) (nil? tl)) 0
         (nil? l) -1
         (nil? tl) 1
         :else (compare l tl)))
      -1000))
  Object
  (toString [_]
    (if (nil? l)
      "nil"
      (str l))))

(deftype TypePattern [t]
  IPatternCompile
  (p-to-clj [this ocr]
    `(instance? ~t ~ocr))
  java.lang.Comparable
  (compareTo [this that]
    (if (instance? TypePattern that)
      (compare (hash t) (hash (.t ^TypePattern that))) ;; NOTE: see note about inheritance below. pontential hash collisions? - David
      -100))
  Object
  (toString [_]
    (str t)))

(declare wildcard-pattern literal-pattern)

(deftype SeqPattern [s]
  IPatternCompile
  (p-to-clj [this ocr]
    `(sequential? ~ocr))
  java.lang.Comparable
  (compareTo [_ that]
    (if (instance? SeqPattern that)
      0
      -200))
  Object
  (toString [_]
    (str s)))

(deftype RestPattern [p]
  java.lang.Comparable
  (compareTo [this that]
    -1999)
  Object
  (toString [_]
    "REST"))

(deftype SeqCrashPattern []
  IPatternCompile
  (p-to-clj [this ocr]
    (let [seq-sym (-> ocr meta :seq-sym)]
      `(= ~seq-sym nil)))
  java.lang.Comparable
  (compareTo [this that]
    -2000)
  Object
  (toString [_]
    "CRASH"))

(deftype MapPattern [m only]
  IPatternCompile
  (p-to-clj [this ocr]
    `(map? ~ocr))
  java.lang.Comparable
  (compareTo [_ that]
    (if (instance? MapPattern that)
      0
      -101))
  Object
  (toString [_]
    (str m " :only " (or only []))))

(deftype MapCrashPattern [only]
  IPatternCompile
  (p-to-clj [this ocr]
    (let [map-sym (-> ocr meta :map-sym)]
      `(= (set (keys ~map-sym)) (set [~@only]))))
  java.lang.Comparable
  (compareTo [this that]
    -3000)
  Object
  (toString [_]
    "CRASH"))

(defn ^WildcardPattern wildcard-pattern
  ([] (WildcardPattern. '_))
  ([sym] (WildcardPattern. sym)))
  
(defn ^SeqCrashPattern seq-crash-pattern []
  (SeqCrashPattern.))

(defn ^RestPattern rest-pattern [p]
  (RestPattern. p))

(defn ^LiteralPattern literal-pattern [l] 
  (LiteralPattern. l))

(defn ^TypePattern type-pattern [t] 
  {:pre [(class? t)]}
  (TypePattern. t))

(defn ^SeqPattern seq-pattern
  ([] (SeqPattern. ()))
  ([s]
     {:pre [(sequential? s)]}
     (SeqPattern. s)))

(defn ^MapPattern map-pattern
  ([] (MapPattern. {} nil))
  ([m] {:pre [(map? m)]}
     (MapPattern. m nil))
  ([m only] {:pre [(map? m)]}
     (MapPattern. m only)))

(defn ^MapCrashPattern map-crash-pattern [only]
  (MapCrashPattern. only))

(def wildcard-pattern? (partial instance? WildcardPattern))
(defn named-wildcard-pattern? [x]
  (when (instance? WildcardPattern x)
    (not= (.sym ^WildcardPattern x) '_)))
(def literal-pattern? (partial instance? LiteralPattern))
(def type-pattern?    (partial instance? TypePattern))
(def seq-pattern?     (partial instance? SeqPattern))
(def seq-crash-pattern?   (partial instance? SeqCrashPattern))
(def rest-pattern?    (partial instance? RestPattern))
(def map-pattern?     (partial instance? MapPattern))

(defmulti pattern-equals (fn [a b] [(type a) (type b)]))

(defmethod pattern-equals [Object WildcardPattern]
  [a b] true)

(defmethod pattern-equals [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b] (= (.l a) (.l b)))

(defmethod pattern-equals [SeqPattern SeqPattern]
  [a b] true)

(defmethod pattern-equals [SeqCrashPattern SeqCrashPattern]
  [a b] true)

(defmethod pattern-equals [RestPattern RestPattern]
  [a b] true)

(defmethod pattern-equals [MapPattern MapPattern]
  [a b] true)

(defmethod pattern-equals [MapCrashPattern MapCrashPattern]
  [a b] true)

(defmethod pattern-equals [TypePattern TypePattern]
  [^TypePattern a ^TypePattern b] (= (.t a) (.t b)))

(defmethod pattern-equals :default 
  [a b] false)

(defmulti crash-pattern? type)

(defmethod crash-pattern? SeqCrashPattern
  [x] true)

(defmethod crash-pattern? MapCrashPattern
  [x] true)

(defmethod crash-pattern? :default
  [x] false)

(defmethod print-method WildcardPattern [^WildcardPattern p ^Writer writer]
  (.write writer (str "<WildcardPattern: " (.sym p) ">")))

(defmethod print-method LiteralPattern [^LiteralPattern p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

(defmethod print-method TypePattern [^TypePattern p ^Writer writer]
  (.write writer (str "<TypePattern: " p ">")))

(defmethod print-method SeqPattern [^SeqPattern p ^Writer writer]
  (.write writer (str "<SeqPattern: " p ">")))

(defmethod print-method SeqCrashPattern [^SeqCrashPattern p ^Writer writer]
  (.write writer "<SeqCrashPattern>"))

(defmethod print-method RestPattern [^RestPattern p ^Writer writer]
  (.write writer (str "<RestPattern: " (.p p) ">")))

(defmethod print-method MapPattern [^MapPattern p ^Writer writer]
  (.write writer (str "<MapPattern: " p ">")))

(defmethod print-method MapCrashPattern [^MapCrashPattern p ^Writer writer]
  (.write writer (str "<MapCrashPattern>")))

(defn constructor? [p]
  (not (wildcard-pattern? p)))

(declare useful-p?)
(declare useful?)

(defprotocol IPatternRow
  (action [this])
  (patterns [this])
  (bindings [this])
  (all-wildcards? [this])
  (drop-nth-bind [this n bind-expr])) ;; TODO: needs better name - David

(declare map-occurrence?)
(declare leaf-bind-expr)

(deftype PatternRow [ps action bindings]
  IPatternRow
  (action [_] action)
  (patterns [_] ps)
  (bindings [_] bindings)
  (all-wildcards? [this]
    (every? wildcard-pattern? ps))
  (drop-nth-bind [this n ocr]
    (let [p (ps n)]
      (if (named-wildcard-pattern? p)
        (let [sym (.sym ^WildcardPattern p)
              binding [sym (leaf-bind-expr ocr)]]
          (PatternRow. (drop-nth ps n) action
                       (conj (or bindings [])
                             binding)))
        (drop-nth this n))))
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
      (PatternRow. nps action bindings)))
  (more [_]
    (if (empty? ps)
      '()
      (let [nps (rest ps)]
        (PatternRow. nps action bindings))))
  (count [_]
    (count ps))
  clojure.lang.IFn
  (invoke [_ n]
    (nth ps n))
  clojure.lang.IPersistentCollection
  (cons [_ x]
    (PatternRow. (conj ps x) action bindings)))

(defn ^PatternRow pattern-row
  ([ps action] (PatternRow. ps action nil))
  ([ps action bindings] (PatternRow. ps action bindings)))

(defrecord LeafNode [value bindings]
  INodeCompile
  (to-clj [this]
    (if (not (empty? bindings))
      (let [bindings (remove (fn [[sym _]] (= sym '_))
                             bindings)]
       `(let [~@(apply concat bindings)]
          ~value))
      value)))

(defrecord FailNode []
  INodeCompile
  (to-clj [this]
    `(throw (Exception. "Found FailNode"))))

(defn dag-clause-to-clj [occurrence pattern action]
  (vector (p-to-clj pattern occurrence) 
          (to-clj action)))

(defrecord SwitchNode [occurrence cases default]
  INodeCompile
  (to-clj [this]
    (let [clauses (mapcat (partial apply dag-clause-to-clj occurrence) cases)
          bind-expr (-> occurrence meta :bind-expr)
          cond-expr (concat `(cond ~@clauses)
                            `(:else ~(to-clj default)))]
      (if bind-expr
        (concat bind-expr (list cond-expr))
        cond-expr))))

(defn ^LeafNode leaf-node
  ([value] (LeafNode. value []))
  ([value bindings] (LeafNode. value bindings)))

(defn ^FailNode fail-node []
  (FailNode.))

(defn ^SwitchNode switch-node
  ([occurrence cases]
     (SwitchNode. occurrence cases (fail-node)))
  ([occurrence cases default]
     (SwitchNode. occurrence cases default)))

(defn seq-occurrence? [ocr]
  (-> ocr meta :seq-occurrence))

(defn map-occurrence? [ocr]
  (-> ocr meta :map-occurrence))

(defmulti leaf-bind-expr (fn [ocr] (-> ocr meta :occurrence-type)))

(defmethod leaf-bind-expr :map
  [ocr] (let [m (meta ocr)]
            `(get ~(:map-sym m) ~(:key m))))

(defmethod leaf-bind-expr :default
  [ocr] ocr)

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
  (necessary-column [this])
  (useful-matrix [this])
  (select [this])
  (score [this])
  (occurrences [this])
  (action-for-row [this j]))

(declare empty-matrix?)

(defprotocol ISpecializeMatrix
  (specialize-matrix [this matrix]))

(declare pattern-matrix)


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
                (apply sorted-set)))]
      (cond
       (empty? rows) (fail-node)
       (let [f (first rows) ;; TODO: A big gross, cleanup - David
             ps (patterns f)]
         (and (not (nil? ps))
              (empty? ps))) (let [f (first rows)]
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
                        (-> ocrs first meta :seq-occurrence) 0 ;; TODO: don't hardcode - David
                        :else (necessary-column this))]
                (if (= col 0)
                  (let [constrs (column-constructors this col)
                        default (let [m (specialize this (wildcard-pattern))]
                                  (if-not (empty-matrix? m)
                                    (compile m)
                                    (fail-node)))]
                    (switch-node
                      (ocrs col)
                      (into [] (map (fn [c]
                                      (let [s (-> this 
                                                  (specialize c) 
                                                  compile)]
                                        [c s]))
                                    constrs))
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

  (score [_] [])

  (rows [_] rows)

  (occurrences [_] ocrs)

  (action-for-row [_ j]
    (action (rows j)))

  IVecMod
  (drop-nth [_ i]
    (PatternMatrix. (vec (map #(drop-nth % i) rows)) ocrs))

  (swap [_ idx]
    (PatternMatrix. (vec (map #(swap % idx) rows))
                    (swap ocrs idx))))

(defn make-sym-pair-generator [c]
  (let [current (atom c)
        next    (atom (gensym c))]
    (fn []
      (let [old-c @current
            old-n @next]
        (swap! current (fn [_] old-n))
        (swap! next    (fn [_] (gensym c)))
        [old-c old-n]))))

(extend-type SeqPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          srows (filter #(pattern-equals this (first %)) rows)
          width (reduce max (map (fn [srow]
                                   (let [^SeqPattern p (first srow)]
                                     (count (.s p))))
                                 srows))
          nrows (->> srows
                     (map (fn [row]
                            (let [^SeqPattern p (first row)
                                  s (.s p)] ;; NOTE: use the pattern that actually belongs to the row - David
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      (reverse
                                       (into s
                                             (repeatedly (clojure.core/inc
                                                          (- width (count s)))
                                                         seq-crash-pattern)))))))
                     vec)
          nocrs (let [seq-ocr focr
                      next-syms (make-sym-pair-generator seq-ocr)
                      ocr-sym (fn ocr-sym [x]
                                (let [ocr (gensym (str (name seq-ocr) x))
                                      [seq-ocr next-ocr] (next-syms)]
                                  (with-meta ocr
                                    {:seq-occurrence true
                                     :occurrence-type :seq
                                     :seq-sym seq-ocr
                                     :bind-expr `(let [~ocr (first ~seq-ocr)
                                                       ~next-ocr (next ~seq-ocr)])})))]
                  (into (conj (into []
                                    (map ocr-sym (range width)))
                              (with-meta (gensym seq-ocr)
                                {:seq-occurence true
                                 :occurrence-type :seq
                                 :seq-sym (first (next-syms))}))
                        (drop-nth ocrs 0)))]

      (pattern-matrix nrows nocrs))))


(extend-type SeqCrashPattern
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
                        sort)
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
                              (reduce conj (drop-nth-bind row 0 focr)
                                      (map second
                                           (sort (merge crash-map wc-map m)))))))
                     vec)
          nocrs (let [map-ocr focr
                      ocr-sym (fn ocr-sym [k]
                                (let [ocr (gensym (str (name map-ocr) "-" (name k)))]
                                  (with-meta ocr
                                    {:map-occurrence true
                                     :occurrence-type :map
                                     :key k
                                     :map-sym map-ocr
                                     :bind-expr `(let [~ocr (get ~map-ocr ~k)])})))]
                  (into (into [] (map ocr-sym all-keys))
                        (drop-nth ocrs 0)))]
      (pattern-matrix nrows nocrs))))

;; TODO: redundant, combine this and SeqCrashPattern ISpecializeMatrix - David

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


(prefer-method print-method clojure.lang.IType clojure.lang.ISeq)

(defn ^PatternMatrix pattern-matrix [rows ocrs]
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

(defn print-matrix
  ([pm] (print-matrix pm 4))
  ([pm col-width]
     (binding [*out* (pp/get-pretty-writer *out*)]
       (print "|")
       (doseq [o (occurrences pm)]
         (pp/cl-format true "~4D~7,vT" o col-width))
       (print "|")
       (prn)
       (doseq [[i row] (map-indexed (fn [p i] [p i]) (rows pm))]
         (print "|")
         (doseq [p (patterns row)]
           (pp/cl-format true "~4D~7,vT" (str p) col-width))
         (print "|")
         (print " " (action-for-row pm i))
         (prn))
       (println))))

;; Pattern matching interface

(defn emit-pattern [pat]
  (letfn [(type-pattern? [pat]
            (and (list? pat)
                 (= (count pat) 2)
                 (= (first pat) 'isa?)))]
   (cond
    (type-pattern? pat) (type-pattern (resolve (second pat)))
    (seq? pat) (seq-pattern
                (loop [ps pat v []]
                  (if (nil? ps)
                    v
                    (let [p (first ps)]
                      (cond
                       (= p '&) (let [p (second ps)]
                                  (recur (nnext ps) (conj v (rest-pattern (emit-pattern p)))))
                       :else (recur (next ps) (conj v (emit-pattern (first ps)))))))))
    (map? pat) (map-pattern
                (->> pat
                     (map (fn [[k v]]
                            (when (not= k :only)
                              [(emit-pattern k) v])))
                     (remove nil?)
                     (into {}))
                (:only pat))
    (symbol? pat) (wildcard-pattern pat)
    :else (literal-pattern pat))))
            
(defn emit-clause [[pat action]]
  (let [p (into [] (map emit-pattern pat))]
    (pattern-row p action)))

(defn emit-matrix [vars clauses]
  (let [cs (partition 2 clauses)
        clause-sources (into [] (map emit-clause cs))]
    (pattern-matrix clause-sources vars)))

(defmacro build-matrix [vars & clauses]
  `(emit-matrix '~vars '~clauses))

(defmacro defmatch [name vars & clauses]
  (let [clj-form (-> (emit-matrix vars clauses)
                   compile
                   to-clj)]
    `(defn ~name ~vars 
       ~clj-form)))

(defmacro match [vars & clauses]
  `~(-> (emit-matrix vars clauses)
      compile
      to-clj))

; =============================================================================
; Active Work

(comment
  (let [x '(1 2 3)]
    (match [x]
           [(1 2 3)] :a0))
  
  (let [x '(1 2 3)]
    (match [x]
           [(1 a & rest)] [:a0 a rest]))

  (emit-pattern '(1 a b))
  (emit-pattern '(1 a & rest))
  (emit-pattern '(1 a & [b c]))

  (let [x '(1 2 (3 5))]
   (match [x]
          [(1 2 (3 4))] :a0
          [(1 2 (3 5))] :a1))

  (let [x '(1 2 (3 5))]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e6]
        (match [x]
               [(1 2 (3 4))] :a0
               [(1 2 (3 5))] :a1)))))
  )
