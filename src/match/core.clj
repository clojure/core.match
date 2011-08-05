(ns match.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.logic.minikanren :exclude [swap]]
        [clojure.core.logic prelude])
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.pprint :as pp])
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
    (str sym))
  (hashCode [_]
    2299))

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
    "CRASH")
  (hashCode [_]
    2399))

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
    (cond 
      (instance? LiteralPattern that) (cond 
                                        (and (not= l (.l that))) -1
                                        (and (nil? l)
                                             (nil? (.l that))) 0
                                        (nil? l) -1
                                        (nil? (.l that)) 1
                                        :else (.compareTo l (.l that)))
      :else -1000))
  Object
  (toString [_]
    (if (nil? l)
      "nil"
      (str l)))
  (hashCode [this]
    (+ 1142 (hash l))))

(deftype TypePattern [t]
  IPatternCompile
  (p-to-clj [this ocr]
    `(instance? ~t ~ocr))
  java.lang.Comparable
  (compareTo [this that]
    (if (instance? TypePattern that)
      (compare (hash t) (hash (.t that))) ;; NOTE: see note about inheritance below. pontential hash collisions? - David
      -100))
  Object
  (toString [_]
    (str t))
  (hashCode [this]
    (+ 2242 (hash t))))

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
    (str s))
  (equals [_ that]
    (instance? SeqPattern that))
  (hashCode [_]
    3331))

(deftype MapPattern [m]
  IPatternCompile
  (p-to-clj [this ocr]
    `(map? ~ocr))
  java.lang.Comparable
  (compareTo [_ that]
    (if (instance? MapPattern)
      0
      -100))
  Object
  (toString [_]
    (str m))
  (equals [_ that]
    (instance? MapPattern that))
  (hashCode [_]
    4337))

(defn ^WildcardPattern wildcard-pattern
  ([] (WildcardPattern. '_))
  ([sym] (WildcardPattern. sym)))
  
(defn ^SeqCrashPattern seq-crash-pattern []
  (SeqCrashPattern.))

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
  ([] (MapPattern. {}))
  ([m] {:pre [(map? m)]}
     (MapPattern. m)))

(def wildcard-pattern? (partial instance? WildcardPattern))
(defn named-wildcard-pattern? [x]
  (when (instance? WildcardPattern x)
    (not= (.sym ^WildcardPattern x) '_)))
(def seq-crash-pattern?   (partial instance? SeqCrashPattern))
(def literal-pattern? (partial instance? LiteralPattern))
(def type-pattern?    (partial instance? TypePattern))
(def seq-pattern?     (partial instance? SeqPattern))
(def map-pattern?     (partial instance? MapPattern))

(defmulti pattern-equals (fn [a b] [(type a) (type b)]))

(defmethod pattern-equals [Object WildcardPattern]
  [a b] true)

(defmethod pattern-equals [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b] (= (.l a) (.l b)))

(defmethod pattern-equals [SeqPattern SeqPattern]
  [a b] true)

(defmethod pattern-equals [MapPattern MapPattern]
  [a b] true)

(defmethod pattern-equals [SeqCrashPattern SeqCrashPattern]
  [a b] true)

(defmethod pattern-equals [TypePattern TypePattern]
  [^TypePattern a ^TypePattern b] (= (.t a) (.t b)))

(defmethod pattern-equals :default 
  [a b] false)

(defmethod print-method WildcardPattern [^WildcardPattern p ^Writer writer]
  (.write writer (str "<WildcardPattern: " (.sym p) ">")))

(defmethod print-method SeqCrashPattern [^SeqCrashPattern p ^Writer writer]
  (.write writer "<SeqCrashPattern>"))

(defmethod print-method LiteralPattern [^LiteralPattern p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

(defmethod print-method TypePattern [^TypePattern p ^Writer writer]
  (.write writer (str "<TypePattern: " p ">")))

(defmethod print-method SeqPattern [^SeqPattern p ^Writer writer]
  (.write writer (str "<SeqPattern: " p ">")))

(defmethod print-method MapPattern [^MapPattern p ^Writer writer]
  (.write writer (str "<MapPattern: " p ">")))

(defn constructor? [p]
  (not (wildcard-pattern? p)))

(declare useful-p?)
(declare useful?)

(defprotocol IPatternRow
  (action [this])
  (patterns [this])
  (bindings [this])
  (all-wildcards? [this])
  (drop-nth-bind [this n ocr])) ;; TODO: needs better name - David

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
        (let [sym (.sym ^WildcardPattern p)]
          (PatternRow. (drop-nth ps n) action
                       (conj (or bindings [])
                             [sym ocr])))
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
    (nth ps n)))

(defn ^PatternRow pattern-row
  ([ps action] (PatternRow. ps action nil))
  ([ps action bindings] (PatternRow. ps action bindings)))

(defrecord LeafNode [value bindings]
  INodeCompile
  (to-clj [this]
    (if (not (empty? bindings))
      `(let [~@(apply concat
                      (remove (fn [[sym _]]
                                (= sym '_))
                       bindings))]
         ~value)
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
                                           wsyms (map #(.sym ^WildcardPattern %) (.ps f))]
                                       (leaf-node (action f)
                                                  (concat (bindings f)
                                                          (map vector wsyms ocrs))))
       :else (let [col (if (-> ocrs first meta :seq-occurrence)
                         0 (necessary-column this))]
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

  ;; TODO: replace with more sophisticated scoring
  (necessary-column [this]
    (first
     (->> (apply map vector (useful-matrix this))
          (map-indexed (fn [i col]
                         [i (reduce (fn [score useful]
                                      (if useful
                                        (clojure.core/inc score)
                                        score))
                                    0 col)]))
          (reduce (fn [[col score :as curr]
                       [ocol oscore :as cand]]
                    (if (> oscore score) cand curr))
                  [0 0]))))

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


(extend-type SeqPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [rows (rows matrix)
          ocrs (occurrences matrix)
          focr (first ocrs)
          srows (filter #(pattern-equals this (first %)) rows)
          width (reduce max (map #(-> % first .s count) srows))
          nrows (->> srows
                     (map (fn [row]
                            (let [^SeqPattern p (first row)
                                  s (.s p)] ;; NOTE: use the pattern that actually belongs to the row - David
                              (reduce prepend (drop-nth-bind row 0 focr)
                                      (into s
                                            (repeat (clojure.core/inc
                                                     (- width (count s)))
                                                    (seq-crash-pattern)))))))
                     vec)
          make-sym-pair-generator (fn [c]
                                    (let [current (atom c)
                                          next    (atom (gensym c))]
                                      (fn []
                                        (let [old-c @current
                                              old-n @next]
                                          (swap! current (fn [_] old-n))
                                          (swap! next    (fn [_] (gensym c)))
                                          [old-c old-n]))))
          nocrs (let [seq-ocr focr
                      next-syms (make-sym-pair-generator seq-ocr)
                      ocr-sym (fn ocr-sym [x]
                                (let [ocr (gensym (str (name seq-ocr) x))
                                      [seq-ocr next-ocr] (next-syms)]
                                  (with-meta ocr
                                    {:seq-occurrence true
                                     :seq-sym seq-ocr
                                     :bind-expr `(let [~ocr (first ~seq-ocr)
                                                       ~next-ocr (next ~seq-ocr)])})))]
                  (into (conj (into []
                                    (map ocr-sym (range width)))
                              (with-meta (gensym seq-ocr)
                                {:seq-occurence true
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

(defn score-p [pm i j]
  )

(defn useful-p? [pm i j]
  (or (and (constructor? (pattern-at pm i j))
           (every? #(not (wildcard-pattern? %))
                   (take j (column pm i))))
      (and (wildcard-pattern? (pattern-at pm i j))
           (not (useful? (drop-nth pm i) j)))))

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
    (seq? pat) (seq-pattern (into () (map emit-pattern pat)))
    (map? pat) (map-pattern
                (->> pat
                     (map (fn [[k v]]
                            [(emit-pattern k) v]))
                     (into {})))
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
; Activ Work
(comment
  (def pm2 (pattern-matrix [(pattern-row [(wildcard-pattern) (literal-pattern false) (literal-pattern true)] :a1)
                            (pattern-row [(literal-pattern false) (literal-pattern true) (wildcard-pattern)] :a2)
                            (pattern-row [(wildcard-pattern) (wildcard-pattern) (literal-pattern false)] :a3)
                            (pattern-row [(wildcard-pattern) (wildcard-pattern) (literal-pattern true)] :a4)]
                           '[x y z]))

  (print-matrix pm2)
  (source-pprint (to-clj (compile pm2)))
  (useful-matrix pm2)
)
