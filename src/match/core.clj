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

(deftype WildcardPattern []
  java.lang.Comparable
  (compareTo [this that]
    1000)
  Object
  (toString [_]
    "_")
  (hashCode [_]
    2299))

(deftype CrashPattern []
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
    `(= ~ocr ~l))
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

(deftype VectorPattern [v]
  IPatternCompile
  (p-to-clj [this ocr]
    `(vector? ~ocr))
  java.lang.Comparable
  (compareTo [_ that]
    (if (instance? VectorPattern that)
      0
      -200))
  Object
  (toString [_]
    (str v))
  (equals [_ that]
    (instance? VectorPattern that))
  (hashCode [_]
    3331))

(defn ^WildcardPattern wildcard-pattern [] 
  (WildcardPattern.))
  
(defn ^CrashPattern crash-pattern []
  (CrashPattern.))

(defn ^LiteralPattern literal-pattern [l] 
  (LiteralPattern. l))

(defn ^TypePattern type-pattern [t] 
  {:pre [(class? t)]}
  (TypePattern. t))

(defn ^VectorPattern vector-pattern [v]
  {:pre [(vector? v)]}
  (VectorPattern. v))

(def wildcard-pattern? (partial instance? WildcardPattern))
(def crash-pattern?    (partial instance? CrashPattern))
(def literal-pattern?  (partial instance? LiteralPattern))
(def type-pattern?     (partial instance? TypePattern))
(def vector-pattern?   (partial instance? VectorPattern))

(defmulti pattern-equals (fn [a b] [(type a) (type b)]))

(defmethod pattern-equals [Object WildcardPattern]
  [a b] true)

(defmethod pattern-equals [LiteralPattern LiteralPattern]
  [^LiteralPattern a ^LiteralPattern b] (= (.l a) (.l b)))

(defmethod pattern-equals [VectorPattern VectorPattern]
  [a b] true)

(defmethod pattern-equals [CrashPattern CrashPattern]
  [a b] true)

(defmethod pattern-equals [TypePattern TypePattern]
  [^TypePattern a ^TypePattern b] (= (.t a) (.t b)))

(defmethod pattern-equals :default 
  [a b] false)

(defmethod print-method WildcardPattern [^WildcardPattern p ^Writer writer]
  (.write writer "<WildcardPattern>"))

(defmethod print-method CrashPattern [^CrashPattern p ^Writer writer]
  (.write writer "<CrashPattern>"))

(defmethod print-method LiteralPattern [^LiteralPattern p ^Writer writer]
  (.write writer (str "<LiteralPattern: " p ">")))

(defmethod print-method TypePattern [^TypePattern p ^Writer writer]
  (.write writer (str "<TypePattern: " p ">")))

(defmethod print-method VectorPattern [^VectorPattern p ^Writer writer]
  (.write writer (str "<VectorPattern: " p ">")))

(defn constructor? [p]
  (not (wildcard-pattern? p)))

(declare useful-p?)
(declare useful?)

(defprotocol IPatternRow
  (action [this])
  (patterns [this])
  (first-concrete-column-num [this])
  (all-wildcards? [this]))

(deftype PatternRow [ps action]
  IPatternRow
  (action [_] action)
  (patterns [_] ps)
  (first-concrete-column-num [this]
    (->> (map wildcard-pattern? ps)
         (map-indexed vector)
         (filter (comp not second))
         first
         first))
  (all-wildcards? [this]
    (every? wildcard-pattern? ps))
  IVecMod
  (drop-nth [_ n]
    (PatternRow. (drop-nth ps n) action))
  (prepend [_ x]
    (PatternRow. (into [x] ps) action))
  (swap [_ n]
    (PatternRow. (swap ps n) action))
  clojure.lang.Indexed
  (nth [_ i]
    (nth ps i))
  (nth [_ i x]
    (nth ps i x))
  clojure.lang.ISeq
  (first [_] (first ps))
  (next [_]
    (if-let [nps (next ps)]
      (PatternRow. nps action)))
  (more [_]
    (if (empty? ps)
      '()
      (let [nps (rest ps)]
        (PatternRow. nps action))))
  (count [_]
    (count ps))
  clojure.lang.IFn
  (invoke [_ n]
    (nth ps n)))

(defn ^PatternRow pattern-row [ps action]
  (PatternRow. ps action))

;; Decision tree nodes

(defrecord LeafNode [value]
  INodeCompile
  (to-clj [this]
    value))

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

(defn ^LeafNode leaf-node [value]
  (LeafNode. value))

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
  (occurences [this])
  (action-for-row [this j]))

;; TODO: probably should break out the conds into multimethods to allow for a clean
;; extension point - David

(declare empty-matrix?)

(defprotocol ISpecializeMatrix
  (specialize-matrix [this matrix]))

(extend-type VectorPattern
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [^PatternMatrix matrix matrix
          rows (.rows matrix)
          ocrs (.ocrs matrix)
          nrows (->> rows
                     (filter #(pattern-equals this (first %)))
                     (map (fn [row]
                            (reduce prepend (drop-nth row 0)
                                    (reverse (conj (.v this) (crash-pattern))))))
                     vec)
          nocrs (let [seq-ocr (first ocrs)
                      ocr-sym (fn ocr-sym [x]
                                (let [ocr (symbol (str (name seq-ocr) x))]
                                  (with-meta ocr
                                    {:seq-occurrence true
                                     :seq-sym seq-ocr
                                     :bind-expr `(let [~ocr (first ~seq-ocr)
                                                       ~seq-ocr (next ~seq-ocr)])})))]
                  (into (conj (into []
                                    (map ocr-sym
                                         (range (count (.v this)))))
                              (with-meta (ocr-sym "r")
                                {:seq-occurence true
                                 :seq-sym seq-ocr}))
                        (drop-nth ocrs 0)))]
      (PatternMatrix. nrows nocrs))))

(extend-type Object
  ISpecializeMatrix
  (specialize-matrix [this matrix]
    (let [^PatternMatrix matrix matrix
          rows (.rows matrix)
          ocrs (.ocrs matrix)
          nrows (->> rows
                     (filter #(pattern-equals this (first %)))
                     (map #(drop-nth % 0))
                     vec)
          nocrs (drop-nth ocrs 0)]
      (PatternMatrix. nrows nocrs))))

(deftype PatternMatrix [rows ocrs]
  IPatternMatrix
  (width [_] (if (not (empty? rows))
               (count (rows 0))
               0))

  (height [_] (count rows))

  (dim [this] [(width this) (height this)])

  (specialize [this p]
    (letfn [(specialize-row [row]
              (let [p (first row)]
                (cond
                 (vector-pattern? p) (let [v (.v ^VectorPattern p)]
                                       (reduce prepend (drop-nth row 0)
                                               (reverse (conj v (crash-pattern)))))
                 :else (drop-nth row 0))))
            (next-rows [p rows]
              (->> rows
                (filter #(pattern-equals p (first %)))
                (map specialize-row)
                vec))
            (seq-bind-expr [ocr seq-ocr]
              `(let [~ocr (first ~seq-ocr)
                     ~seq-ocr (next ~seq-ocr)]))
            (next-occurrences [p ocrs]
              (cond
               (vector-pattern? p) (let [seq-ocr (first ocrs)
                                         ocr-sym (fn ocr-sym [x]
                                                   (let [ocr (symbol (str (name seq-ocr) x))]
                                                    (with-meta ocr
                                                      {:seq-occurrence true
                                                       :seq-sym seq-ocr
                                                       :bind-expr (seq-bind-expr ocr seq-ocr)})))]
                                     (into (conj (into []
                                                       (map ocr-sym
                                                            (range (count (.v ^VectorPattern p)))))
                                                 (with-meta (ocr-sym "r")
                                                   {:seq-occurence true
                                                    :seq-sym seq-ocr}))
                                           (drop-nth ocrs 0)))
               :else (drop-nth ocrs 0)))]
      (PatternMatrix.
        (next-rows p rows)
        (next-occurrences p ocrs))))

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
              (empty? ps))) (leaf-node (action (first rows)))
       (all-wildcards? (first rows)) (leaf-node (action (first rows)))
       :else (let [col (first-concrete-column-num (first rows))]
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
    (->> (apply map vector (useful-matrix this))
         (map-indexed (fn [i col]
                        [(reduce (fn [s b]
                                   (if b (clojure.core/inc s) s))
                                 0 col) i]))
         (reduce (fn [m [c i]]
                   (if (> c m) i m))
                 0)))

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

  (occurences [_] ocrs)

  (action-for-row [_ j]
    (action (rows j)))

  IVecMod
  (drop-nth [_ i]
    (PatternMatrix. (vec (map #(drop-nth % i) rows)) ocrs))

  (swap [_ idx]
    (PatternMatrix. (vec (map #(swap % idx) rows))
                    (swap ocrs idx))))

(defn empty-matrix? [pm]
  (= (dim pm) [0 0]))

(prefer-method print-method clojure.lang.IType clojure.lang.ISeq)

(defn ^PatternMatrix pattern-matrix [rows ocrs]
  (PatternMatrix. rows ocrs))

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
       (doseq [o (occurences pm)]
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
    (vector? pat) (vector-pattern (into [] (map emit-pattern pat)))
    (= pat '_) (wildcard-pattern)
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
  ;; we're working with this at the moment, no guards, no implication
  ;; just the basic Maranget algorithm. We'd like to be execute the following
  ;;
  ;; (match [x y z]
  ;;   [_  f# t#] 1
  ;;   [f# t# _ ] 2
  ;;   [_  _  f#] 3
  ;;   [_  _  t#] 4)

  (def pr1 (pattern-row [(wildcard-pattern) (literal-pattern false) (literal-pattern true)] :a1))
  (pattern-row [(wildcard-pattern) (literal-pattern false) (literal-pattern true)] :a1)

  ;; (match [x y z]
  ;;   [_  f# t#] 1
  ;;   [f# t# _ ] 2
  ;;   [_  _  f#] 3
  ;;   [_  _  t#] 4)
  
  (def pm2 (pattern-matrix [(pattern-row [(wildcard-pattern) (literal-pattern false) (literal-pattern true)] :a1)
                            (pattern-row [(literal-pattern false) (literal-pattern true) (wildcard-pattern)] :a2)
                            (pattern-row [(wildcard-pattern) (wildcard-pattern) (literal-pattern false)] :a3)
                            (pattern-row [(wildcard-pattern) (wildcard-pattern) (literal-pattern true)] :a4)]
                           '[x y z]))

  (print-matrix pm2)

  (specialize pm2 (wildcard-pattern))

  (pprint (compile pm2))

  ;; YES
  (source-pprint (to-clj (compile pm2)))

  (useful-matrix pm2)

  (print-matrix pm2)
  (print-matrix (select pm2))
  (print-matrix (specialize (select pm2) (wildcard-pattern)))
  (print-matrix (specialize (select pm2) (literal-pattern true)))
  (print-matrix (specialize (select pm2) (literal-pattern false)))
  (print-matrix (select (specialize (select pm2) (literal-pattern true))))
  (print-matrix (specialize (select (specialize (select pm2) (literal-pattern true))) (literal-pattern false)))
  (print-matrix (specialize (select pm2) (literal-pattern false)))
  (print-matrix (select (specialize (select pm2) (literal-pattern false))))
  (print-matrix (specialize (select (specialize (select pm2) (literal-pattern false))) (literal-pattern true)))
  ;; ^ we can discard :a4
  ;; 

  ;; fail node
  (def cm1 (pattern-matrix []
                           '[]))
  (compile cm1)
  ;;=> #match.core.FailNode[]

  ;; leaf node - case m > 0, n = 0
  (def cm2 (pattern-matrix [(pattern-row [] :a1)]
                           '[]))
  (compile cm2)
  ;;=> #match.core.LeafNode[:a1]

  ;; leaf node - case m > 0, n > 0
  (def cm3 (pattern-matrix [(pattern-row [(wildcard-pattern)] :a1)]
                           '[x]))
  (compile cm3)
  ;;=> #match.core.LeafNode[:a1]

  ;; switch 
  (def cm4 (pattern-matrix [(pattern-row [true] :a1)]
                           '[x]))
  (compile cm4)

  (def cm5 (pattern-matrix [(pattern-row [(wildcard-pattern) (literal-pattern false) (literal-pattern true)] :a1)]
                           '[x y z]))
  (compile cm5)
  (to-clj (compile cm5))

  (pprint (-> (build-matrix [x]
                            [[1]] 1
                            [(isa? Object)] 2)
              compile))

  (pprint (-> (build-matrix [x]
                            [[1]] 1
                            [(isa? Object)] 2)
              compile
              to-clj))

  (pprint (-> (build-matrix [x]
                            [[1]] 1
                            [(isa? String)] 2
                            [(isa? Object)] 3)
              compile
              to-clj))

  (def m1 (build-matrix [x y z]
                        [1 2 3] :a0
                        [[1 2 3] 4 5] :a1
                        [[2 3 4] 5 6] :a2))

  (def m2 (build-matrix [x]
                        [[1 2 3]] :a0
                        [[1 2 4]] :a1))

  (pprint (compile m1))

  (source-pprint (-> m1 compile to-clj))

  (source-pprint (-> m2 compile to-clj))

  ;; WORKS
  (let [x [1 2 4]]
    (match [x]
           [[1 2 3]] :a0
           [[1 2 4]] :a1))

  ;; this looks perfect
  (source-pprint (-> m2 (specialize (vector-pattern [1 2 3])) compile to-clj))
)