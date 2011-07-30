(ns match.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.logic.minikanren :exclude [swap]]
        [clojure.core.logic prelude])
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

;; TODO: flesh out what a decision tree looks like, what remains to be compiled

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

(defprotocol IPattern
  (match? [this c]))

;; TODO refactor with implementation inheritance

(deftype WildcardPattern []
  java.lang.Comparable
  (compareTo [this that]
    1000)
  Object
  (equals [this that]
    (instance? WildcardPattern that))
  (toString [_]
    "_")
  (hashCode [_]
    2299)
  IPattern
  (match? [_ _]
    true)
  clojure.lang.IFn
  (invoke [this n]
    (match? this n)))

(deftype CrashPattern []
  java.lang.Comparable
  (compareTo [this that]
    2000)
  Object
  (equals [this that]
    (instance? CrashPattern that))
  (toString [_]
    "CRASH")
  (hashCode [_]
    2399)
  IPattern
  (match? [_ _]
    true)
  clojure.lang.IFn
  (invoke [this n]
    (match? this n)))

(deftype LiteralPattern [l]
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
  (equals [this that]
    (and (instance? LiteralPattern that)
         (= (.l that)
            l)))
  (toString [_]
    (if (nil? l)
      "nil"
      (str l)))
  (hashCode [this]
    (+ 1142 (hash l)))
  IPattern
  (match? [this c]
    (= l c))
  clojure.lang.IFn
  (invoke [this n]
    (match? this n)))

(deftype TypePattern [t]
  java.lang.Comparable
  (compareTo [this that]
    (if (instance? TypePattern that)
      (compare (hash t) (hash (.t that))) ;; NOTE: see note about inheritance below. pontential hash collisions? - David
      -100))
  Object
  (equals [this that]
    (and (instance? TypePattern that)
         (= (.t that) t))) ;; NOTE: inheritance/implication should be handled by logic engine - David
  (toString [_]
    (str t))
  (hashCode [this]
    (+ 2242 (hash t)))
  IPattern
  (match? [this c]
    (instance? t c))
  clojure.lang.IFn
  (invoke [this n]
    (match? this n)))

(defprotocol IVectorPattern
  (current-index [this]))

(declare wildcard-pattern literal-pattern)

(deftype VectorPattern [v n]
  java.lang.Comparable
  (compareTo [this that]
    (if (instance? VectorPattern that)
      (.compareTo v (.v that))
      -200))
  Object
  (equals [this that]
    (instance? VectorPattern that))
  (toString [_]
    (str v))
  (hashCode [_]
    (hash v))
  IVectorPattern
  (current-index [_] ;; TODO possibly metadata, does not effect equality
    n)
  IPattern
  (match? [this n]
    (vector? n))
  clojure.lang.IFn
  (invoke [this n]
    (match? this n)))

(defn ^WildcardPattern wildcard-pattern [] 
  (WildcardPattern.))
  
(defn ^WildcardPattern crash-pattern [] 
  (CrashPattern.))

(defn ^LiteralPattern literal-pattern [l] 
  (LiteralPattern. l))

(defn ^TypePattern type-pattern [t] 
  {:pre [(class? t)]}
  (TypePattern. t))

(defn vector-pattern 
  (^VectorPattern [v] 
     {:pre [(vector? v)]}
     (VectorPattern. v 0))
  (^VectorPattern [v n] 
     {:pre [(vector? v)
            (and (integer? n)
                 (pos? n))]}
     (VectorPattern. v n)))

(def wildcard-pattern? (partial instance? WildcardPattern))
(def crash-pattern?    (partial instance? CrashPattern))
(def literal-pattern?  (partial instance? LiteralPattern))
(def type-pattern?     (partial instance? TypePattern))
(def vector-pattern?   (partial instance? VectorPattern))

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
    (let [nps (next ps)]
      (or (and nps (PatternRow. nps action))
          '())))
  (count [_]
    (count ps))
  clojure.lang.IFn
  (invoke [_ n]
    (nth ps n)))

(defn ^PatternRow pattern-row [ps action]
  (PatternRow. ps action))

;; Decision tree nodes

(defprotocol INodeCompile
  (to-clj [this]))

(defrecord LeafNode [value]
  INodeCompile
  (to-clj [this]
    value))

(defn ^LeafNode leaf-node [value]
  (LeafNode. value))

(defrecord FailNode []
  INodeCompile
  (to-clj [this]
    `(throw (Exception. "Found FailNode"))))

(defn ^FailNode fail-node []
  (FailNode.))

(defn dag-clause-to-clj [occurrence pattern action]
  (vector `(~pattern ~occurrence) 
          (to-clj action)))

(defrecord SwitchNode [occurrence cases]
  INodeCompile
  (to-clj [this]
    (let [clauses (->> cases
                    (map (partial apply dag-clause-to-clj occurrence))
                    (apply concat))]
      `(cond ~@clauses))))

(defn ^SwitchNode switch-node [occurrence cases]
  (SwitchNode. occurrence cases))

(defrecord AssignmentNode [clauses node]
  INodeCompile
  (to-clj [this]
    `(let [~@clauses]
       ~(to-clj node))))

(defn ^AssignmentNode assignment-node [clauses node]
  (AssignmentNode. clauses node))

;; Pattern Matrix

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

(deftype PatternMatrix [rows ocrs]
  IPatternMatrix
  (width [_] (count (rows 0)))
  (height [_] (count rows))
  (dim [this] [(width this) (height this)])
  (specialize [this p]
    (letfn [(filter-by-first-column [p rows] 
              (filter #(= (first %) p) rows))
            (specialize-row [row]
              (let [p (first row)]
                (cond
                 (vector-pattern? p) (let [v (.v ^VectorPattern p)]
                                       (reduce prepend (drop-nth row 0)
                                               (reverse (conj v (crash-pattern)))))
                 :else (drop-nth row 0))))
            (next-rows [p rows]
              (->> rows
                (filter-by-first-column p)
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
                                                    (with-meta
                                                      ocr
                                                      {:seq-occurrence true
                                                       :bind-expr (seq-bind-expr ocr seq-ocr)})))]
                                     (into (conj (into []
                                                       (map ocr-sym
                                                            (range (count (.v ^VectorPattern p)))))
                                                 (ocr-sym "r"))
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
        (all-wildcards? (first rows)) (leaf-node (action (first rows)))
        :else (let [col (first-concrete-column-num (first rows))]
                (if (= col 0)
                  (let [constrs (column-constructors this col)]
                    (switch-node
                      (ocrs col)
                      (conj (into [] (map (fn [c]
                                            (let [s (-> this 
                                                      (specialize c) 
                                                      compile)]
                                              [c s]))
                                          constrs))
                            [(wildcard-pattern) (fail-node)])))
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

; =============================================================================
; Active Work
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

  (compile pm2)
  
  (to-clj (compile pm2))


  (useful-matrix pm2)

  (print-matrix pm2)
  (print-matrix (select pm2))
  (print-matrix (specialize (select pm2) (literal-pattern true)))
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

  ;; NOTE: we support any kind of type in any column, the very first test
  ;; needs to be on the types of the constructors in a given column
  (pprint (-> (build-matrix [x]
                            [[1]] 1
                            [(isa? String)] 2
                            [(isa? Object)] 3)
              compile
              to-clj))
  ;; NOTE: we should sideline type patterns for now, focus on literal
  ;; patterns. We design doc about how to deal with types as they
  ;; pose a lot of challenges in terms of coming with a good syntax
  ;; that works with types, records, *and* Java classes.
  ;; perhaps punt on classes and force people to use field checks
  ;; in guards?

  (def m1 (build-matrix [x y z]
                        [1 2 3] 1
                        [[1 2 3] 4 5] 2
                        [[2 3 4] 5 6] 3))

  (pprint (specialize m1 (vector-pattern [1 2 3])))

  (pprint (compile m1))

  (-> (.ocrs (specialize m1 (vector-pattern [1 2 3])))
      first
      meta)
  ;; {:seq-occurence true}
)

; =============================================================================
; On Hold

(comment
  (def guard-priorities {'= 0
                         'isa? 1})

  (defn sort-guards [[as] [bs]]
    (let [asi (get guard-priorities as 2)
          bsi (get guard-priorities bs 2)]
      (cond
       (< asi bsi) -1
       (> asi bsi) 1
       :else 0)))

  (defn guards-for [p gs]
    (sort sort-guards
          (reduce (fn [s g]
                    (if (contains? (set g) p)
                      (conj s g)
                      s))
                  [] gs)))

  (defn proc-row [[ps gs :as row]]
    (vec
     (map (fn [p]
            (let [pgs (guards-for p gs)]
              (pattern p pgs)))
          ps)))

  (defn ms->pm [ms]
    (pattern-matrix (map proc-row ms)))
  
  ;; create a pattern matrix
  (def pm1 (pattern-matrix [[(pattern 'a '[(isa? B a)]) (pattern 0)]
                            [(pattern 'a '[(isa? C a)]) (pattern 1)]]))

  ;; raw signatures and guards to pattern matrix
  (ms->pm '[[[a b 0] [(isa? A a) (isa? B b)]]
            [[a b 1] [(isa? A a) (isa? B b)]]])

  ;; test the can look at the pm as a seq
  )
