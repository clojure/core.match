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

(defprotocol IDecisionTree
  (->dag [this]))

(deftype DecisionTree [branches]
  IDecisionTree
  (->dag [this]))

(defprotocol IPattern
  (term [this])
  (literal? [this])
  (type-pred? [this])
  (guard [this]))

(deftype Pattern [p gs]
  Object
  ;; TODO: consider guards
  (equals [this other]
    (or (identical? this other)
        (= p (.p ^Pattern other))))
  (hashCode [this]
    (hash p))
  IPattern
  (term [_] p)
  (literal? [this]
    (or (number? p)))
  (type-pred? [this]
    (let [[pred] (first gs)]
      (= pred 'isa?)))
  (guard [this] (first gs)))

(defmethod print-method Pattern [^Pattern x ^Writer writer]
  (if-let [gs (.gs x)]
    (.write writer (str "<Pattern: " (.p x) " guards: " gs ">"))
    (.write writer (str "<Pattern: " (.p x) ">"))))

(defn ^Pattern pattern
  ([p] (Pattern. p nil))
  ([p gs] {:pre [(or (sequential? gs) (nil? gs))]}
     (Pattern. p gs)))

(def ^Pattern wildcard (pattern '_))

(defn wildcard? [p]
  (identical? p wildcard))

(defn constructor? [p]
  (not (wildcard? p)))

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
    (->> (map wildcard? ps)
         (map-indexed vector)
         (filter (comp not second))
         first
         first))
  (all-wildcards? [this]
    (every? wildcard? ps))
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


(defn dag-clause-to-clj [variable dispatch action]
  (vector (if (wildcard? dispatch)
            'true
            `(= ~variable ~(term dispatch)))
          (to-clj action)))

(defrecord SwitchNode [variable cases]
  INodeCompile
  (to-clj [this]
    (let [clauses (->> cases
                    (map (partial apply dag-clause-to-clj variable))
                    (apply concat))]
      `(cond ~@clauses))))

(defn ^SwitchNode switch-node [variable cases]
  (SwitchNode. variable cases))



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

(deftype PatternMatrix [rows ocrs]
  IPatternMatrix
  (width [_] (count (rows 0)))
  (height [_] (count rows))
  (dim [this] [(width this) (height this)])
  (specialize [this p]
    (PatternMatrix.
     (vec (->> rows
               (filter (fn [[f]]
                          (or (= f p)
                              (wildcard? f))))
               (map #(drop-nth % 0))))
     (drop-nth ocrs 0)))
  (compile [this]
    (cond
      (empty? rows) (fail-node)
      (all-wildcards? (first rows)) (leaf-node (action (first rows)))
      :else (let [col (first-concrete-column-num (first rows))]
              (if (= col 0)
                (let [constrs (set (filter (comp not wildcard?) (column this col)))]
                  (switch-node
                    (ocrs col)
                    (conj (into [] (map (fn [c]
                                          (let [s (-> this 
                                                    (specialize c) 
                                                    compile)]
                                            [c s]))
                                        constrs))
                          [wildcard (fail-node)])))
                (compile (swap this col))))))
  (pattern-at [_ i j] ((rows j) i))
  (column [_ i] (vec (map #(nth % i) rows)))
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
           (every? #(not (wildcard? %))
                   (take j (column pm i))))
      (and (wildcard? (pattern-at pm i j))
           (not (useful? (drop-nth pm i) j)))))

(defn useful? [pm j]
  (some #(useful-p? pm % j)
        (range (count (row pm j)))))

(defmulti print-pattern (fn [x] (term x)))
(defmethod print-pattern true
   [x] 't#)
(defmethod print-pattern false
   [x] 'f#)
(defmethod print-pattern :default
   [x] (term x))

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
           (pp/cl-format true "~4D~7,vT" (print-pattern p) col-width))
         (print "|")
         (print " " (action-for-row pm i))
         (prn))
       (println))))

;; Pattern matching interface

(defn emit-clause [[pat action]]
  (let [p (into [] (map #(if (= % (term wildcard))  ;; quick hack, will need to be modified for compound values
                           wildcard
                           (pattern %))
                        pat))]
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

  (def pr1 (pattern-row [wildcard (pattern false) (pattern true)] :a1))
  (pattern-row [wildcard (pattern false) (pattern true)] :a1)

;; (match [x y z]
;;   [_  f# t#] 1
;;   [f# t# _ ] 2
;;   [_  _  f#] 3
;;   [_  _  t#] 4)
  
(def pm2 (pattern-matrix [(pattern-row [wildcard (pattern false) (pattern true)] :a1)
                          (pattern-row [(pattern false) (pattern true) wildcard] :a2)
                          (pattern-row [wildcard wildcard (pattern false)] :a3)
                          (pattern-row [wildcard wildcard (pattern true)] :a4)]
                         '[x y z]))
(print-matrix pm2)

(compile pm2)
  
(to-clj (compile pm2))


  (useful-matrix pm2)

  (print-matrix pm2)
  (print-matrix (select pm2))
  (print-matrix (specialize (select pm2) (pattern true)))
  (print-matrix (select (specialize (select pm2) (pattern true))))
  (print-matrix (specialize (select (specialize (select pm2) (pattern true))) (pattern false)))
  (print-matrix (specialize (select pm2) (pattern false)))
  (print-matrix (select (specialize (select pm2) (pattern false))))
  (print-matrix (specialize (select (specialize (select pm2) (pattern false))) (pattern true)))
  ;; ^ we can discard :a4
  ;; 

 ; fail node
  (def cm1 (pattern-matrix []
                           '[]))
  (compile cm1)
  ;=> #match.core.FailNode[]

  ; leaf node - case m > 0, n = 0
  (def cm2 (pattern-matrix [(pattern-row [] :a1)]
                           '[]))
  (compile cm2)
  ;=> #match.core.LeafNode[:a1]

  ; leaf node - case m > 0, n > 0
  (def cm3 (pattern-matrix [(pattern-row [wildcard] :a1)]
                           '[x]))
  (compile cm3)
  ;=> #match.core.LeafNode[:a1]

  ; switch 
  (def cm4 (pattern-matrix [(pattern-row [true] :a1)]
                           '[x]))
  (compile cm4)

  (def cm5 (pattern-matrix [(pattern-row [wildcard (pattern false) (pattern true)] :a1)]
                           '[x y z]))
  (compile cm5)
  (to-clj (compile cm5))

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
