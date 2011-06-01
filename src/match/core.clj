(ns match.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.logic.minikanren :exclude [swap]]
        [clojure.core.logic prelude])
  (:use [clojure.pprint :only [pprint]])
  (:require [clojure.pprint :as pp])
  (:import [java.io Writer]))

;; TODO: add action to the row, this information needs to be passed along
;; TODO: flesh out what a decision tree looks like, what remains to be compiled
;; TODO: pattern matrix should take list of occurences

(defn vec-drop-nth [v idx]
  (into (subvec v 0 idx)
        (subvec v (clojure.core/inc idx) (count v))))

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
  (drop-nth [this n])
  (prepend [this x]))

(deftype PatternRow [ps action]
  IPatternRow
  (action [_] action)
  (patterns [_] ps)
  (drop-nth [_ n]
    (PatternRow. (vec-drop-nth ps n) action))
  (prepend [_ x]
    (PatternRow. (into [x] ps) action))
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

(defprotocol IPatternMatrix
  (width [this])
  (height [this])
  (dim [this])
  (specialize [this c])
  (compile [this])
  (pattern-at [this i j])
  (column [this i])
  (drop-column [this i])
  (row [this j])
  (rows [this])
  (necessary-column [this])
  (useful-matrix [this])
  (select [this])
  (swap [this idx])
  (score [this]))

(declare necessary?)

(deftype PatternMatrix [rows]
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
               (map #(drop-nth % 0))))))
  (compile [this]
    (let [pm (select this)
          f (set (column pm 0))]
      (map compile (map #(specialize pm %) f))))
  (pattern-at [_ i j] ((rows j) i))
  (column [_ i] (vec (map #(nth % i) rows)))
  (drop-column [_ i]
    (PatternMatrix. (vec (map #(drop-nth % i) rows))))
  (row [_ j] (nth rows j))
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
  (swap [_ idx]
    (PatternMatrix.
     (vec (map (fn [row]
                 (let [p (nth row idx)]
                   (-> row
                       (drop-nth idx)
                       (prepend p))))
               rows))))
  (select [this]
    (swap this (necessary-column this)))
  (score [_] [])
  (rows [_] rows))

(prefer-method  print-method clojure.lang.IType clojure.lang.ISeq)

(defn ^PatternMatrix pattern-matrix [rows]
  (PatternMatrix. rows))

(defn score-p [pm i j]
  )

(defn necessary? [column]
  (every? (fn [p]
            (or (literal? p)
                (type-pred? p)))
          column))

(defn useful-p? [pm i j]
  (or (and (constructor? (pattern-at pm i j))
           (every? #(not (wildcard? %))
                   (take j (column pm i))))
      (and (wildcard? (pattern-at pm i j))
           (not (useful? (drop-column pm i) j)))))

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
       (doseq [row (rows pm)]
         (print "|")
         (doseq [p (patterns row)]
           (pp/cl-format true "~4D~7,vT" (print-pattern p) col-width))
         (print "|")
         (prn)))))

;; =============================================================================
;; Active Work

(comment
  ;; we're working with this at the moment, no guards, no implication
  ;; just the basic Maranget algorithm. We'd like to be execute the following
  ;;
  ;; (match [x y z]
  ;;   [_  f# t#] 1
  ;;   [f# t# _ ] 2
  ;;   [_  _  f#] 3
  ;;   [_  _  t#] 4)
  ;;
  (def pm2 (pattern-matrix [[wildcard (pattern false) (pattern true)]
                            [(pattern false) (pattern true) wildcard]
                            [wildcard wildcard (pattern false)]
                            [wildcard wildcard (pattern true)]]))

  (def pr1 (pattern-row [wildcard (pattern false) (pattern true)] :a1))

  (def pm2 (pattern-matrix [(pattern-row [wildcard (pattern false) (pattern true)] :a1)
                            (pattern-row [(pattern false) (pattern true) wildcard] :a2)
                            (pattern-row [wildcard wildcard (pattern false)] :a3)
                            (pattern-row [wildcard wildcard (pattern true)] :a4)]))

  (print-matrix pm2)

  ;; 700ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (necessary-column pm2))))

  (useful-matrix pm2)

  ;; TODO: don't use prepend on vector, add as method of PatternMatrix
  (specialize (select pm2) (pattern true))
  (specialize (select pm2) (pattern false))
  )

;; =============================================================================
;; On Hold

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