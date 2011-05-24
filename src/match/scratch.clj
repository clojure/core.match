(ns match.scratch
  (:refer-clojure :exclude [reify == inc compile])
  (:use [logos.minikanren :exclude [swap]]
        [logos tabled rel])
  (:use [clojure.pprint :only [pprint]])
  (:import [java.io Writer]))

(def guard-priorities {'= 0
                       'isa? 1})

(defn vec-drop-nth [v idx]
  (into (subvec v 0 idx)
        (subvec v (clojure.core/inc idx) (count v))))

(defn prepend [v x]
  (into [x] v))

(defprotocol IPattern
  (literal? [this])
  (type-pred? [this])
  (guard [this]))

(deftype Pattern [p gs]
  Object
  ;; TODO: consider guards
  (equals [this other]
          (let [o ^Pattern other]
            (= p (.p o))))
  IPattern
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

(defprotocol IPatternMatrix
  (width [this])
  (height [this])
  (dim [this])
  (specialize [this c])
  (->dag [this])
  (compile [this])
  (pattern-at [this i j])
  (column [this i])
  (drop-column [this i])
  (row [this j])
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
        (map #(vec-drop-nth % 0)
             (filter (fn [[f]]
                       (or (= f p)
                           (wildcard? f)))
                     rows))))
  (->dag [this])
  (compile [this])
  (pattern-at [_ i j] ((rows j) i))
  (column [_ i] (vec (map #(nth % i) rows)))
  (drop-column [_ i]
     (PatternMatrix. (vec (map #(vec-drop-nth % i) rows))))
  (row [_ j] (nth rows j))
  (necessary-column [this]
     (reduce (fn [m [c i]]
               (if (> c m) i m))
             0 (map-indexed (fn [i col]
                              [(reduce (fn [s b]
                                         (if b (clojure.core/inc s) s))
                                       0 col) i])
                            (apply map vector
                                   (useful-matrix this)))))
  (useful-matrix [this]
     (vec (map vec
               (partition (width this)
                          (for [j (range (height this))
                                i (range (width this))]
                            (useful-p? this i j))))))
  (swap [_ idx]
    (PatternMatrix.
     (vec (map (fn [row]
                 (let [p (nth row idx)]
                   (-> row
                       (vec-drop-nth idx)
                       (prepend p))))
               rows))))
  (select [this]
          (swap this (necessary-column this)))
  (score [_] [])
  clojure.lang.ISeq
  (seq [_] (seq rows)))

(defn ^PatternMatrix pattern-matrix [rows]
  (PatternMatrix. rows))

(defn score-p [pm i j]
  )

(defn wildcard? [p]
  (identical? p wildcard))

(defn constructor? [p]
  (not (wildcard? p)))

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

(comment
  ;; create a pattern matrix
  (def pm1 (pattern-matrix [[(pattern 'a '[(isa? B a)]) (pattern 0)]
                           [(pattern 'a '[(isa? C a)]) (pattern 1)]]))

  ;; raw signatures and guards to pattern matrix
  (seq (ms->pm '[[[a b 0] [(isa? A a) (isa? B b)]]
                 [[a b 1] [(isa? A a) (isa? B b)]]]))

  ;; test the can look at the pm as a seq
  (seq pm1)

  (type pm1)
  
  (seq (swap pm1 1))

  (def pm2 (pattern-matrix [[wildcard (pattern false) (pattern true)]
                            [(pattern false) (pattern true) wildcard]
                            [wildcard wildcard (pattern false)]
                            [wildcard wildcard (pattern true)]]))

  (drop-column pm2 0)

  ;; 600ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (necessary-column pm2))))

  (seq (select pm2))

  (specialize (select pm2) (pattern true))
  (specialize (select pm2) (pattern false))

  ;; need to reread the bit about necessity before moving ahead much further
  ;; looks like we need to think about scoring the column, we also need to
  ;; read the accompanying paper on which rows can be considered useless

  ;; score
  )