(ns match.scratch
  (:refer-clojure :exclude [reify == inc compile])
  (:use [logos.minikanren :exclude [swap]]
        [logos tabled rel])
  (:use [clojure.pprint :only [pprint]])
  (:import [java.io Writer]))

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
          (or (identical? this other)
              (= p (.p ^Pattern other))))
  (hashCode [this]
            (hash p))
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

(defn wildcard? [p]
  (identical? p wildcard))

(defn constructor? [p]
  (not (wildcard? p)))

(declare useful-p?)
(declare useful?)

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
        (map #(vec-drop-nth % 0)
             (filter (fn [[f]]
                       (or (= f p)
                           (wildcard? f)))
                     rows))))
  (->dag [this])
  (compile [this]
     (let [f (set (column (select this) 0))]
       (map #(specialize this %) f)))
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
  (rows [_] rows))

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

  (drop-column pm2 0)

  ;; 600ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (necessary-column pm2))))

  (seq (select pm2))

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
  (seq (ms->pm '[[[a b 0] [(isa? A a) (isa? B b)]]
                 [[a b 1] [(isa? A a) (isa? B b)]]]))

  ;; test the can look at the pm as a seq
  (seq pm1)

  (type pm1)
  
  (seq (swap pm1 1))

  )