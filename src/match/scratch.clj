(ns match.scratch
  (:refer-clojure :exclude [reify == inc compile])
  (:use [logos.minikanren :exclude [swap]]
        [logos tabled rel])
  (:use [clojure.pprint :only [pprint]])
  (:import [java.io Writer]))

(def guard-priorities {'= 0
                       'isa? 1})

(defn drop-nth [v idx]
  (into (subvec v 0 idx)
        (subvec v (clojure.core/inc idx) (count v))))

(defn prepend [v x]
  (into [x] v))

(defprotocol IPattern
  (literal? [this])
  (type-pred? [this])
  (guard [this]))

(deftype Pattern [p gs]
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
  (specialize [this c])
  (->dag [this])
  (compile [this])
  (pattern-at [this x y])
  (column [this n])
  (row [this n])
  (necessary-column [this])
  (necessity-matrix [this])
  (swap [this idx])
  (score [this]))

(declare necessary?)

(deftype PatternMatrix [rows]
  IPatternMatrix
  (specialize [this c])
  (->dag [this])
  (compile [this])
  (pattern-at [_ n m] ((rows n) m))
  (column [_ n] (map #(nth % n) rows))
  (row [_ n] (nth rows n))
  (necessary-column [this]
    (let [c (count rows)]
      (loop [idx 0]
        (cond
          (= idx c) 0
          (necessary? (column this idx)) idx
          :else (recur (inc idx))))))
  (necessity-matrix [this]
     (map #(map constructor? %) rows))
  (swap [_ idx]
    (PatternMatrix.
     (into []
           (map (fn [row]
                  (let [p (nth row idx)]
                   (-> row
                       (drop-nth idx)
                       (prepend p))))
                rows))))
  (score [_] [])
  clojure.lang.ISeq
  (seq [_] (seq rows)))

(defn ^PatternMatrix pattern-matrix [rows]
  (PatternMatrix. rows))

(defn score-p [pm i j]
  )

(defn constructor? [p]
  (not (identical? p wildcard)))

(defn necessary? [column]
  (every? (fn [p]
            (or (literal? p)
                (type-pred? p)))
          column))

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
  (into []
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

  (necessity-matrix pm2)
  
  ;; need to reread the bit about necessity before moving ahead much further
  ;; looks like we need to think about scoring the column, we also need to
  ;; read the accompanying paper on which rows can be considered useless

  ;; score
  )