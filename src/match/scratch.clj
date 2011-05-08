(ns match.scratch
  (:refer-clojure :exclude [reify == inc compile])
  (:use [logos minikanren tabled rel])
  (:use [clojure.pprint :only [pprint]])
  (:import [java.io Writer]))

(def guard-priorities {'= 0
                       'isa? 1})

(defprotocol IPattern
  (guard [this]))

(deftype Pattern [p gs]
  IPattern
  (guard [this] (first gs)))

(defmethod print-method Pattern [^Pattern x ^Writer writer]
  (if-let [gs (.gs x)]
    (.write writer (str "<Pattern: " (.p x) " guards: " gs ">"))
    (.write writer (str "<Pattern: " (.p x) ">"))))

(defn ^Pattern pattern
  ([p] (Pattern. p nil))
  ([p gs] {:pre [(or (sequential? gs) (nil? gs))]}
     (Pattern. p gs)))

(defprotocol IPatternMatrix
  (specialize [this c])
  (->dag [this])
  (compile [this])
  (pattern-at [this x y])
  (column [this n])
  (row [this n]))

(deftype PatternMatrix [rows]
  IPatternMatrix
  (specialize [this c])
  (->dag [this])
  (compile [this])
  (pattern-at [this n m] ((rows n) m))
  (column [this n] (map #(nth % n) rows))
  (row [this n] (nth rows n)))

(defn ^PatternMatrix pattern-matrix [rows]
  (PatternMatrix. rows))

(defn guards-for [p gs]
  (sort sort-guards
        (reduce (fn [s g]
                  (if (contains? (set g) p)
                    (conj s g)
                    s))
                [] gs)))

(defn sort-guards [[as] [bs]]
  (let [asi (get guard-priorities as 2)
        bsi (get guard-priorities bs 2)]
    (cond
     (< asi bsi) -1
     (> asi bsi) 1
     :else 0)))

(defn proc-row [[ps gs :as row]]
  (map (fn [p]
         (let [pgs (guards-for p gs)]
           (pattern p pgs)))
       ps))

(defn sigs->pm [sigs]
  (pattern-matrix (map proc-row sigs)))

(comment
  (def pm (pattern-matrix [[(pattern 'a '[(isa? B a)]) (pattern 0)]
                           [(pattern 'a '[(isa? C a)]) (pattern 1)]]))

  (.rows (sigs->pm '[[[a b 0] [(isa? A a) (isa? B b)]]
                     [[a b 1] [(isa? A a) (isa? B b)]]]))
  )