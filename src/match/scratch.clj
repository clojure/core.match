(ns match.scratch
  (:refer-clojure :exclude [reify == inc compile])
  (:use [logos minikanren tabled rel])
  (:use [clojure.pprint :only [pprint]])
  (:import [java.io Writer]))

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

(defn sigs->pm [sigs]
  )

(comment
  (def pm (pattern-matrix [[(pattern 'a '[(isa? B a)]) (pattern 0)]
                           [(pattern 'a '[(isa? C a)]) (pattern 1)]]))

  (column pm 1)
  )