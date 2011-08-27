(ns match.array
  (:refer-clojure :exclude [compile])
  (:use [match.core :as m]))

(def IntArray (class (int-array [])))
(derive ::ints ::m/array)
(defmethod tag ::ints
  [_] IntArray)

(comment
  (let [x (int-array [1 2 3])]
    (match [x]
      [([_ _ 2] ::ints)] :a0
      [([1 1 3] ::ints)] :a1
      [([1 2 3] ::ints)] :a2
      :else :a3))
  
 ;; 60ms
  (let [x (int-array [1 2 3])]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e7]
        (match [x]
          [([_ _ 2] ::ints)] :a0
          [([1 1 3] ::ints)] :a1
          [([1 2 3] ::ints)] :a2)))))

  ;; offsets
  ;; FIXME: needs to account for offset - David
  (let [x (int-array [1 1 2 3])
        o 1]
    (match [x]
      [([_ _ 2] ::ints :offset o)] :a0
      [([1 1 3] ::ints :offset o)] :a1
      [([1 2 3] ::ints :offset o)] :a2))

  ;; 80ms
  (let [x (int-array [1 1 2 3])
        o 1]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 2] ::ints :offset o)] :a0
           [([1 1 3] ::ints :offset o)] :a1
           [([1 2 3] ::ints :offset o)] :a2)))))

  ;; better syntax
  (let [x (int-array [1 2 3])]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
        (match [^{:vec ::ints :offset 0} x]
          [[_ _ 2]] :a0
          [[1 1 3]] :a1
          [[1 2 3]] :a2)))))
 )
