(ns match.bits
  (:refer-clojure :exclude [compile])
  (:use [match.core :as m]))

(derive ::bits ::m/vector)

(defmethod check-size? ::bits
  [_] false)

(defmethod test-inline ::bits
  [_ ocr] `(instance? Long ~ocr))

(defmethod nth-inline ::bits
  [_ ocr i] `(bit-shift-right (bit-and ~ocr (bit-shift-left 1 ~i)) ~i))

(comment
  (let [x 5]
    (match [x]
      [([_ _ 1 1] ::bits)] :a0
      [([1 0 1 _] ::bits)] :a1
      :else :a2))
  
  (let [x 5]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 1 1] ::bits)] :a0
           [([1 0 1 _] ::bits)] :a1
           :else :a2)))))
  )
