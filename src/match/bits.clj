(ns match.bits
  (:refer-clojure :exclude [compile])
  (:use match.core))

(defmethod vtest-inline ::bits
  [_ ocr] `(instance? Long ~ocr))
(defmethod vnth-inline ::bits
  [_ ocr i] `(bit-shift-right (bit-and ~ocr (bit-shift-left 1 ~i)) ~i))

(comment
  (let [x 5]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 1 1] ::bits)] :a0
           [([1 0 1 _] ::bits)] :a1)))))
  )
