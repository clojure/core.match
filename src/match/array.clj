(ns match.array
  (:refer-clojure :exclude [compile])
  (:use match.core))

(def IntArray (class (int-array [])))

(defmethod vtest-inline ::ints
  [_ ocr] `(instance? IntArray ~ocr))
(defmethod vnth-inline ::ints
  [_ ocr i] `(aget ~ocr ~i))
(defmethod vnth-offset-inline ::ints
  [_ ocr i offset] `(aget ~ocr (unchecked-add ~i ~offset)))
(defmethod vsubvec-inline ::ints
  [_ ocr i] ocr)

(comment
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
  (let [x (int-array [1 1 2 3])
        o 1]
    (match [x]
      [([_ _ 2] :vec ::ints :offset o)] :a0
      [([1 1 3] :vec ::ints :offset o)] :a1
      [([1 2 3] :vec ::ints :offset o)] :a2))

  ;; 80ms
  (let [x (int-array [1 1 2 3])
        o 1]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 2] :vec ::ints :offset o)] :a0
           [([1 1 3] :vec ::ints :offset o)] :a1
           [([1 2 3] :vec ::ints :offset o)] :a2)))))

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
