(ns match.array
  (:refer-clojure :exclude [compile])
  (:use [match.core :as m]))

;; =============================================================================
;; Shared

(derive ::array ::m/vector)
(defmethod nth-inline ::array
  [t ocr i]
  `(aget ~ocr ~i))
(defmethod count-inline ::array
  [t ocr]
  `(alength ~ocr))
(defmethod subvec-inline ::array
  [_ ocr i] ocr)

;; =============================================================================
;; ints

(def IntArray (class (int-array [])))
(derive ::ints ::array)
(defmethod tag ::ints
  [_] IntArray)

;; =============================================================================
;; objects

(def ObjectArray (class (object-array [])))
(derive ::objects ::array)
(defmethod tag ::objects
  [_] ObjectArray)

(comment
  ;; hmm interesting, this is where type information would help
  (do
    (set! *warn-on-reflection* true)
    (let [t (object-array [:black (object-array [:red (object-array [:red 1 2 3]) 3 4]) 5 6])]
      (match [t]
        [(([:black ([:red ([:red _ _ _] ::objects) _ _] ::objects) _ _] ::objects) |
          ([:black ([:red _ _ ([:red _ _ _] ::objects)] ::objects) _ _] ::objects) |
          ([:black _ _ ([:red ([:red _ _ _] ::objects) _ _] ::objects)] ::objects))] :valid
          :else :invalid)))

  ;; 220ms
  (let [^objects t (object-array [:black (object-array [:red (object-array [:red 1 2 3]) 3 4]) 5 6])]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e7]
        (match [t]
          [(([:black ([:red ([:red _ _ _] ::objects) _ _] ::objects) _ _] ::objects) |
            ([:black ([:red _ _ ([:red _ _ _] ::objects)] ::objects) _ _] ::objects) |
            ([:black _ _ ([:red ([:red _ _ _] ::objects) _ _] ::objects)] ::objects))] :valid
            :else :invalid)))))
  )

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
