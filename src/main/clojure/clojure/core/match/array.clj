;; WARNING: this namespace is experimental

(ns clojure.core.match.array
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :as m]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Shared

(derive ::array ::clojure.core.match/vector)
(defmethod nth-inline ::array
  [t ocr i]
  `(aget ~ocr ~i))
(defmethod count-inline ::array
  [t ocr]
  `(alength ~ocr))
(defmethod subvec-inline ::array
  ([_ ocr start] ocr)
  ([_ ocr start end] ocr))

;; =============================================================================
;; ints

(derive ::ints ::array)
(defmethod tag ::ints
  [_] "[I")

;; =============================================================================
;; objects

(derive ::objects ::array)
(defmethod tag ::objects
  [_] "[Ljava.lang.Object;")

(comment
  ;; specialize based on type hints in match
  (let [x (int-array [1 2 3])]
    (match [^ints x]
      [[_ _ 2]] :a0
      [[1 1 3]] :a1
      [[1 2 3]] :a2
      :else :a3))
  
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
        (dotimes [_ 1e6]
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

  (do
    (set! *warn-on-reflection* true)
    
    (defmacro asets [a vs]
      `(do
         ~@(map (fn [a b c] (concat a (list b c)))
                (repeat `(aset ~a)) (range (count vs)) vs)
         ~a))
    
    (defn B ^objects [l v r]
      (let [^objects o (make-array Object 4)]
        (asets o [:black l v r])))

    (defn R ^objects [l v r]
      (let [^objects o (make-array Object 4)]
        (asets o [:red l v r])))
    
    (defn balance-array [^objects node]
      (matchv ::objects [node]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])] :balance
         :else :balanced))

    ;; 200ms
    (let [^objects node (B nil nil (R nil nil (R nil nil nil)))]
      (dotimes [_ 10]
        (time
         (dotimes [_ 1e7]
           (balance-array node)))))

    #_(let [^objects node (B nil nil (R nil nil (B nil nil nil)))]
      (balance-array node))
    )
 )
