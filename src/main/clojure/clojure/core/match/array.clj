;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; WARNING: this namespace is experimental
(ns ^{:skip-wiki true}
  clojure.core.match.array
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :as m]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Shared

(derive ::m/array ::m/vector)
(defmethod nth-inline ::m/array
  [t ocr i]
  `(aget ~ocr ~i))
(defmethod count-inline ::m/array
  [t ocr]
  `(alength ~ocr))
(defmethod subvec-inline ::m/array
  ([_ ocr start] ocr)
  ([_ ocr start end] ocr))

;; =============================================================================
;; ints

(derive ::m/ints ::m/array)
(defmethod tag ::m/ints
  [_] "[I")

;; =============================================================================
;; objects

(derive ::m/objects ::m/array)
(defmethod tag ::m/objects
  [_] "[Ljava.lang.Object;")

(comment
  ;; specialize based on type hints in match
  (let [x (int-array [1 2 3])]
    (match [^ints x]
      [[_ _ 2]] :a0
      [[1 1 3]] :a1
      [[1 2 3]] :a2
      :else :a3))

  ;; FIXME
  (let [x (int-array [1 2 3 4])]
    (match [^ints x]
      [[_ _ 2 & _]] :a0
      [[1 1 3 & _]] :a1
      [[1 2 3 & _]] :a2
      :else :a3))
  
  (let [x (int-array [1 2 3])]
    (match [x]
      [([_ _ 2] ::m/ints)] :a0
      [([1 1 3] ::m/ints)] :a1
      [([1 2 3] ::m/ints)] :a2
      :else :a3))
  
  ;; ~100ms
  (let [x (int-array [1 2 3])]
    (dotimes [_ 5]
      (time
        (dotimes [_ 1e7]
          (match [^ints x]
            [[_ _ 2]] :a0
            [[1 1 3]] :a1
            [[1 2 3]] :a2)))))

  ;; offsets
  ;; FIXME: needs to account for offset - David
  (let [x (int-array [1 1 2 3])
        o 1]
    (match [x]
      [([_ _ 2] ::m/ints :offset o)] :a0
      [([1 1 3] ::m/ints :offset o)] :a1
      [([1 2 3] ::m/ints :offset o)] :a2))

  ;; 80ms
  (let [x (int-array [1 1 2 3])
        o 1]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 2] ::m/ints :offset o)] :a0
           [([1 1 3] ::m/ints :offset o)] :a1
           [([1 2 3] ::m/ints :offset o)] :a2)))))

  (do
    (set! *warn-on-reflection* true)
    
    (defmacro asets [a vs]
      `(do
         ~@(map (fn [a b c] (concat a (list b c)))
                (repeat `(aset ~a)) (range (count vs)) vs)
         ~a))
    
    (defn B [l v r]
      (let [^objects o (make-array Object 4)]
        (asets o [:black l v r])))

    (defn R [l v r]
      (let [^objects o (make-array Object 4)]
        (asets o [:red l v r])))
    
    (defn balance-array [node]
      (matchv ::objects [node]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])] :balance
         :else :balanced))

    ;; 200ms
    (let [node (B nil nil (R nil nil (R nil nil nil)))]
      (dotimes [_ 10]
        (time
         (dotimes [_ 1e7]
           (balance-array node)))))

    #_(let [node (B nil nil (R nil nil (B nil nil nil)))]
      (balance-array node))
    )
 )
