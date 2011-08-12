(ns match.perf
  (:refer-clojure :exclude [compile])
  (:use match.core))

(comment
  ;; 2-3ms, literal match
  (let [x 4 y 6 z 9]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x y z ]
           [(1 | 2 | 3) _ _] :a0
           [4 (5 | 6 | 7) _] :a1)))))

  ;; ~600ms, seq match on vector
  (let [x [1 2 nil nil nil]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [[1]]     :a0
           [[1 2]]   :a1
           [[1 2 nil nil nil]] :a2)))))

  ;; ~360ms, seq match on seq
  (let [x '(1 2 nil nil nil)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [[1]]     :a0
           [[1 2]]   :a1
           [[1 2 nil nil nil]] :a2)))))

  ;; 17ms, destructure on vector
  (let [x [1 2 nil nil nil]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [[a b c d e] x])))))

  ;; 550ms, destructure on seq
  (let [x '(1 2 nil nil nil)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [[a b c d e] x])))))

  ;; 170ms, map match
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
        (match [x]
          [{_ :a 2 :b :only [:a :b]}] :a0
          [{1 :a _ :c}] :a1
          [{3 :c _ :d 4 :e}] :a2)))))

  ;; 300ms, map destructure
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{a :a b :b c :c} x])))))

  ;; 300ms, map match with only
  (let [x {:a 1 :b 2}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [{_ :a 2 :b :only [:a :b]}] :a0
           [{1 :a _ :c}] :a1
           [{3 :c _ :d 4 :e}] :a2)))))
 )