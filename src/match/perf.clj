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

  ;; ~300ms, seq match on vector
  (let [x [1 2 nil nil nil]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [[1]]     :a0
           [[1 2]]   :a1
           [[1 2 nil nil nil]] :a2)))))

  ;; ~150ms, seq match on seq
  (let [x '(1 2 nil nil nil)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [[1]]     :a0
           [[1 2]]   :a1
           [[1 2 nil nil nil]] :a2)))))

  ;; ~13ms, destructure on vector
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

  ;; 200ms, map match
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
        (match [x]
          [{_ :a 2 :b :only [:a :b]}] :a0
          [{1 :a _ :c}] :a1
          [{3 :c _ :d 4 :e}] :a2)))))

  ;; 200, map destructure
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{a :a b :b c :c} x])))))

  ;; small maps are not a good indicator
  ;; 150ms, map pattern
  (let [x (zipmap (map keyword (take 40 (repeatedly gensym)))
                  (map keyword (take 40 (repeatedly gensym))))
        x (assoc x :a 1)
        x (assoc x :c 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
        (match [x]
          [{_ :a 2 :b :only [:a :b]}] :a0
          [{1 :a _ :c}] :a1
          [{3 :c _ :d 4 :e}] :a2)))))

  ;; 100ms, map destructure
  (let [x (zipmap (map keyword (take 40 (repeatedly gensym)))
                  (map keyword (take 40 (repeatedly gensym))))
        x (assoc x :a 1)
        x (assoc x :c 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{a :a c :c} x])))))

  ;; 400ms, map match with only
  (let [x {:a 1 :b 2}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [{_ :a 2 :b :only [:a :b]}] :a0
           [{1 :a _ :c}] :a1
           [{3 :c _ :d 4 :e}] :a2)))))

   ;; 240ms
  (do
    (extend-type java.util.Date
      IMatchLookup
      (val-at* [this k not-found]
        (case k
          :year    (.getYear this)
          :month   (.getMonth this)
          :date    (.getDate this)
          :hours   (.getHours this)
          :minutes (.getMinutes this)
          not-found)))
    (let [d (java.util.Date. 2010 10 1 12 30)]
      (dotimes [_ 10]
        (time
         (dotimes [_ 1e6]
           (match [d]
             [{2009 :year a :month}] a
             [{(2010 | 2011) :year b :month}] b))))))

  ;; satisfies call is a bit slow
  (let [d (java.util.Date. 2010 10 1 12 30)]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e6]
        (let [y (.getYear d)]
          (if (satisfies? IMatchLookup d)
           (if (= 2009 y)
             nil
             (if (= 2010 y)
               (.getMonth d)
               nil))))))))
 )