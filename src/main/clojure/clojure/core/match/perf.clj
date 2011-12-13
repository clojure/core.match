(ns clojure.core.match.perf
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :as m]))

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

  ;; 50ms, vector match
  (let [x [1 1 2]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 2] ::clojure.core.match/vector)] :a0
           [([1 1 3] ::clojure.core.match/vector)] :a1
           [([1 2 3] ::clojure.core.match/vector)] :a2)))))

  ;; 2.3s
  (let [v [1 2 3 4]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [v]
           [([1 & r] ::clojure.core.match/vector)] :a0
           :else :a1)))))

  ;; 270ms
  (let [v [1 2 3 4]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [v]
           [([1 1 3 & r] ::clojure.core.match/vector)] :a0
           [([1 2 & r] ::clojure.core.match/vector)] :a1
           :else :a3)))))

  ;; 90ms, destructure vector
  (let [x [1 2 3]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (let [[a b c] x])))))

  ;; 1.3s
  (let [x [1 2 3 4]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (let [[a & r] x])))))

  ;; 200ms, map match
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
        (match [x]
          [({:a _ :b 2} :only [:a :b])] :a0
          [{:a 1 :c _}] :a1
          [{:c 3 :d _ :e 4}] :a2)))))

  ;; 200, map destructure
  (let [x {:a 1 :b 2 :c 3}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{:a a :b b :c c} x])))))

  ;; small maps are not a good indicator
  ;; 80ms, map pattern
  (let [x (zipmap (map keyword (take 40 (repeatedly gensym)))
                  (map keyword (take 40 (repeatedly gensym))))
        x (assoc x :a 1)
        x (assoc x :c 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
        (match [x]
          [({:a _ :b 2} :only [:a :b])] :a0
          [{:a 1 :c _}] :a1
          [{:c 3 :d _ :e 4}] :a2)))))

  ;; 130ms, map destructure
  (let [x (zipmap (map keyword (take 40 (repeatedly gensym)))
                  (map keyword (take 40 (repeatedly gensym))))
        x (assoc x :a 1)
        x (assoc x :c 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{:a a :c c} x])))))

  ;; 340ms, map match with only
  (let [x {:a 1 :b 2}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [({:a _ :b 2 } :only [:a :b])] :a0
           [{:a 1 :c _}] :a1
           [{:c 3 :d _ :e 4}] :a2)))))

   ;; 240ms
  (do
    (extend-type java.util.Date
      IMatchLookup
      (val-at [this k not-found]
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
             [{:year 2009 :month a}] a
             [{:year (2010 | 2011) :month b}] b))))))

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

  (deftype Foo [a b]
    clojure.lang.ILookup
    (valAt [this k]
      (.valAt this k nil))
    (valAt [this k not-found]
      (case k
        :a a
        :b b
        not-found)))

  ;; 130ms
  (let [x (Foo. 1 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (match [x]
           [{:a 3 :c 4}] :a0
           [{:d 3 :e 4}] :a1
           [{:a 1}] :a2
           :else :a3)))))

  ;; 3s
  ;; hmm we could optimize literals w/ case
  (let [x '(:tenth foo)]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e7]
        (match [x]
          [[:first & r]] :a0
          [[:second & r]] :a1
          [[:third & r]] :a2
          [[:fourth & r]] :a3
          [[:fifth & r]] :a4
          [[:sixth & r]] :a5
          [[:seventh & r]] :a6
          [[:eigth & r]] :a7
          [[:ninth & r]] :a8
          [[:tenth & r]] :a9)))))

  ;; 100ms
  (let [x '(:tenth foo)]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e7]
        (case (first x)
          :first   :a0
          :second  :a1
          :third   :a2
          :fourth  :a3
          :fifth   :a4
          :sixth   :a5
          :seventh :a6
          :eigth   :a7
          :ninth   :a8
          :tenth   :a9
          nil)))))

  ;; 173ms, ok, when the multimethod is first defined 
  (let [^objects v (object-array [zero? :zero even? :even odd? :odd])
        f (fn [x] (cond
                   ((aget v 0) x) (aget v 1)
                   ((aget v 2) x) (aget v 3)
                   ((aget v 4) x) (aget v 5)
                   :else nil))]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f 3)))))

  ;; 680ms, much slower, perhaps because the callsite keeps changing?
  (let [^objects v (object-array [zero? :zero even? :even odd? :odd])
        f (fn [x] (loop [i 0]
                    (cond
                     (> i 5) nil
                     ((aget v i) x) (aget v (inc i))
                     :else (recur (+ i 2)))))]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f 3)))))

  ;; ah just like deftype
  ;; inline dispatching be optimized
  ;; we'll have to see how feasible this is
  (defpred foo
    [0] :a0
    [(a :when even?)] :a1
    [(a :when div3?)] :a2)

  ;; this should be namespace local
  ;; also this needs to be inside
  ;; so we should use the slower
  ;; dispatch table
  (extend-pred foo
    [1] :new-a)
 )
