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

  ;; 50ms, vector match
  (let [x [1 1 2]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 2] :vec)] :a0
           [([1 1 3] :vec)] :a1
           [([1 2 3] :vec)] :a2)))))

  (def IntArray (class (int-array [])))

  (defmethod vtest-inline ::ints
    [_ ocr] `(instance? IntArray ~ocr))
  (defmethod vnth-inline ::ints
    [_ ocr i] `(aget ~ocr ~i))
  (defmethod vnth-offset-inline ::ints
    [_ ocr i offset] `(aget ~ocr (unchecked-add ~i ~offset)))
  (defmethod vsubvec-inline ::ints
    [_ ocr i] ocr)

  ;; 60ms
  (let [x (int-array [1 2 3])]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
        (match [x]
          [([_ _ 2] :vec ::ints)] :a0
          [([1 1 3] :vec ::ints)] :a1
          [([1 2 3] :vec ::ints)] :a2)))))

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

  (defmethod vtest-inline ::bits
    [_ ocr] `(instance? Long ~ocr))
  (defmethod vnth-inline ::bits
    [_ ocr i] `(bit-shift-right (bit-and ~ocr (bit-shift-left 1 ~i)) ~i))

  (let [x 5]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 1 1] :vec ::bits)] :a0
           [([1 0 1 _] :vec ::bits)] :a1)))))


  ;; 90ms, destructure vector
  (let [x [1 2 3]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (let [[a b c] x])))))

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
  ;; 80ms, map pattern
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

  ;; 130ms, map destructure
  (let [x (zipmap (map keyword (take 40 (repeatedly gensym)))
                  (map keyword (take 40 (repeatedly gensym))))
        x (assoc x :a 1)
        x (assoc x :c 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (let [{a :a c :c} x])))))

  ;; 340ms, map match with only
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
           [{3 :a 4 :c}] :a0
           [{3 :d 4 :e}] :a1
           [{1 :a}] :a2
           :else :a3)))))

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