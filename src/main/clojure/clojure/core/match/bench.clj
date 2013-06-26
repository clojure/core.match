(ns ^{:skip-wiki true}
  clojure.core.match.bench
  (:use [clojure.core.match :only [match]]))

(comment
  ;; 1.7ghz MBA timings

  ;; ~200ms
  (dotimes [_ 5]
    (time
      (dotimes [i 1e6]
        (let [x (zero? (mod i 2))
              y (zero? (mod i 3))
              z (zero? (mod i 5))]
          (match [x y z]
            [_ false true] 1
            [false true _ ] 2
            [_ _ false] 3
            [_ _ true] 4
            :else 5)))))

  ;; ~50ms
  (let [x {:a 1 :b 1}]
    (dotimes [_ 5]
      (time
        (dotimes [_ 1e6]
          (match [x]
            [{:a _ :b 2}] :a0
            [{:a 1 :b 1}] :a1
            [{:c 3 :d _ :e 4}] :a2
            :else nil)))))

  ;; ~500ms
  (let [n [:black [:red [:red 1 2 3] 3 4] 5 6]]
    (dotimes [_ 5]
      (time
        (dotimes [_ 1e6]
          (match [n]
            [(:or [:black [:red [:red a x b] y c] z d]
                  [:black [:red a x [:red b y c]] z d]
                  [:black a x [:red [:red b y c] z d]]
                  [:black a x [:red b y [:red c z d]]])] :balance
            :else :valid)))))
  )
