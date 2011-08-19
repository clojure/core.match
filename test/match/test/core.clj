(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core]
        [match.core.debug])
  (:use [clojure.test]))

(deftest pattern-match-1
  (is (= (let [x true
               y true
               z true]
           (match [x y z]
             [_ false true] 1
             [false true _ ] 2
             [_ _ false] 3
             [_ _ true] 4
             :else 5))
         4)))

(deftest pattern-match-bind-1
  (is (= (let [x 1 y 2 z 4]
           (match [x y z]
             [1 2 b] [:a0 b]
             [a 2 4] [:a1 a]
             :else []))
         [:a0 4])))

(deftest seq-pattern-match-1
  (is (= (let [x [1]]
           (match [x]
             [1] 1
             [[1]] 2
             :else []))
         2)))

(deftest seq-pattern-match-2
  (is (= (let [x [1 2 nil nil nil]]
           (match [x]
             [[1]]     :a0
             [[1 2]]   :a1
             [[1 2 nil nil nil]] :a2
             :else []))
         :a2)))

(deftest seq-pattern-match-bind-1
  (is (= (let [x '(1 2 4)
               y nil
               z nil]
           (match [x y z]
             [[1 2 b] _ _] [:a0 b]
             [[a 2 4] _ _] [:a1 a]
             :else []))
         [:a0 4])))

(deftest seq-pattern-match-wildcard-row
  (is (= (let [x '(1 2 3)]
           (match [x]
             [[1 z 4]] z
             [[_ _ _]] :a2
             :else [])
           :a2))))

(deftest map-pattern-match-1
  (is (= (let [x {:a 1 :b 1}]
           (match [x]
             [{_ :a 2 :b}] :a0
             [{1 :a _ :c}] :a1
             [{3 :c _ :d 4 :e}] :a2
             :else []))
         :a1)))

(deftest map-pattern-match-only-1
  (is (and (= (let [x {:a 1 :b 2}]
                (match [x]
                  [{_ :a 2 :b :only [:a :b]}] :a0
                  [{1 :a _ :c}] :a1
                  [{3 :c _ :d 4 :e}] :a2
                  :else []))
              :a0)
           (= (let [x {:a 1 :b 2 :c 3}]
                (match [x]
                  [{_ :a 2 :b :only [:a :b]}] :a0
                  [{1 :a _ :c}] :a1
                  [{3 :c _ :d 4 :e}] :a2
                  :else []))
              :a1))))

(deftest map-pattern-match-bind-1
  (is (= (let [x {:a 1 :b 2}]
           (match [x]
             [{a :a b :b}] [:a0 a b]
             :else []))
         [:a0 1 2])))

(deftest seq-pattern-match-empty-1
  (is (= (let [x '()]
           (match [x]
             [[]] :a0
             [[1 & r]] [:a1 r]
             :else []))
         :a0)))

(deftest seq-pattern-match-rest-1
  (is (= (let [x '(1 2)]
           (match [x]
             [[1]] :a0
             [[1 & r]] [:a1 r]
             :else []))
         [:a1 '(2)])))

(deftest seq-pattern-match-rest-2
  (is (= (let [x '(1 2 3 4)]
           (match [x]
             [[1]] :a0
             [[_ 2 & [a & b]]] [:a1 a b]
             :else []))
         [:a1 3 '(4)])))

(deftest or-pattern-match-1
  (is (= (let [x 4 y 6 z 9]
           (match [x y z]
             [(1 | 2 | 3) _ _] :a0
             [4 (5 | 6 | 7) _] :a1
             :else []))
         :a1)))

(deftest or-pattern-match-seq-1
  (is (= (let [x '(1 2 3)
               y nil
               z nil]
           (match [x y z]
             [[1 (3 | 4) 3] _ _] :a0
             [[1 (2 | 3) 3] _ _] :a1
             :else []))
         :a1)))

(deftest or-pattern-match-map-2
  (is (= (let [x {:a 3}
               y nil
               z nil]
           (match [x y z]
             [{(1 | 2) :a} _ _] :a0
             [{(3 | 4) :a} _ _] :a1
             :else []))
         :a1)))

(defn div3? [n]
    (= (mod n 3) 0))

(deftest guard-pattern-match-1
  (is (= (let [y '(2 3 4 5)]
           (match [y]
             [[_ (a :when even?) _ _]] :a0
             [[_ (b :when [odd? div3?]) _ _]] :a1
             :else []))
         :a1)))

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

(deftest map-pattern-interop-1
  (is (= (let [d (java.util.Date. 2010 10 1 12 30)]
           (match [d]
             [{2009 :year a :month}] [:a0 a]
             [{(2010 | 2011) :year b :month}] [:a1 b]
             :else []))
         [:a1 10])))

(deftest map-pattern-ocr-order-1
  (is (= (let [v [{:a 1} 2]]
           (match [v]
             [[{2 :a} 2]] :a0
             [[{_ :a} 2]] :a1
             :else []))
         :a1)))

(deftest as-pattern-match-1
  (is (= (let [v [[1 2]]]
           (match [v]
             [[3 1]] :a0
             [[([1 a] :as b)]] [:a1 a b]
             :else []))
         [:a1 2 [1 2]])))

(deftest else-clause-1
  (is (= (let [v [1]]
           (match [v]
                  [2] 1
                  :else 21))
         21)))

(deftest else-clause-seq-pattern-1
  (is (= (let [v [[1 2]]]
           (match [v]
                  [[1 3]] 1
                  :else 21))
         21)))

(deftest else-clause-map-pattern-1
  (is (= (let [v {:a 1}]
           (match [v]
                  [{a :a}] 1
                  :else 21))
         1)))

(deftest else-clause-guard-pattern-1
  (is (= (let [v 1]
           (match [v]
                  [(_ :when even?)] 1
                  :else 21))
         21)))

(deftest else-clause-or-pattern-1
  (is (= (let [v 3]
           (match [v]
                  [(1 | 2)] :a0
                  :else :a1))
         :a1)))

(deftest match-expr-1
  (is (= (->> (range 1 10)
              (map (fn [x]
                     (match [(mod x 3) (mod x 5)]
                       [0 0] "FizzBizz"
                       [0 _] "Fizz"
                       [_ 0] "Buzz"
                       :else (str x)))))
         '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz"))))

(deftest match-single-1
  (is (= (let [x 3]
           (match-1 x
             1 :a0
             2 :a1
             :else :a2))
         :a2)))

(deftest match-single-2
  (is (= (let [x 3]
           (match-1 (mod x 2)
             1 :a0
             2 :a1
             :else :a2))
         :a0)))

#_(deftest match-single-3  ;; TODO precondition that switch-node occurance must be symbols fails.. wrong precondition?
  (is (= (let [x 3]
           (match-1 [1 2] 
                  [2 1] :a0 
                  (b :when #(= (count %) 2)) :a1))
         :a1)))
