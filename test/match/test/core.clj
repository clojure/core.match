(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest pattern-match-1
  (is (= (let [x true
               y true
               z true]
           (match [x y z]
             [_ false true] 1
             [false true _ ] 2
             [_ _ false] 3
             [_ _ true] 4))
         4)))

(deftest pattern-match-bind-1
  (is (= (let [x 1 y 2 z 4]
           (match [x y z]
             [1 2 b] [:a0 b]
             [a 2 4] [:a1 a]))
         [:a0 4])))

(deftest seq-pattern-match-1
  (is (= (let [x [1]]
           (match [x]
             [1] 1
             [[1]] 2))
         2)))

(deftest seq-pattern-match-2
  (is (= (let [x [1 2 nil nil nil]]
           (match [x]
             [[1]]     :a0
             [[1 2]]   :a1
             [[1 2 nil nil nil]] :a2))
         :a2)))

(deftest seq-pattern-match-bind-1
  (is (= (let [x '(1 2 4)]
           (match [x y z]
             [[1 2 b]] [:a0 b]
             [[a 2 4]] [:a1 a]))
         [:a0 4])))

(deftest seq-pattern-match-wilcard-row
  (is (= (let [x '(1 2 3)]
           (match [x]
             [[1 z 4]] z
             [[_ _ _]] :a2)
           :a2))))

(deftest map-pattern-match-1
  (is (= (let [x {:a 1 :b 1}]
           (match [x]
             [{_ :a 2 :b}] :a0
             [{1 :a _ :c}] :a1
             [{3 :c _ :d 4 :e}] :a2))
         :a1)))

(deftest map-pattern-match-only-1
  (is (and (= (let [x {:a 1 :b 2}]
                (match [x]
                  [{_ :a 2 :b :only [:a :b]}] :a0
                  [{1 :a _ :c}] :a1
                  [{3 :c _ :d 4 :e}] :a2))
              :a0)
           (= (let [x {:a 1 :b 2 :c 3}]
                (match [x]
                  [{_ :a 2 :b :only [:a :b]}] :a0
                  [{1 :a _ :c}] :a1
                  [{3 :c _ :d 4 :e}] :a2))
              :a1))))

(deftest map-pattern-match-bind-1
  (is (= (let [x {:a 1 :b 2}]
           (match [x]
             [{a :a b :b}] [:a0 a b]))
         [:a0 1 2])))

(deftest seq-pattern-match-empty-1
  (is (= (let [x '()]
           (match [x]
             [[]] :a0
             [[1 & r]] [:a1 r]))
         :a0)))

(deftest seq-pattern-match-rest-1
  (is (= (let [x '(1 2)]
           (match [x]
             [[1]] :a0
             [[1 & r]] [:a1 r]))
         [:a1 '(2)])))

(deftest seq-pattern-match-rest-2
  (is (= (let [x '(1 2 3 4)]
           (match [x]
             [[1]] :a0
             [[_ 2 & [a & b]]] [:a1 a b]))
         [:a1 3 '(4)])))

(deftest or-pattern-match-1
  (is (= (let [x 4 y 6 z 9]
           (match [x y z ]
             [(1 | 2 | 3) _ _] :a0
             [4 (5 | 6 | 7) _] :a1))
         :a1)))

(deftest or-pattern-match-seq-1
  (is (= (let [x '(1 2 3)]
           (match [x y z ]
             [[1 (3 | 4) 3]] :a0
             [[1 (2 | 3) 3]] :a1))
         :a1)))

(deftest or-pattern-match-map-2
  (is (= (let [x {:a 3}]
           (match [x y z ]
             [{(1 | 2) :a}] :a0
             [{(3 | 4) :a}] :a1))
         :a1)))