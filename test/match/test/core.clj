(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest pattern-sorted-set-test
  (is (-> (sorted-set (literal-pattern nil)
                      (literal-pattern nil))
          count
          (= 1))))

(deftest type-pattern-precondition-test
  (is (thrown? java.lang.AssertionError
               (type-pattern 1))))

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
                  [(1)] 2))
         2)))

(deftest seq-pattern-match-2
  (is (= (let [x [1 2 nil nil nil]]
           (match [x]
                  [(1)]     :a0
                  [(1 2)]   :a1
                  [(1 2 nil nil nil)] :a2))
         :a2)))

(deftest seq-pattern-match-bind-1
  (is (= (let [x '(1 2 4)]
           (match [x y z]
                  [(1 2 b)] [:a0 b]
                  [(a 2 4)] [:a1 a]))
         [:a0 4])))

(deftest seq-pattern-match-wilcard-row
  (is (= (let [x '(1 2 3)]
           (match [x]
                  [(1 z 4)] z
                  [(_ _ _)] :a2)
           :a2))))