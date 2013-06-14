(ns clojure.core.match.test.impl
  (:refer-clojure :exclude [compile])
  (:use clojure.core.match.protocols
        clojure.core.match
        clojure.core.match.debug
        clojure.core.match.regex)
  (:use [clojure.test]))

(deftest pattern-equality
  (testing "wildcard patterns"
    (is (true? (= (wildcard-pattern) (wildcard-pattern))))
    (is (true? (= (wildcard-pattern 'a) (wildcard-pattern 'a))))
    (is (false? (= (wildcard-pattern 'a) (wildcard-pattern 'b)))))
  (testing "literal patterns"
    (is (true? (= (literal-pattern 1) (literal-pattern 1))))
    (is (false? (= (literal-pattern 1) (literal-pattern 2)))))
  (testing "rest patterns"
    (is (true? (= (rest-pattern 'a) (rest-pattern 'a))))
    (is (false? (= (rest-pattern 'a) (rest-pattern 'b)))))
  (testing "map patterns"
    (is (true? (= (map-pattern '{:a a}) (map-pattern '{:a a}))))
    (is (false? (= (map-pattern '{:a a}) (map-pattern '{:a b})))))
  (testing "vector patterns"
    (is (true? (= (vector-pattern '[1 a 2]) (vector-pattern '[1 a 2]))))
    (is (false? (= (vector-pattern '[1 a 2]) (vector-pattern '[1 a 3])))))
  (testing "or patterns"
    (is (true? (= (or-pattern [(literal-pattern 1) (literal-pattern 2)])
                  (or-pattern [(literal-pattern 1) (literal-pattern 2)]))))
    (is (false? (= (or-pattern [(literal-pattern 1) (literal-pattern 2)])
                   (or-pattern [(literal-pattern 1) (literal-pattern 3)])))))
  (testing "guard patterns"
    (is (true? (= (guard-pattern 'a #{even?})
                  (guard-pattern 'a #{even?}))))
    (is (false? (= (guard-pattern 'a #{even?})
                   (guard-pattern 'a #{odd?})))))
  (testing "predicate patterns"
    (is (true? (= (predicate-pattern 'a #{even?})
                  (predicate-pattern 'a #{even?}))))
    (is (false? (= (predicate-pattern 'a #{even?})
                   (predicate-pattern 'a #{odd?}))))))

(deftest pattern-row-equality
  (is (true? (= (pattern-row [(literal-pattern 1) (literal-pattern 2)] :a0)
                (pattern-row [(literal-pattern 1) (literal-pattern 2)] :a0))))
  (is (false? (= (pattern-row [(literal-pattern 1) (literal-pattern 2)] :a0)
                 (pattern-row [(literal-pattern 1) (literal-pattern 3)] :a0)))))

(deftest pattern-matrix-equality
  (let [m0 (build-matrix [x y z]
             [_     false true ] (recur x y z 1)
             [false true  _    ] (recur x y z 2)
             [_     _     false] (recur x y z 3)
             [_     _     true ] (recur x y z 4)
             :else 5)
        m1 (build-matrix [x y z]
             [_     false true ] (recur x y z 1)
             [false true  _    ] (recur x y z 2)
             [_     _     false] (recur x y z 3)
             [_     _     true ] (recur x y z 4)
             :else 5)]
    (is (true? (= m0 m1)))))

