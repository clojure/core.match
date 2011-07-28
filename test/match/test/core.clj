(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest match?-tests
         (is
           (match? (wildcard-pattern) 'blah))
         (is
           (match? (literal-pattern true) true))
         (is
           (not (match? (literal-pattern true) false)))
         (is
           (not (match? (type-pattern clojure.lang.IPersistentVector) true)))
         (is
           (match? (type-pattern clojure.lang.IPersistentVector) [1])))


(deftest equality-tests
         (is
           (= (switch-node 'x [])
              (switch-node 'x [])))
         (is 
           (not= (switch-node 'x [1])
                 (switch-node 'x [])))
         (is
           (= (leaf-node 'x)
              (leaf-node 'x)))
         (is
           (= (fail-node)
              (fail-node))))

(deftest isa?-compile-dag
         (is
           (= (-> (build-matrix [x]
                                [true] 1
                                [(isa? java.lang.Object)] 2)
                compile)
              (switch-node 'x
                           [[(type-pattern java.lang.Object)  (leaf-node 2)]
                            [(literal-pattern true) (leaf-node 1)]
                            [(wildcard-pattern) (fail-node)]]))))

(deftest isa?-to-clj
         (is
           (= (-> (build-matrix [x]
                                [true] 1
                                [(isa? java.lang.Object)] 2)
                compile
                to-clj)
              `(cond 
                 (~(type-pattern Object) ~'x) 2 
                 (~(literal-pattern true) ~'x) 1 
                 (~(wildcard-pattern) ~'x) (throw (java.lang.Exception. "Found FailNode"))))))

(deftest simple-boolean-compile-dag
         (is
           (= (compile (build-matrix []))
              (fail-node))
           "Base case")
         (is
           (= (compile (build-matrix [x]
                                     [true] 1))
              (switch-node 'x
                           [[(literal-pattern true) (leaf-node 1)]
                            [(wildcard-pattern) (fail-node)]]))
           "Simple leaf node")
         (is
           (= (compile (build-matrix [x y]
                                     [true false] 1
                                     [false true] 2))
              (switch-node 'x
                           [[(literal-pattern false) (switch-node 'y
                                                [[(literal-pattern true) (leaf-node 2)]
                                                 [(wildcard-pattern) (fail-node)]])]
                            [(literal-pattern true) (switch-node 'y
                                               [[(literal-pattern false) (leaf-node 1)]
                                                [(wildcard-pattern) (fail-node)]])]
                            [(wildcard-pattern) (fail-node)]]))
           "Two rows")
         (is
           (= (compile (build-matrix [x y]
                                     [_ _] 1
                                     [true true] 2))
              (leaf-node 1))))

(deftest test-to-clj
         (is 
           (= (-> (leaf-node true)
                to-clj)
              true)
           "Leaf node")
         (is
           (= (-> (fail-node)
                to-clj)
              '(throw (java.lang.Exception. "Found FailNode")))
           "Fail node")
         (is 
           (= (-> (switch-node 'x
                               [[(wildcard-pattern) (leaf-node 1)]])
                to-clj)
              `(cond
                 (~(wildcard-pattern) ~'x) 1))
           "Switch node and wildcards")
        (is 
          (= (-> (switch-node 'x
                              [[(wildcard-pattern) (leaf-node 1)]
                               [(wildcard-pattern) (fail-node)]])
               to-clj)
             `(cond 
                (~(wildcard-pattern) ~'x) 1 
                (~(wildcard-pattern) ~'x) (throw (java.lang.Exception. "Found FailNode"))))
          "Switch node with early match case"))

(deftest simple-boolean-to-clj
         (is
           (= (-> (build-matrix [])
                compile
                to-clj)
              '(throw (java.lang.Exception. "Found FailNode")))
           "Simple failure")
         (is
           (= (-> (build-matrix [x]
                                [_] 1
                                [_] 2)
                compile
                to-clj)
              1)
           "Early match case"))

(deftest pattern-equals-test
         (is
           (= (literal-pattern true)
              (literal-pattern true)))
         (is
           (not= (literal-pattern true)
                 (literal-pattern false)))
         (is
           (not= (literal-pattern [1])
                 (literal-pattern [1 2])))
         (is
           (not= (literal-pattern 1)
                 (literal-pattern [1])))
         (is
           (= (vector-pattern [1 2 3])
              (vector-pattern [1])))
         (is
           (not= (literal-pattern [1 2])
                 (vector-pattern  [1 2])))
         (is
           (not= (type-pattern clojure.lang.IPersistentVector)
                 (vector-pattern  [1 2]))))


(deftest pattern-comparable-test
         (is (-> (sorted-set (literal-pattern nil)
                             (literal-pattern nil))
               count
               (= 1))))

(deftest type-pattern-precondition-test
         (is
           (thrown? java.lang.AssertionError
                    (type-pattern 1))))

(deftest vector-pattern-precondition-test
         (is
           (thrown? java.lang.AssertionError
                    (vector-pattern 1)))
         (is
           (thrown? java.lang.AssertionError
                    (vector-pattern [1] -1)))
         (is
           (thrown? java.lang.AssertionError
                    (vector-pattern [1] "a"))))


(deftest seq-pattern-match
         (is
           (= (-> (build-matrix [x]
                                [[1]] 1
                                [1] 2)
                compile)
              (switch-node 'x
                           [[(literal-pattern 1) (leaf-node 2)]
                            [(type-pattern clojure.lang.Sequential) (switch-node 'x0
                                                                                 [[(literal-pattern 1) (switch-node 'x1
                                                                                                                    [[(literal-pattern nil) (leaf-node 1)]
                                                                                                                     [(wildcard-pattern) (fail-node)]])]
                                                                                  [(wildcard-pattern) (fail-node)]])]
                            [(wildcard-pattern) (fail-node)]])))
         ;; TODO enable
         #_(is
           (= (-> (build-matrix [x]
                                [[1]] 1
                                [[1 2]] 2
                                [1] 3)
                compile)
              (switch-node 'x
                           [[(literal-pattern coll?)
                             (switch-node 'x0
                                          [[(literal-pattern 1) (switch-node 'x1
                                                                     [[(literal-pattern nil)   (leaf-node 1)]
                                                                      [(literal-pattern 2)   (switch-node 'x2
                                                                                                  [[(literal-pattern nil) (leaf-node 2)]
                                                                                                   [(wildcard-pattern) (fail-node)]])]
                                                                      [(wildcard-pattern) (fail-node)]])]
                                           [(wildcard-pattern) (fail-node)]])]
                            [(literal-pattern 1) (leaf-node 3)]
                            [(wildcard-pattern) (fail-node)]]))))

(deftest column-constructors-test
         (is
           (= (-> (build-matrix [x]
                                [_] 1)
                (column-constructors 0))
              (sorted-set)))
         (is
           (= (-> (build-matrix [x y]
                                [1 1] 1
                                [[1 2] 1] 2)
                (column-constructors 0))
              (sorted-set (literal-pattern 1)
                          (vector-pattern [1 2])))))

(deftest specialize-test
         (is (= (-> (build-matrix [x y]
                                  [true false] 1
                                  [false true] 2)
                  (specialize (literal-pattern true))
                  compile)
                (-> (build-matrix [y]
                                  [false] 1)
                  compile))))
