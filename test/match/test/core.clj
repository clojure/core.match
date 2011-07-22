(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest simple-boolean-compile-dag
         (is
           (= (compile (build-matrix []))
              (fail-node))
           "Base case")
         (is
           (= (compile (build-matrix [x]
                                     [true] 1))
              (switch-node 'x
                           [[(pattern true) (leaf-node 1)]
                            [wildcard (fail-node)]]))
           "Simple leaf node")
         (is
           (= (compile (build-matrix [x y]
                                     [true false] 1
                                     [false true] 2))
              (switch-node 'x
                           [[(pattern true) (switch-node 'y
                                               [[(pattern false) (leaf-node 1)]
                                                [wildcard (fail-node)]])]
                            [(pattern false) (switch-node 'y
                                                [[(pattern true) (leaf-node 2)]
                                                 [wildcard (fail-node)]])]
                            [wildcard (fail-node)]]))
           "Two rows")
         (is
           (= (compile (build-matrix [x y]
                                     [_ _] 1
                                     [true true] 2))
              (switch-node 'x 
                           [[(pattern true) (switch-node 'y [[(pattern true) (leaf-node 2)] 
                                                             [wildcard (fail-node)]])]
                            [wildcard (switch-node 'y [[wildcard (leaf-node 1)] 
                                                       [wildcard  (fail-node)]])] 
                            [wildcard (fail-node)]]))))

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
                               [[wildcard (leaf-node 1)]])
                to-clj)
              '(clojure.core/cond 
                 true 1))
           "Switch node and wildcards")
        (is 
          (= (-> (switch-node 'x
                              [[wildcard (leaf-node 1)]
                               [wildcard (fail-node)]])
               to-clj)
             '(clojure.core/cond 
                true 1 
                true (throw (java.lang.Exception. "Found FailNode"))))
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
                                [_] 1)
                compile
                to-clj)
              '(clojure.core/cond 
                 true 1 
                 true (throw (java.lang.Exception. "Found FailNode"))))
           "Early match case"))
