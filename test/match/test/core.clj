(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest simple-compile-dag
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


