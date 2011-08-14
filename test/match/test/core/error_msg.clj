(ns match.test.core.error-msg
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core]
        [match.core.debug])
  (:use [clojure.test]))

(deftest match-errors-occurances-symbol
  (is (thrown-with-msg?
        AssertionError
        #"Occurances must be in a vector. Try changing x to \[x\]"
        (m-to-clj x
                  [1] :a1))))

(deftest match-errors-occurances-list
  (is (thrown-with-msg?
        AssertionError
        #"Occurances must be in a vector. \(x\) is not a vector"
        (m-to-clj (x)
                  [1] :a1))))

(deftest match-errors-pattern-row1
  (is (thrown-with-msg?
        AssertionError
        #"Pattern rows must be wrapped in \[\]. Try changing 1 to \[1\]"
        (m-to-clj [x]
                  1 :a1))))

(deftest match-errors-pattern-row-list1
  (is (thrown-with-msg?
        AssertionError
        #"Pattern rows must be wrapped in \[\]. Try changing \(1\) to \[\(1\)\]. Note: pattern rows are not patterns. They cannot be wrapped in a :when guard, for example"
        (m-to-clj [x]
                  (1) :a1))))

(deftest match-errors-pattern-row-list2
  (is (thrown-with-msg?
        AssertionError
        #"Pattern rows must be wrapped in \[\]. Try changing \(1\) to \[\(1\)\]. Note: pattern rows are not patterns. They cannot be wrapped in a :when guard, for example"
        (m-to-clj [x]
                  [2] :a0
                  (1) :a1))))

(deftest match-errors-uneven-clauses1
  (is (thrown-with-msg?
        AssertionError
        #"Uneven number of Pattern Rows. The last form `\[1\]` seems out of place."
        (m-to-clj [x]
                  [1]))))

(deftest match-errors-uneven-clauses2
  (is (thrown-with-msg?
        AssertionError
        #"Uneven number of Pattern Rows. The last form `\[1\]` seems out of place."
        (m-to-clj [x]
                  [1] :a1
                  [1]))))

(deftest match-list-syntax-error
  (is (thrown-with-msg?
        AssertionError
        #"Invalid list syntax `:what` in \(1 :what a\)."
        (m-to-clj [x]
                  [(1 :what a)] :a1))))

(deftest match-else-clause-error
  (is (thrown-with-msg?
        AssertionError
        #":else form only allowed on final pattern row"
        (m-to-clj [x]
                  :else 1
                  [1] 1
                  :else 1))))

