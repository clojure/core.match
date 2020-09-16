;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.match.test.core.error-msg
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.match]
        [clojure.core.match.debug])
  (:use [clojure.test]))

(deftest match-errors-occurrences-symbol
  (is (thrown-with-msg?
        AssertionError
        #"Occurrences must be in a vector. Try changing x to \[x\]"
        (m-to-clj x
          [1] :a1))))

(deftest match-errors-occurrences-list
  (is (thrown-with-msg?
        AssertionError
        #"Occurrences must be in a vector. \(x\) is not a vector"
        (m-to-clj (x)
          [1] :a1))))

(deftest match-errors-pattern-row1
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: Pattern rows must be wrapped in \[\]. Try changing 1 to \[1\]"
        (m-to-clj [x]
          1 :a1))))

(deftest match-errors-pattern-row-list1
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: Pattern rows must be wrapped in \[\]. Try changing \(1\) to \[\(1\)\]. Note: pattern rows are not patterns. They cannot be wrapped in a :when guard, for example"
        (m-to-clj [x]
          (1) :a1))))

(deftest match-errors-pattern-row-list2
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 2: Pattern rows must be wrapped in \[\]. Try changing \(1\) to \[\(1\)\]. Note: pattern rows are not patterns. They cannot be wrapped in a :when guard, for example"
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
        #"^Invalid list syntax :what in \(1 :what a\)"
        (m-to-clj [x]
          [(1 :what a)] :a1))))

(deftest match-else-clause-error
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: :else form only allowed on final pattern row"
        (m-to-clj [x]
          :else 1
          [1] 1
          :else 1))))

(deftest match-differing-patterns
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: Pattern row has differing number of patterns. \[1 2\] has 2 pattern/s, expecting 1 for occurrences \[x\]"
        (m-to-clj [x]
          [1 2] 1
          :else 1))))

(deftest match-duplicate-wildcards
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: Pattern row reuses wildcards in \[a a\].  The following wildcards are ambiguous: a.  There's no guarantee that the matched values will be same.  Rename the occurrences uniquely."
        (m-to-clj [x y]
          [a a] a
          :else 1))))

(deftest match-duplicate-wildcards2
  (is (thrown-with-msg?
        AssertionError
        #"Pattern row 1: Pattern row reuses wildcards in \[.*\].  The following wildcards are ambiguous: aa, x.  There's no guarantee that the matched values will be same.  Rename the occurrences uniquely."
        (m-to-clj [xx yy]
          [x (:or [:black [:red [:red a x b] y c] z d]
                  [:black [:red a x [:red b y c]] z d]
                  [:black a x [:red [:red b y c] z d]]
                  [:black aa x [:red [:black aa y c] z d]]
                  [:black a x [:red b y [:red c z d]]])] a
          :else 1))))
