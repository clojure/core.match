(ns match.test.regex
  (:use [match regex [core :only [match]]] )
  (:use clojure.test))

(deftest basic-regex
         (is (= (match ["asdf"]
                       [#"asdf"] 1
                       :else 2)
                1)))
