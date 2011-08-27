(ns match.test.java
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core]
        [match.java]
        [clojure.test]))

(object-match java.util.Date)

(deftest object-match-date
  (is (= 10 (match [(java.util.Date. 2009 10 1 12 30)]
                    [{:year 2009 :month a}] a
                    [{:year (2010 | 2011) :month b}] b
                    :else :wrong))))

(object-match java.io.File)

(deftest object-match-file
  (is (= (.getAbsolutePath (java.io.File. ".")) 
         (match [(java.io.File. ".")]
                [{:directory? true :absolute-path p}] p
                :else :wrong))))

