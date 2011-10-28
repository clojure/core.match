(ns clojure.core.match.test.java
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.match]
        [clojure.core.match.java]
        [clojure.test]))

(bean-match java.util.Date)

(deftest bean-match-date
  (is (= 10 (match [(java.util.Date. 2009 10 1 12 30)]
                    [{:year 2009 :month a}] a
                    [{:year (:or 2010 2011) :month b}] b
                    :else :wrong))))

(bean-match java.io.File)

(deftest bean-match-file
  (is (= (.getAbsolutePath (java.io.File. ".")) 
         (match [(java.io.File. ".")]
                [{:directory? true :absolute-path p}] p
                :else :wrong))))

