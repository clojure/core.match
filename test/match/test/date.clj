(ns match.test.date
  (:use clojure.test)
  (:use [match [core :only [match]] date]))

(deftest date-test1
  (is (= (match [(java.util.Date. 2010 10 1 12 30)]
                [{:year 2009 :month a}] a
                [{:year (2010 | 2011) :month b}] b
                :else :wrong)
         10)))
