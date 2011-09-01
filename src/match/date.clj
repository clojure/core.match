(ns match.date
  (:use [match.java :only [bean-match]]))

(bean-match java.util.Date)
