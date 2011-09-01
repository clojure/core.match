(ns match.regex
  (:use [match.core :only [emit-pattern to-source pattern-equals
                           pattern-compare]]))

(defrecord RegexPattern [regex])

(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat))

(defmethod to-source RegexPattern
  [pat ocr]
  `(re-matches ~(:regex pat) ~ocr))

(defmethod pattern-compare [RegexPattern RegexPattern]
  [a b] (if (and (= (.pattern (:regex a)) (.pattern (:regex b)))
                 (= (.flags (:regex a)) (.flags (:regex b))))
          0
          -1))
