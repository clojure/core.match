(ns match.regex
  (:use [match.core :only [emit-pattern to-source pattern-equals
                           pattern-compare]]))

(defrecord RegexPattern [regex])

;; Regex extension
(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat))

(defmethod to-source RegexPattern
  [pat ocr]
  `(re-matches ~(:regex pat) ~ocr))

(defmethod pattern-equals [RegexPattern RegexPattern]
  [a b] (= (.pattern (:regex a)) (.pattern (:regex b))))

(defmethod pattern-compare [RegexPattern RegexPattern]
  [a b] (if (= (.pattern (:regex a)) (.pattern (:regex b)))
          0
          -1))
