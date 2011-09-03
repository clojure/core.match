(ns match.regex
  (:use [match.core :only [emit-pattern to-source pattern-equals
                           pattern-compare]])
  (:import java.util.regex.Pattern))

(defrecord RegexPattern [regex])

(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat))

(defmethod to-source RegexPattern
  [pat ocr]
  `(re-matches ~(:regex pat) ~ocr))

(defmethod pattern-compare [RegexPattern RegexPattern]
  [a b] (if (and (= (.pattern ^Pattern (:regex a)) (.pattern ^Pattern (:regex b)))
                 (= (.flags ^Pattern (:regex a)) (.flags ^Pattern (:regex b))))
          0
          -1))
