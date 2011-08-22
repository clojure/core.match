(ns match.regex
  (:use [match.core :only [IPatternCompile match-1 to-source emit-pattern
                           pattern-equals pattern-compare]]))

(defrecord RegexPattern [regex]
  IPatternCompile
  (to-source [this ocr]
    `(re-matches ~regex ~ocr)))

(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat))

(defmethod pattern-equals [RegexPattern RegexPattern]
  [a b] (= (:regex a) (:regex b)))

(comment
  (let [s "hello world"]
    (match-1 s
      #"hello.+" :a0
      #"goodbye.+" :a1
      :else :a2))
  )