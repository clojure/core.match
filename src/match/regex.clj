(ns match.regex
  (:refer-clojure :exclude [compile])
  (:use match.core))

(defrecord RegexPattern [regex]
  IPatternCompile
  (p-to-clj [this ocr]
    `(re-matches ~regex ~ocr)))

(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat))

(defmethod pattern-equals [RegexPattern RegexPattern]
  [^RegexPattern a ^RegexPattern b] (= (:regex a) (:regex b)))

(defmethod pattern-compare [RegexPattern RegexPattern]
  [a b] -1)

(comment
  (let [s "hello world"]
   (match [s]
     [#"hello.+"] :a0
     [#"goodbye.+"] :a1
     :else :a2))
  )