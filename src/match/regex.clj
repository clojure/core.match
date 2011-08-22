(ns match.regex
  (:refer-clojure :exclude [compile])
  (:use match.core))

(deftype RegexPattern [regex _meta]
  clojure.lang.IObj
  (meta [_] _meta)
  (withMeta [_ new-meta]
    (RegexPattern. regex new-meta))
  IPatternCompile
  (p-to-clj [this ocr]
    `(re-matches ~regex ~ocr))
  Object
  (toString [_]
    (str regex)))

(defmethod emit-pattern java.util.regex.Pattern
  [pat]
  (RegexPattern. pat nil))

(defmethod pattern-equals [RegexPattern RegexPattern]
  [^RegexPattern a ^RegexPattern b] (= (.regex a) (.regex b)))

(defmethod pattern-compare [RegexPattern RegexPattern]
  [a b] -1)

(comment
  (let [s "hello world"]
   (match [s]
     [#"hello.+"] :a0
     [#"goodbye.+"] :a1
     :else :a2))
  )