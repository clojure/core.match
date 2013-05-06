(ns clojure.core.match.js.tests
  (:use-macros [clojure.core.match.js :only [match]])
  (:require [clojure.core.match]))

(defn js-print [& args]
  (if (js* "typeof console != 'undefined'")
    (.log js/console (apply str args))
    (js/print (apply str args))))

(set! *print-fn* js-print)

(defn pattern-match-1 []
  (let [x true
        y true
        z true]
    (match [x y z]
      [_ false true] 1
      [false true _ ] 2
      [_ _ false] 3
      [_ _ true] 4
      :else 5)))

(assert (= (pattern-match-1) 4))

(println "Tests completed without exception.")
