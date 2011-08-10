(ns match.core.debug
  (:refer-clojure :exclude [compile])
  (:use [match.core :only [build-matrix compile to-clj source-pprint]])
  (:use [clojure.pprint :only [pprint]]))

(defmacro m-to-matrix [vars & clauses]
  `(-> (build-matrix ~vars ~@clauses)
     pprint))

(defmacro m-to-dag [vars & clauses]
  `(-> (build-matrix ~vars ~@clauses)
     compile
     pprint))

(defmacro m-to-clj [vars & clauses]
  `(-> (build-matrix ~vars ~@clauses)
     compile
     to-clj
     source-pprint))
