(ns clojure.core.match.js
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :exclude [match]]))

(defmacro match 
  [vars & clauses]
  (binding [*clojurescript* true
            *line* (-> &form meta :line)
            *locals* (dissoc &env '_)
            *warned* (atom false)]
    `~(clj-form vars clauses)))
