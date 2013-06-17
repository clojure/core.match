(ns clojure.core.match.js
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :exclude [match]]))

(defmacro match 
  [vars & clauses]
  (let [[vars clauses]
        (if (vector? vars)
          [vars clauses]
          [(vector vars)
            (mapcat (fn [[c a]]
                      [(if (not= c :else) (vector c) c) a])
              (partition 2 clauses))])]
   (binding [*clojurescript* true
             *line* (-> &form meta :line)
             *locals* (dissoc &env '_)
             *warned* (atom false)]
     `~(clj-form vars clauses))))
