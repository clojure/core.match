(ns clojure.core.match.js
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :exclude [match matchv match-let]]))

(defmacro asets [a vs]
  `(do
     ~@(map (fn [a b c] (concat a (list b c)))
         (repeat `(aset ~a)) (range (count vs)) vs)
     ~a))

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
             *warned* (atom false)
             *recur-present* true]
     `~(clj-form vars clauses))))

(defmacro matchv [type vars & clauses]
  (binding [*clojurescript* true
            *vector-type* type
            *line* (-> &form meta :line)
            *locals* (dissoc &env '_)
            *warned* (atom false)
            *recur-present* true]
    `~(clj-form vars clauses)))

(defmacro match-let [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (match [~@bindvars#]
         ~@body))))

