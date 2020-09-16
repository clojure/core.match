;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.core.match
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :exclude [match matchv match-let matchm]]))

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
             *locals* (dissoc (:locals &env) '_)
             *warned* (atom false)]
     `~(clj-form vars clauses))))

(defmacro match*
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
             *locals* (dissoc (:locals &env) '_)
             *warned* (atom false)
             *no-backtrack* true]
     `~(clj-form vars clauses))))

(defmacro matchv [type vars & clauses]
  (binding [*clojurescript* true
            *vector-type* type
            *line* (-> &form meta :line)
            *locals* (dissoc (:locals &env) '_)
            *warned* (atom false)]
    `~(clj-form vars clauses)))


(defmacro matchv* [type vars & clauses]
  (binding [*clojurescript* true
            *vector-type* type
            *line* (-> &form meta :line)
            *locals* (dissoc (:locals &env) '_)
            *warned* (atom false)
            *no-backtrack* true]
    `~(clj-form vars clauses)))

(defmacro matchm
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
             *locals* (dissoc (:locals &env) '_)
             *warned* (atom false)]
     `~(clj-form vars clauses))))

(defmacro match-let [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (match [~@bindvars#]
         ~@body))))

(defmacro match-let* [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (match* [~@bindvars#]
         ~@body))))

