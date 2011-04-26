(ns match.core
  (:refer-clojure :exclude [reify == inc])
  (:use [logos minikanren rel]))

(def method-table (atom {}))
(def pred-table (atom {}))
(defrel is a b)

(defn var->sym [v]
  (symbol (str (.ns v)) (name (.sym v))))

(defn defpred* [& xs]
  (let [[predsym predfn] (map (comp var->sym resolve) xs)]
   `(swap! pred-table assoc '~predsym '~predfn)))

(defmacro defpred [& xs]
  (apply defpred* xs))

(defn defm* [p & r]
  (if (= (first r) :guard)
    nil
    nil))

(defmacro defm [& xs]
  (apply defm* xs))

(comment
  (defpred even? even?)
  )