(ns clojure.core.match.pred
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.match :as match]))

(defprotocol IPredicate 
  (test-fn [this]))

(defprotocol IDispMatrix
  (sort-dispatches [this])
  (add-dispatch [this d f])
  (run-dispatch [this input])
  (patterns [this]))

(declare sort-by-preference)

(defn sort-dispatches* [dispatch-matrix]
  dispatch-matrix) ;; TODO plug back in logic sort

(def default-dispatch
  {:dispatch :default
   :action (fn [] (throw (Exception. "Default dispatch")))})

(defn matches-pattern? [pat args]
  (->> (map #((test-fn %1) %2) pat args)
    (every? identity)))

(defn run-dispatch* [dispatch-matrix args]
  "dispatch-matrix is a vector of maps with keys :dispatch :action"
  (let [ps (conj dispatch-matrix default-dispatch)]
    (println ps)
    (some ;; TODO handle case where action returns falsy value
      #(when (or (println "testing" %1)
                 (= (:dispatch %1) :default)
                 (matches-pattern? (:dispatch %1) args))
         ((:action %1)))  
      ps)))


(deftype DispMatrix [dispatches]
  IDispMatrix
  (add-dispatch [this d f]
    (swap! dispatches conj {:dispatch d :action f})
    (sort-dispatches this))

  (sort-dispatches [this]
    (swap! dispatches sort-dispatches*))
  
  (run-dispatch [this args]
    (run-dispatch* @dispatches args)))

(defn dispatch-matrix []
  (DispMatrix. (atom [])))

;; Logic

(defrel subsumeso super sub)


;; Interface

(defmacro defdisp [name]
  `(def ~name
     (dispatch-matrix)))

(defmacro defm [name pattern & body]
  `(add-dispatch ~name ~pattern (fn ~@body)))

(defmacro defp [name test]
  `(def ~name
     (reify IPredicate
       (test-fn [this] ~test))))

(defn subsumes 
  "Declares super as a strict superset of sub"
  [super sub]
  (fact subsumeso super sub))

;;Example

(defp any-integer integer?)
(defp pos-integer (comp pos? integer?))

(subsumes any-integer pos-integer)

(defdisp test-disp)
(defm test-disp 
      [any-integer]
      [a]
      {:second a})

(defm test-disp 
      [pos-integer]
      [a]
      {:first a})

;(run-dispatch test-disp [2])
