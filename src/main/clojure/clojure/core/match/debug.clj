(ns clojure.core.match.debug
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match.protocols]
        [clojure.core.match 
         :only [emit-matrix compile occurrences
                rows action-for-row clj-form]])
  (:require [clojure.pprint :as pp]))

(defn source-pprint [source]
  (binding [pp/*print-pprint-dispatch* pp/code-dispatch
            pp/*print-suppress-namespaces* true]
    (pp/pprint source)))

(defmacro build-matrix [vars & clauses]
  `(emit-matrix '~vars '~clauses))

(defmacro m-to-matrix [vars & clauses]
  `(-> (build-matrix ~vars ~@clauses)
     pprint-matrix))

(defmacro m-to-dag [vars & clauses]
  (binding [clojure.core.match/*line* (-> &form meta :line)
            clojure.core.match/*locals* &env
            clojure.core.match/*warned* (atom false)]
    `~(-> (emit-matrix vars clauses)
        compile
        pp/pprint)))

(defmacro m-to-clj [vars & clauses]
  (binding [clojure.core.match/*line* (-> &form meta :line)
            clojure.core.match/*locals* &env
            clojure.core.match/*warned* (atom false)]
    (try 
      (-> (clj-form vars clauses)
        source-pprint)
      (catch AssertionError e
        `(throw (AssertionError. ~(.getMessage e)))))))


(defn pprint-matrix
  ([pm] (pprint-matrix pm 4))
  ([pm col-width]
     (binding [*out* (pp/get-pretty-writer *out*)]
       (print "|")
       (doseq [o (occurrences pm)]
         (pp/cl-format true "~4D~7,vT" o col-width))
       (print "|")
       (prn)
       (doseq [[i row] (map-indexed (fn [p i] [p i]) (rows pm))]
         (print "|")
         (doseq [p (:ps row)]
           (pp/cl-format true "~4D~7,vT" (str p) col-width))
         (print "|")
         (print " " (action-for-row pm i))
         (prn))
       (println))))
