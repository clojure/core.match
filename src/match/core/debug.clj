(ns match.core.debug
  (:refer-clojure :exclude [compile])
  (:use [match.core :only [emit-matrix compile occurrences
                           rows patterns action-for-row n-to-clj
                           clj-form set-trace! no-trace!]])
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
  (binding [match.core/*line* (-> &form meta :line)
            match.core/*locals* &env
            match.core/*warned* (atom false)]
    `~(-> (emit-matrix vars clauses)
        compile
        pp/pprint)))

(defmacro m-to-clj [vars & clauses]
  (binding [match.core/*line* (-> &form meta :line)
            match.core/*locals* &env
            match.core/*warned* (atom false)]
    `~(source-pprint (clj-form vars clauses))))

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
         (doseq [p (patterns row)]
           (pp/cl-format true "~4D~7,vT" (str p) col-width))
         (print "|")
         (print " " (action-for-row pm i))
         (prn))
       (println))))
