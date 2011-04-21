(ns match.loader
  (:refer-clojure :exclude [reify == inc intern])
  (:use logos.minikanren
        logos.rel))

(def cl (ClassLoader/getSystemClassLoader))

(deftype A [])

;; defrel should be able define indexing
(defrel is a b)
(index-rel is b)

(comment
  (defrel is ^:index a ^:index b)
 )

;; man is this fun!
(defn load-class [c]
  (doseq [x (bases c)]
    (fact is c x)))

(comment
  (load-class String)
  (load-class clojure.lang.PersistentVector)
  (load-class clojure.lang.PersistentHashMap)
  (load-class clojure.lang.PersistentHashSet)
  (load-class clojure.lang.PersistentList)

  (run* [q]
     (is String q))

  ;; we would like to index the inverse here

  (run* [q]
        (is q clojure.lang.IObj))

  ;; holy crap this would be an awesome explorer
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run* [q]
              (is q clojure.lang.Counted))))))
  )