(ns match.loader
  (:refer-clojure :exclude [reify == inc intern])
  (:use [clojure.core.logic minikanren prelude tabled]))

(def cl (ClassLoader/getSystemClassLoader))

(deftype A [])

;; defrel should be able define indexing
(defrel is* a b)

(comment
  (defrel is ^:index a ^:index b)
 )

(defn load-class [c]
  (let [bs (bases c)]
    (doseq [x bs]
      (fact is* c x)
      (load-class x))))

;; erg, we need to fix match
(def is
  (tabled [a b]
    (conde
      ((is* a b))
      ((exist [c]
        (is* a c)
        (is c b))))))

(comment
  ;; FIXME
  (defne is [a b]
    ([_ _] (is* a b))
    ([_ _] (exist [c]
             (is* a c)
             (is c b))))
  )

(comment
  (load-class String)
  (load-class clojure.lang.IFn)
  (load-class clojure.lang.PersistentVector)
  (load-class clojure.lang.PersistentHashMap)
  (load-class clojure.lang.PersistentHashSet)
  (load-class clojure.lang.PersistentList)

  (run* [q]
     (is String q))

  ;; we would like to index the inverse here

  (run* [q]
        (is* q clojure.lang.IObj))

  ;; holy crap this would be an awesome explorer
  ;; memoing makes a massive difference here
  ;; 500ms versus 22000ms for 1e4
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (doall
        (run* [q]
              (is q clojure.lang.Counted))))))

  ;; now this is cool
  (run* [q]
     (is q clojure.lang.Sequential)
     (is q clojure.lang.Counted))

  (run* [q]
     (is q clojure.lang.IFn))
  )