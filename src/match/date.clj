(ns match.date
  (:use [match.core :only [IMatchLookup]]))

(extend-type java.util.Date
  IMatchLookup
  (val-at* [this k not-found]
    (case k
      :year    (.getYear this)
      :month   (.getMonth this)
      :date    (.getDate this)
      :hours   (.getHours this)
      :minutes (.getMinutes this)
      not-found)))


;; Example implementation using clojure.core/bean
(comment
  (extend-type java.util.Date
    IMatchLookup
    (val-at* [this k not-found]
      (get (bean this) k not-found))))
