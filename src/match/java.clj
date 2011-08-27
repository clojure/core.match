(ns match.java
  (:use [match.core :only [IMatchLookup val-at* match]]
        [clojure.string :only [lower-case]]))

(def ^:private method-name-pattern #"^(is|get)(.*)$")

(defn- dash-case 
  [^String s] 
  (let [gsub (fn [s re sub] (.replaceAll (re-matcher re s) sub))] 
    (-> s
      (gsub #"([A-Z]+)([A-Z][a-z])" "$1-$2")    
      (gsub #"([a-z]+)([A-Z])" "$1-$2")
      (lower-case)))) 

(defn- keywordize 
  [^String s]
  (let [[_ pre n] (re-find (re-matcher method-name-pattern s))]
    (-> n dash-case (str (if (= pre "is") "?")) keyword)))

(defmacro object-match
  "Generate an implementation of match.core/IMatchLookup for the given Java class.
  Keys are mapped like this:
  
    isVisible       -> :visible?
    getText         -> :text
    getAbsolutePath -> :absolute-path 
  "
  [klass] 
  (let [method-names (->> (.getMethods ^Class (resolve klass))
                       ; Methods that have is/get naming
                       (filter (fn [^java.lang.reflect.Method m] 
                                 (re-find method-name-pattern (.getName m)))) 
                       ; Methods with no args
                       (filter (fn [^java.lang.reflect.Method m] 
                                 (= 0 (count (.getParameterTypes m))))) 
                       ; Grab name as a symbol
                       (map    (fn [^java.lang.reflect.Method m] 
                                 (.getName m))))
        this (gensym "this")]
    `(extend-type ~klass
       IMatchLookup
       (~'val-at* [~this k# not-found#]
          (case k#
            ~@(mapcat 
                (fn [n] [(keywordize n) `(. ~this ~(symbol n))]) 
                method-names)
            not-found#)))))

