(ns match.test.core
  (:refer-clojure :exclude [reify == inc compile])
  (:use [match.core])
  (:use [clojure.test]))

(deftest simple-compile
  (is
    (= (compile (pattern-matrix [] '[]))
       (fail-node)))
  (is
    (= (compile (pattern-matrix [] '[]))
       (fail-node))))

(def pm2 (pattern-matrix [(pattern-row [wildcard (pattern false) (pattern true)] :a1)
                          (pattern-row [(pattern false) (pattern true) wildcard] :a2)
                          (pattern-row [wildcard wildcard (pattern false)] :a3)
                          (pattern-row [wildcard wildcard (pattern true)] :a4)]
                         '[x y z]))

;(emit-matrix 
;  '[x y]
;  [[_ true] 1
;   [_ false] 2
;   [true _] 3
;   [true _] 4
;   ]
;  )
