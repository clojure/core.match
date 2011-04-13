(ns match.core
  (:use [clojure.pprint :only [pprint]]))

;; :mu - method not understood
;; :ma - method ambiguous

(def e1 '(id (class f1)))
(def e2 '(id (class f2)))
(def e3 '(id (class (x f1))))
(def e4 '(= (y f1) (y f2)))

(def dag {0 {:value e1
             :edges '{A 1 B 2 C 3}}
          1 {:value e3
             :edges '{A 4 B 9 C 9 D 4}}
          2 {:value e3
             :edges '{A 4 B 5 C 9 D 4}}
          3 {:value e2
             :edges '{A 6 B 6 C 7 D 8}}
          4 {:value e4
             :edges {false 9 true 10}}
          5 :m2
          6 {:value e3
             :edges '{A 12 B 11 C 12 D 12}}
          7 :m3
          8 :ma
          9 :mu
          10 :m1
          11 :m2
          12 :m4})

(declare dag->case)

(defn emit-node [[c n] dag]
  `(~c ~(dag->case dag n)))

(defn dag->case [dag idx]
  (let [node (dag idx)]
    (if (not (map? node))
      node
      (let [{:keys [value edges]} node]
        `(case ~value
               ~@(mapcat #(emit-node % dag) edges))))))

(comment
  ;; man I love Clojure
  (pprint (dag->case dag 0))

  (case (id (class f1))
        A (case (id (class (x f1)))
                A (case (= (y f1) (y f2)) true :m1 false :mu)
                B :mu
                C :mu
                D (case (= (y f1) (y f2)) true :m1 false :mu))
        B (case (id (class (x f1)))
                A (case (= (y f1) (y f2)) true :m1 false :mu)
                B :m2
                C :mu
                D (case (= (y f1) (y f2)) true :m1 false :mu))
        C (case (id (class f2))
                A (case (id (class (x f1))) A :m4 B :m2 C :m4 D :m4)
                B (case (id (class (x f1))) A :m4 B :m2 C :m4 D :m4)
                C :m3
                D :ma))
  )