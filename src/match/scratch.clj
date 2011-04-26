(ns match.scratch
  (:use [clojure.pprint :only [pprint]]))

;; :mu - method not understood
;; :ma - method ambiguous

;; expressions ar the tests
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

(def methodtable (atom {}))

(defmacro defm [name spec args & body]
  (if ))

(comment
  (is x A)
  
  (isa x ::a) 
  (isa x ::b)

  (derive ::b ::a)

  (defn flip [f]
    (fn [a b]
      (f b a)))
  
  (condp (flip isa?) ::b
    ::a :cool)

  ;; 194ms for 1e6, yuk
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (condp (flip isa?) ::b
         ::a :cool))))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e8]
       (.getName ^Class (class "foo")))))

  ;; we can support hierarchies but they'll be the dog slow path

  ;; perhaps build on strings for now, change later

  ;; implicit and
  ;; no support for or for now

  (defm gf [(is A a) (is A (y a)) (not (is B (y a))) (= (y a) (y b))]
    [a b] :m1)

  (defm gf [(is B a) (is (y a) B)]
    [a b] :m2)

  (defm gf [(is C a) (is (y a) B) (is (y b) A)]
    [a b] :m2)
  
  (defm gf [(is C a)]
    [a b] :m4)

  ;; we rather just want to be able to do whatever we want
  ;; and supply preds any order, the macro should be able to
  ;; sort it out.
  ;; we don't want to mess w/ functions/ macros
  ;; :of is the only special operator, for working with real types

  (defm gf [a b]
    [(a :of A) ((y a) :of A)
     (not ((y b) :of B)) (= (y a) (= y b))]
    :m1)

  (defm gf [a b]
    [(a :of B) ((y a) :of B)]
    :m2)

  (defm gf [a b]
    [(a :of B) ((y a) :of B) ((y b) :of A)]
    :m2)
  
  (defm gf [a b]
    [(a :of C)]
    :m4)

  (defm gf [a b]
    [(b :of clojure.lang.PersistentVector)]
    :m4)

  ;; guards coming before arg list is weird since we want to guard
  ;; things that have appeared in bindings
  (defn gf1 [0 _] :m1)
  (defn gf1 [1 _] :m2)
  (defn gf1 [a {y :y :as b}]
    [(> 9 a) (float? a) (b :of B) (== y 5)]
    :m2)

  (methods gf1) ; #{ {:arglist [a {y :y :as b}] :guard {a ... }} }

  ;; this is going to be tricky

  (defm gf2 [a]
    (gf2 a (transient! [])))
  (defm gf2 [[] v] (persistent! v))
  (defm gf2 [[a & r] v] [(even? a) (integer? a)]
    (recur r (conj v {:even-int a})))
  (defm gf2 [[a & r] v] [(odd? a) (float? a)]
    (recur r (conj v {:odd-float a})))
  (defm gf2 [[a & r] v]
    (recur r v))

  (defm gf2
    ([a] (gf2 (transient! [])))
    ([[] v] (persistent! v))
    ([[a & r] v] :guard [(even? a) (integer? a)]
       (recur r (conj v {:even-int a})))
    ([[a & r] v] :guard [(odd? a) (float? a)]
       (recur r (conj v {:odd-float a})))
    ([[a & r] v]
       (recur r v)))

  ;; can we make order not matter here?
  ;; we can impose an order, is there something undecidable?
  (defm precip
    ([n] :guard [(integer? n)] (/ 1 n))
    ([0] 0))

  (defm precip2
    ([n] :guard [(integer? n)] (/ 1 n))
    ([0] 0)
    ([1] 1)
    ([2] 2)
    ([3] 3)) ;; that's a constant time jump, OK

  ;; if we have a literal, we check if it can subsumed by a guard?

  ;; what if there's a check for (A. 0), a record
  ;; and a test for (A? x)

  (?- complement even? odd? )
  (?- complement odd? even? )

  (defm record-match [a b]
    ([(A. 0) 1])
    ([(A. x) 3] [(even? x)])
    ([(A. 1) b])
    ([(A. x) b] [(odd? x)]))

  ;; 1. group expressions
  ;;
  ;; lots lots of thinking to do
  ;; in all cases we need to extract a's field
  ;;
  ;; is A
  ;; is x constant
  ;; is b constant
  ;; is x even or odd?

  ;; would be nice if we could predefine define contrapositives
  ;; *this* is what Rich Hickey is talking about
  ;; what we'll have is an *incremental logic engine*
  ;; the logic engine is ephemeral tho
  )

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