(ns clojure.core.match.js.tests
  (:use-macros [clojure.core.match.js :only [match]])
  (:require-macros [clojure.core.match.array])
  (:require [clojure.core.match]))

(defn js-print [& args]
  (if (js* "typeof console != 'undefined'")
    (.log js/console (apply str args))
    (js/print (apply str args))))

(set! *print-fn* js-print)

;; =============================================================================
;; Basic matching

(assert
  (= (let [x true
          y true
          z true]
       (match [x y z]
         [_ false true] 1
         [false true _ ] 2
         [_ _ false] 3
         [_ _ true] 4
         :else 5))
    4))

(assert
  (= ((fn [x y z done]
        (if (not done)
          (match [x y z]
            [_ false true] (recur x y z 1)
            [false true _ ] (recur x y z 2)
            [_ _ false] (recur x y z 3)
            [_ _ true] (recur x y z 4)
            :else 5)
          done)) true true true false)
    4))

(assert
  (= (let [x 1 y 2 z 4]
       (match [x y z]
         [1 2 b] [:a0 b]
         [a 2 4] [:a1 a]
         :else []))
    [:a0 4]))

;; =============================================================================
;; Seq matching

(assert
  (= (let [x [1]]
       (match [x]
         [1] 1
         [([1] :seq)] 2
         :else []))
    2))

(assert
  (= (let [x [1 2 nil nil nil]]
       (match [x]
         [([1] :seq)]     :a0
         [([1 2] :seq)]   :a1
         [([1 2 nil nil nil] :seq)] :a2
         :else []))
    :a2))

(assert
  (= (let [x '(1 2 4)
            y nil
            z nil]
       (match [x y z]
         [([1 2 b] :seq) _ _] [:a0 b]
         [([a 2 4] :seq) _ _] [:a1 a]
         :else []))
    [:a0 4]))

(assert
  (= (let [x '(1 2 3)]
       (match [x]
         [([1 z 4] :seq)] z
         [([_ _ _] :seq)] :a2
         :else [])
       :a2)))

;; =============================================================================
;; Map matching

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b 1}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else nil))
    :a1))


(assert
  (= (let [x {:a 1 :b 2}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b 1}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else nil))
    :a0))

(assert
  (= (let [x {:c 3 :d 9 :e 4}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b 1}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else nil))
    :a2))

(assert
  (= (let [x {:c 3 :e 4}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b 1}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else nil))
    nil))

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{:a _ :b 1}] :a0
         [{:a 1 :b _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a0))

(assert
  (= (let [x {:a 1 :b 1 :c 1}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a 1 :b _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a1))

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{:a _ :b 2}] :a0
         [{:a _ :b _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a1))

(assert
  (= (let [x {:a 1}]
       (match [x]
         [{:a 1 :b 1}] :a0
         [{:a _ :b _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    []))

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{:b 1}] :a0
         [{:a _ :b _}] :a1
         [{:a _ :b _}] :a2
         :else []))
    :a0))

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{}] :a0
         [{:a _ :b _}] :a1
         [{:a 1 :b 1}] :a2
         :else []))
    :a0))

(assert
  (= (let [x {:a 1 :b 1}]
       (match [x]
         [{:x nil :y nil}] :a0
         [{:a _ :b _}] :a1
         [{:a 1 :b 1}] :a2
         :else []))
    :a1))

(assert
  (= (let [x {:a 1 :b 2}]
       (match [x]
         [({:a _ :b 2} :only [:a :b])] :a0
         [{:a 1 :c _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a0))

(assert
  (= (let [x {:a 1 :b 2 :c 3}]
       (match [x]
         [({:a _ :b 2} :only [:a :b])] :a0
         [{:a 1 :c _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a1))

(assert
  (= (let [x {:a 1 :b 2}]
       (match [x]
         [{:a a :b b}] [:a0 a b]
         :else []))
    [:a0 1 2]))

;; =============================================================================
;; Seq pattern edge cases

(assert
  (= (let [x '()]
       (match [x]
         [([] :seq)] :a0
         [([1 & r] :seq)] [:a1 r]
         :else []))
    :a0))

(assert
  (= (let [x '(1 2)]
       (match [x]
         [([1] :seq)] :a0
         [([1 & r] :seq)] [:a1 r]
         :else []))
    [:a1 '(2)]))

(assert
  (= (let [x '(1 2 3 4)]
       (match [x]
         [([1] :seq)] :a0
         [([_ 2 & ([a & b] :seq)] :seq)] [:a1 a b]
         :else []))
    [:a1 3 '(4)]))

;; =============================================================================
;; Or patterns

(assert
  (= (let [x 4 y 6 z 9]
       (match [x y z]
         [(:or 1 2 3) _ _] :a0
         [4 (:or 5 6 7) _] :a1
         :else []))
    :a1))

(assert
  (= (let [x '(1 2 3)
            y nil
            z nil]
       (match [x y z]
         [([1 (:or 3 4) 3] :seq) _ _] :a0
         [([1 (:or 2 3) 3] :seq) _ _] :a1
         :else []))
    :a1))

(assert
  (= (let [x {:a 3}
            y nil
            z nil]
       (match [x y z]
         [{:a (:or 1 2)} _ _] :a0
         [{:a (:or 3 4)} _ _] :a1
         :else []))
    :a1))

;; =============================================================================
;; Guard patterns

;; =============================================================================
;; Edge cases

(assert
  (= (let [a 1 b 1]
       (match [1 2]
         [a 3] :a1
         [1 2] :a2
         [2 _] :a5
         [_ 3] :a4
         :else :a3))
    :a2))

(assert
  (= (let [x :as y :when z 1]
       (match [x y z]
         [a ':when 1] :success
         [:as _ 2] :fail
         :else :fail))
    :success))

(assert
  (=  (let [e '(+ 1 (+ 2 3))
             op (first e)
             op? #(= % op)]
        (match [e]
          [([p :guard op? x ([p2 :guard op? y z] :seq)] :seq)] (list p x y z)))
    '(+ 1 2 3)))

(assert
  (= (let [e '(+ 1 (+ 2 3))]
       (match [e]
         [(['+ x (['+ y z] :seq)] :seq)] (list '+ x y z)))
    '(+ 1 2 3)))

(assert
  (= (let [e 'quote
            f 10]
       (match [e f]
         ['quote quote] quote))
    10))

(assert
  (=  (let [e '(:a (quote 10))]
        (match [e]
          [([quote (['quote 10] :seq)] :seq)] quote))
    :a))

;; =============================================================================
;; As pattern

(assert
  (= (let [v [[1 2]]]
       (match [v]
         [([3 1] :seq)] :a0
         [([(([1 a] :seq) :as b)] :seq)] [:a1 a b]
         :else []))
    [:a1 2 [1 2]]))

;; =============================================================================
;; Else cases

(assert
  (= (let [v [1]]
       (match [v]
         [2] 1
         :else 21))
    21))

(assert
  (= (let [v [[1 2]]]
       (match [v]
         [([1 3] :seq)] 1
         :else 21))
    21))

(assert
  (= (let [v {:a 1}]
       (match [v]
         [{:a a}] 1
         :else 21))
    1))

(assert
  (= (let [v 3]
       (match [v]
         [(:or 1  2)] :a0
         :else :a1))
    :a1))

(assert
  (= (->> (range 1 16)
       (map (fn [x]
              (match [(mod x 3) (mod x 5)]
                [0 0] "FizzBuzz"
                [0 _] "Fizz"
                [_ 0] "Buzz"
                :else (str x)))))
    '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz" "11" "Fizz" "13" "14" "FizzBuzz")))

;; =============================================================================
;; Single expressions

(assert
  (= (let [x 3]
       (match x
         1 :a0
         2 :a1
         :else :a2))
    :a2))

(assert
  (= (let [x 3]
       (match (mod x 2)
         1 :a0
         2 :a1
         :else :a2))
    :a0))

;; =============================================================================
;; Locals matching

(assert
  (= (let [x 2
            y 2]
       (match [x]
         [0] :a0
         [1] :a1
         [y] :a2
         :else :a3))
    :a2))

(assert
  (= (let [x 2]
       (match [x]
         [0] :a0
         [1] :a1
         [2] :a2
         :else :a3))
    :a2))

(assert
  (= (let [a 1]
       (match [1 2]
         [1 3] :a0
         [a 2] :a1
         :else :a2))
    :a1))

;; =============================================================================
;; More edgecases

(assert
  (= (match [true false]
       [true false] 1
       [false true] 2
       :else (throw (js/Error. "Shouldn't be here")))
    1))

(assert
  (= (let [x [1 2]]
       (match [x] 
         [(:or [1 2] [3 4] [5 6] [7 8] [9 10])] :a0
         :else (throw (js/Error. "Shouldn't be here"))))
    :a0))

(assert
  (= (let [_ 1
           x 2
           y 3]
       (match [x y]
         [1 1] :a0
         [_ 2] :a1
         [2 3] :a2
         :else :a3))
    :a2))

;; =============================================================================
;; Vector patterns

(assert
  (= (let [x [1 2 3]]
       (match [x]
         [([_ _ 2] ::clojure.core.match/vector)] :a0
         [([1 1 3] ::clojure.core.match/vector)] :a1
         [([1 2 3] ::clojure.core.match/vector)] :a2
         :else :a3))
    :a2))

(assert
  (= (let [n [:black [:red [:red 1 2 3] 3 4] 5 6]]
       (match [n]
         [(:or [:black [:red [:red a x b] y c] z d]
            [:black [:red a x [:red b y c]] z d]
            [:black a x [:red [:red b y c] z d]]
            [:black a x [:red b y [:red c z d]]])] :balance
         :else :valid))
    :balance))

(assert
  (= (let [v [1 2 3 4]]
       (match [v]
         [([1 1 3 & r] ::clojure.core.match/vector)] :a0
         [([1 2 4 & r] ::clojure.core.match/vector)] :a1
         [([1 2 3 & r] ::clojure.core.match/vector)] :a2
         :else :a3))
    :a2))

(assert
  (= (let [v [1 2 3 4]]
       (let [v [1 2 3 4]]
         (match [v]
           [([1 1 3 & r] ::clojure.core.match/vector)] :a0
           [([1 2 & r] ::clojure.core.match/vector)] :a1
           :else :a3)))
    :a1))

(assert
  (= (let [node 1]
       (match [node]
         [[1]] :a0
         [a] a
         :else :a1))
    1))

(assert
  (= (let [v []]
       (match [v]
         [[]] 1
         :else 2))
    1))

(assert
  (= (let [v [1 2]]
       (match [v]
         [[]] :a0
         [[x & r]] :a1
         :else :a2))
    :a1))

(assert
  (= (let [v [[1 2]]]
       (match [v]
         [[3 1]] :a0
         [[([1 a] :as b)]] [:a1 a b]
         :else :a2))
    [:a1 2 [1 2]]))

;; =============================================================================
;; Yet more edge cases

(assert
  (= (let [l '(1 2 3)]
       (match [l]
         [([a & [b & [c]]] :seq)] :a0
         :else :a1))
    :a0))

(assert
  (= (match [[:pow :x 2]]
       [[:pow arg pow]] 0
       [[:mult & args]] 1
       :else 2)
    0))

(assert
  (= (match [false]
       [false] true)
    true))

(assert
  (= (match [[:plus 1 2 3]]
       [[:pow arg pow]] 0
       [[:plus & args]] 1
       :else 2))
    1)

(assert
  (= (let [x {:a 1 :b 2 :c 10 :d 30}]
       (match [x]
         [({:a _ :b _ :c _ :d _} :only [:a :b :c :d])] :a-1
         [({:a _ :b 2} :only [:a :b])] :a0
         [{:a 1 :c _}] :a1
         [{:c 3 :d _ :e 4}] :a2
         :else []))
    :a-1))

(assert
  (and (= (let [m {:a 1}]
            (match [m]
              [({:a 1} :only [:a])] :a0
              :else :a1))
         :a0)
       (= (let [m {:a 1 :b 2}]
            (match [m]
              [({:a 1} :only [:a])] :a0
              :else :a1))
         :a1)))

(assert
  (= (let [m {:foo 1 "bar" 2}]
       (match [m]
         [{:foo 1 "bar" 2}] :a0
         :else :a1))
    :a0))

;; =============================================================================
;; Errors

(assert
  (= (try
       (match :a :a (throw (js/Error.)) :else :c)
       (catch js/Error e
         :d))
    :d))

;; =============================================================================
;; Match order

(assert
  (= (let [x '(1 2) y 1]
       (match [x y]
         [([1] :seq) _] :a0
         [_ 1] :a1
         [([1 2] :seq) _] :a2
         [_ 2] :a3
         :else :a4))
    :a1))

(assert
  (= (let [x '(1 2) y 1]
       (match [x y]
         [([1] :seq) _] :a0
         [([1 2] :seq) _] :a2
         [_ 1] :a1
         [_ 2] :a3
         :else :a4))
    :a2))

(assert
  (= (let [x '(1 2) y 3]
       (match [x y]
         [([1] :seq) _] :a0
         [_ 1] :a1
         [([1 2] :seq) _] :a2
         [_ 2] :a3
         :else :a4))
    :a2))

(assert
  (= (let [x '(1) y 3]
       (match [x y]
         [([1] :seq) _] :a0
         [_ 1] :a1
         [([1 2] :seq) _] :a2
         [_ 2] :a3
         :else :a4))
    :a0))

(assert
  (= (let [x '(1 2 3) y 2]
       (match [x y]
         [([1] :seq) _] :a0
         [_ 1] :a1
         [([1 2] :seq) _] :a2
         [_ 2] :a3
         :else :a4))
    :a3))

(assert
  (= (match [["foo"]]
       [["foo"]] :a0
       [["foo" a]] :a1
       [["baz"]] :a2
       [["baz" a b]] :a3
       :else :a4)
    :a0))

(assert
  (= (match [[2]]
       [[1]] :a0
       [1] :a1
       [[2]] :a2
       [2] :a3
       :else :a4)
    :a2))

(assert
  (= ((fn [x done]
        (if done
          done
          (match [x]
            [[1]] (recur x :a0)
            [1] (recur x :a1)
            [[2]] (recur x :a2)
            [2] (recur x :a3)
            :else :a4))) [2] false)
    :a2))

(assert
  (= ((fn [x done]
        (if done
          done
          (match [x]
            [[1]] (recur x :a0)
            [1] (recur x :a1)
            [[2]] (recur x :a2)
            [2] (recur x :a3)
            [3] (recur x :a4)
            [[3]] (recur x :a4)
            :else :a5))) [3] false)
    :a4))

(assert
  (= (match [[2]]
       [1] :a0
       [[1]] :a1
       [2] :a2
       [[2]] :a3
       :else :a4)
    :a3))

(assert
  (= (let [xs [:c]]
       (match xs
         [:a] :a0
         [:b b] :a1
         [:c] :a2
         :else :a3))
    :a2))

(assert
  (= (let [xs [1 2 3]]
       (match [xs]
         [([1 2 4] :seq)] :a0
         [[1 2 5]] :a1
         [([1 2 6] :seq)] :a2
         [[1 2 3]] :a3))
    :a3))

;; =============================================================================
;; Extending objects to pattern matching

(extend-type js/Date
  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (case k
      :day (.getDay this)
      :month (.getMonth this)
      :year (.getFullYear this)
      not-found)))

(assert
  (= (match [(js/Date. 2010 10 1 12 30)]
       [{:year 2009 :month a}] a
       [{:year (:or 2010 2011) :month b}] b
       :else :wrong)
    10))

;; =============================================================================
;; Arrays

(assert
  (= (let [x (int-array [1 2 3])]
       (match [^ints x]
         [[_ _ 2]] :a0
         [[1 1 3]] :a1
         [[1 2 3]] :a2
         :else :a3))
    :a2))

(assert
  (= (let [x (object-array [:foo :bar :baz])]
       (match [^objects x]
         [[_ _ :bar]] :a0
         [[:foo :foo :bar]] :a1
         [[:foo :bar :baz]] :a2
         :else :a3))
    :a2))

;; =============================================================================
;; Tickets

(assert (= (match 3 x x) 3))
(assert (= (match 'my-sym a a) 'my-sym))

(assert
  (= (let [xqq {:cz 1 :dz 2}]
       (match [xqq]
         [{:z a :zz b}] [:a0 a b]
         [{:cz a :dz b}] [:a2 a b]
         :else []))
    [:a2 1 2]))

(assert
  (= (let [xmm {:bz 2}]
       (match [xmm]
         [{:az a}] [:a0 a]
         [{:bz b}] [:a1 b]
         :else []))
    [:a1 2]))

(assert
  (= (match (vector)
       ([(re :guard string?)] :seq) 4
       [] 6)
    6))

(assert
  (= (match [ [1 2] ] [([& _] :seq)] true)
    true))

(assert
  (= (let [x []]
       (match [x]
         [[h & t]] [h t]
         :else :nomatch))
    :nomatch))

(assert
  (= (let [x [1]]
       (match [x]
         [[h & t]] [h t]
         :else :nomatch))
    [1 []]))

(assert
  (= (match [[:x]]
       [[m n & _]] 1
       :else nil)
    nil))

(assert
  (= (let [l '(1 2 3)]
       (match [l]
         [([a & [b & [c d]]] :seq)] :a0
         :else :a1))
    :a1))
 
(assert
  (= (let [x ()]
       (match [x]
         [([h & t] :seq)] [h t]
         [_] :a1))
    :a1))

(println "Tests completed without exception.")
