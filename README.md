match
====

An optimized pattern match and predicate dispatch library for Clojure. Currently the library only implements pattern matching.

This library implements a pattern match compilation algorithm that uses the notion of "necessity" from lazy pattern matching.

For example the following pattern:

```clojure
(let [x true
      y true
      z true]
  (match [x y z]
     [_ false true] 1
     [false true _ ] 2
     [_ _ false] 3
     [_ _ true] 4))
```

expands into

```clojure
(cond
 (= y false) (cond
              (= z false) (let [] 3)
              (= z true) (let [] 1)
              :else (throw (java.lang.Exception. "Found FailNode")))
 (= y true) (cond
             (= x false) (let [] 2)
             :else (cond
                    (= z false) 3
                    (= z true) 4
                    :else (throw
                           (java.lang.Exception.
                            "Found FailNode"))))
 :else (cond
        (= z false) (let [] 3)
        (= z true) (let [] 4)
        :else (throw (java.lang.Exception. "Found FailNode"))))
```

Note that y gets tested first. Lazy pattern matching consistently gives compact decision trees. This means faster pattern matching. You can find out more in the top paper cited below.

Matching literals
----

```clojure
(let [x true
      y true
      z true]
  (match [x y z]
     [_ false true] 1
     [false true _ ] 2
     [_ _ false] 3
     [_ _ true] 4))
;; => 4
```

Wherever you would use a wildcard you can use a binding:

```clojure
(let [x 1 y 2 z 4]
  (match [x y z]
     [1 2 b] [:a0 b]
     [a 2 4] [:a1 a]))
;; => [:a0 4]
```

Seq matching
----

```clojure
(let [x [1 2 nil nil nil]]
   (match [x]
     [[1]   :a0
     [[1 2]] :a1
     [[1 2 nil nil nil]] :a2))
;; => :a2
```

Seq patterns also support the familiar rest syntax from destructuring.

```clojure
(let [x '(1 2 3 4)]
  (match [x]
    [[1]] :a0
    [[_ 2 & [a & b]]] [:a1 a b]))
;; => [:a1 3 '(4)]
```

Map matching
----

```clojure
(let [x {:a 1 :b 1}]
   (match [x]
     [{_ :a 2 :b}] :a0
     [{1 :a _ :c}] :a1
     [{3 :c _ :d 4 :e}] :a2))
;; => :a1
```

You can constrain map matching so that only maps with the exact key set will match:

```clojure
(match [x]
  [{_ :a 2 :b :only [:a :b]}] :a0
  [{1 :a c :c}] :a1
  [{3 :c d :d 4 :e}] :a2)
```

Or Patterns
----

Or patterns are supported anywhere you would use a pattern:

```clojure
(let [x '(1 2 3)]
  (match [x y z ]
    [[1 (3 | 4) 3]] :a0
    [[1 (2 | 3) 3]] :a1))
;; :a1
    
(let [x {:a 3}]
  (match [x y z ]
    [{(1 | 2) :a}] :a0
    [{(3 | 4) :a}] :a1))
;; => :a1
```

Guards
----

Guards are simple boolean tests. You can specify them like so:

```clojure
(let [y '(2 3 4 5)]
  (match [y]
    [[_ (a :when even?) _ _]] :a0
    [[_ (b :when [odd? div3?]) _ _]] :a1))
;; => :a1
```

Road Map
----

A good chunk of Maranget's algorithm for pattern matching has been implemented. We would like to flesh out the pattern matching functionality. Once that work is done, we'll move on to predicate dispatch.

If you would like to see a more detailed account of what we're considering you can look here (https://github.com/swannodette/match/wiki/Design-Wiki).

Resources
----

The four most relevant papers:

* [Compiling Pattern Matching to Good Decision Trees](http://pauillac.inria.fr/~maranget/papers/ml05e-maranget.pdf)
* [Efficient Predicate Dispatch](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.4553)
* [Warnings for Pattern Matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)
* [Type-Safe Modular Hash-Consing](http://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf)

Further reading:

* [Optimizing Pattern Matching](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.6.5507)
* [Pattern Guards And Transformational Patterns](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.8851)
* [Extensible Pattern Matching for Extensible Languages](http://www.ccs.neu.edu/home/samth/ifl2010-slides.pdf)
* [Art of the Metaobject Protocol](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=3925)
* [Custom Specializers in Object-Oriented Lisp](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.144.405&rep=rep1&type=pdf)

Copyright (C) 2011 David Nolen

Distributed under the Eclipse Public License, the same as Clojure.
