match
====

An optimizing pattern match and predicate dispatch library for Clojure.

Note there's nothing to see here yet. There's a lot of work and research to do before anything useful comes of this. If you've signed a Clojure CA I'm more than willing to accept help.

The basic idea is to maintain a DAG and compile it into a series of nested case statements. The following illustrates the basic idea and compares it against multimethod performance in the latest Clojure 1.3.0 alphas.

More [crazy ideas here](https://github.com/swannodette/match/wiki/Crazy-Ideas).

```clj
(defn m1 []
  :m1)

(defn mu []
  :mu)

(deftype PredFn [f]
  clojure.lang.IFn
  (invoke [this a1] (@f a1))
  (invoke [this a1 a2] (@f a1 a2)))

(defn dag [fa fb]
  (let [f (fn []
            (let [ya (y fa)
                  yb (y fb)]
              (if (= ya yb)
                (m1))))]
   (if (instance? A fa)
     (let [x (x fa)]
       (if (instance? A x)
         (f)
         (if (instance? B x)
           (if (instance? C x)
             (if (instance? D x)
               nil)))))
     (if (instance? B fa)
       nil
       (if (instance? C fa)
         nil
         (if (instance? D fa)
           (mu)))))))

;; ~100ms
(let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)
        f (PredFn. (atom dag4))]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f o1 o2)))))

(defmulti gf (fn [f1 f2]
               [(class f1)
                (class f2)
                (class (x f1))
                (= (y f1) (y f2))]))

(defmethod gf [A Object A true] [f1 f2] :m1)

;; ~1900ms-2000ms
(let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (gf o1 o2)))))
```

Resources
----

The three most relevant papers:

* [Efficient Predicate Dispatch](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.4553)
* [Compiling Pattern Matching to Good Decision Trees](http://pauillac.inria.fr/~maranget/papers/ml05e-maranget.p
df)
* [Warning for Pattern Matching](http://moscova.inria.fr/~maranget/papers/warn/index.html)

Further reading:

* [Optimizing Pattern Matching](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.6.5507)
* [Pattern Guards And Transformational Patterns](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.35.8851)
* [Extensible Pattern Matching for Extensible Languages](http://www.ccs.neu.edu/home/samth/ifl2010-slides.pdf)
* [Art of the Metaobject Protocol](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=3925)
* [Custom Specializers in Object-Oriented Lisp](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.144.405&rep=rep1&type=pdf)

Copyright (C) 2011 David Nolen

Distributed under the Eclipse Public License, the same as Clojure.
