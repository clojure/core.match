(ns match.perf)

(defprotocol I
  (x [this])
  (y [this]))

(defprotocol IClass
  (id [this]))

(deftype A [x y] I (x [_] x) (y [_] y))
(deftype B [x y] I (x [_] x) (y [_] y))
(deftype C [x y] I (x [_] x) (y [_] y))
(deftype D [x y] I (x [_] x) (y [_] y))

(extend-type A IClass (id [_] 0))
(extend-type B IClass (id [_] 1))
(extend-type C IClass (id [_] 2))
(extend-type D IClass (id [_] 3))

(defn dag1 [f1 f2]
  (case (id f1)
        0 (case (id (x f1))
                0 (case (= (y f1) (y f2))
                        true :m1
                        false :m-not-understood)
                1 :m-not-understood
                2 :m-not-understood
                3 (case (= (y f1) (y f2))
                        true :m1
                        false :m-not-understood))
        1 (case (id (x f1)) 
                0 (case (= (y f1) (y f2))
                        true :m1
                        false :m-not-understood)
                1 :m2
                2 :m-not-understood
                3 (case (= (y f1) (y f2))
                        true :m1
                        false :m-not-understood))
        2 (case (id f2)
                0 (case (id (x f1))
                        0 :m4
                        1 :m2
                        2 :m4
                        3 :m4)
                1 (case (id (x f1))
                        0 :m4
                        1 :m2
                        2 :m4
                        3 :m4)
                2 :m3
                3 :m-ambiguous)))

(defn dag2 [f1 f2]
  (letfn [(sd1 []
            (case (= (y f1) (y f2))
                         true :m1
                         false :m-not-understood))
          (sd2 []
            (case (id (x f1))
                         0 :m4
                         1 :m2
                         2 :m4
                         3 :m4))]
   (case (id f1)
         0 (case (id (x f1))
                 0 (sd1)
                 1 :m-not-understood
                 2 :m-not-understood
                 3 (sd1))
         1 (case (id (x f1)) 
                 0 (sd1)
                 1 :m2
                 2 :m-not-understood
                 3 (sd1))
         2 (case (id f2)
                 0 (sd2)
                 1 (sd2)
                 2 :m3
                 3 :m-ambiguous))))

(defn dag3 [fa fb]
  (case (.getName ^Class (class fa))
            "match.perf.A" (case (.getName ^Class (class (x fa)))
                                     "match.perf.A" (case (= (y fa) (y fb))
                                                          true :ma
                                                          false :m-nu)
                                     "match.perf.B" :m-nu
                                     "match.perf.C" :m-nu
                                     "match.perf.D" (case (= (y fa) (y fb))
                                                          true :ma
                                                          false :m-nu))
            "match.perf.B" (case (.getName ^Class (class (x fa))) 
                                     "match.perf.A" (case (= (y fa) (y fb))
                                                          true :ma
                                                          false :m-nu)
                                     "match.perf.B" :mb
                                     "match.perf.C" :m-nu
                                     "match.perf.D" (case (= (y fa) (y fb))
                                                          true :ma
                                                          false :m-nu))
            "match.perf.C" (case (.getName ^Class (class fb))
                                     "match.perf.A" (case (.getName ^Class (class (x fa)))
                                                              "match.perf.A" :md
                                                              "match.perf.B" :mb
                                                              "match.perf.C" :md
                                                              "match.perf.D" :md)
                                     "match.perf.B" (case (.getName ^Class (class (x fa)))
                                                              "match.perf.A" :md
                                                              "match.perf.B" :mb
                                                              "match.perf.C" :md
                                                              "match.perf.D" :md)
                                     "match.perf.C" :mc
                                     "match.perf.D" :m-amb)))

(defn m1 []
  :m1)

(defn mu []
  :mu)

(deftype PredFn [f]
  clojure.lang.IFn
  (invoke [this a1] (f a1))
  (invoke [this a1 a2] (f a1 a2)))

(comment
  ;; 60ms
  (let [f (PredFn. nil)]
   (dotimes [_ 10]
     (time
      (dotimes [_ 1e8]
        (f 1 2)))))
  )

(defn dag4 [fa fb]
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

(defn dag5 [fa fb]
  (let [f (fn []
            (let [ya (y fa)
                  yb (y fb)]
              (case (= ya yb)
                    true (m1)
                    false nil)))]
    (case (instance? A fa)
          true (let [x (x fa)]
                 (case (instance? A x)
                       true (f)
                       false (case (instance? B x)
                                   true (case (instance? C x)
                                              true (case (instance? D x)
                                                         true nil
                                                         false nil)
                                              false nil)
                                   false nil)))
          false (case (instance? B fa)
                      true nil
                      false (case (instance? C fa)
                                  true nil
                                  false (case (instance? D fa)
                                              true (mu)
                                              false nil))))))

(deftype PredFn2 [f]
  clojure.lang.IFn
  (invoke [this a1] (@f a1))
  (invoke [this a1 a2] (@f a1 a2)))

(comment
  ;; ~90ms
  ;; 70ms
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)
        f (PredFn. dag4)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f o1 o2)))))

  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)
        f (PredFn2. (atom dag4))]
    (f o1 o2))

  ;; 100ms
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)
        f (PredFn2. (atom dag4))]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f o1 o2)))))

  ;; slower, perhaps useful for very large switches
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)
        f (PredFn. dag5)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (f o1 o2)))))
  )

(comment
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dag1 o1 o2))
  
  ;; 330ms
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (dag1 o1 o2)))))

  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (id o1)))))

  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dag2 o1 o2))
  
  ;; 360ms
  ;; sharing fns is fine if we need that
  ;; particularly if we support or
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (dag2 o1 o2)))))

  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dag3 o1 o2))

  ;; 175ms
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (dag3 o1 o2)))))

  (let [s1 (B. nil nil)
        o1 (D. (A. nil nil) s1)
        o2 (D. (A. nil nil) s1)]
    (dag4 o1 o2))

  ;; 60ms
  (let [s1 (B. nil nil)
        o1 (A. (A. nil nil) s1)
        o2 (A. (A. nil nil) s1)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (dag4 o1 o2)))))

  (defn bar []
    (if (= 1 1)
      (if (= 2 2)
        (if (= 3 3)
          true))))

  (defn foo []
    (bar))

  ;; 250ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e8]
       (foo))))

  ;; whether expanding to if or case statements is really an implementation detail
  ;; but even so, unless the 
  )

(comment
  (defn foo1 [x]
    (if (= x 10000000)
      x
      (recur (inc x))))

  (defn foo2 [x]
    (let [a (atom nil)
          b (reset! a x)]
     (if (= x 10000000)
       x
       (recur (inc x)))))

  ;; 117ms
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (foo1 0))))

  ;; 160ms
  ;; 600ms, not bad
  (dotimes [_ 10]
    (time
     (dotimes [_ 1]
       (foo2 0))))
  )