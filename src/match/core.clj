(ns match.core
  (:refer-clojure :exclude [reify == inc])
  (:use [logos minikanren tabled rel]))

;; -----------------------------------------------------------------------------
;; Tables

(def method-table (atom {})) ;; storing methods
(def pred-table (atom {}))   ;; tracking predicate symbols

;; -----------------------------------------------------------------------------
;; Logic

(defrel is a b)

(def implies
     (tabled [a b]
       (conde
         ((is a b))
         ((exist [z]
            (is a z)
            (implies z b))))))

(defrecord Boo [])

;; -----------------------------------------------------------------------------
;; defm

(defn var->sym [v]
  (symbol (str (.ns v)) (name (.sym v))))

(defn get-implied-pred-fns [predsym]
  (map (comp var-get resolve)
       (run* [q]
         (implies predsym q))))

(defn guards-for [a gs]
  (reduce (fn [s g]
              (if (contains? (set g) a)
                (conj s g)
                s))
          [] gs))

(defn index-guards [as gs]
  (reduce (fn [m a]
            (assoc m a (guards-for a gs)))
          {} as))

(defn type-spec? [[p s]]
  (== p `isa?))

(defn defpred* [& xs]
  (let [[predsym predfn] (map (comp var->sym resolve) xs)]
   `(swap! pred-table assoc '~predsym '~predfn)))

(defmacro defpred [& xs]
  (apply defpred* xs))

(defn add-method [name as gs body]
  (swap! method-table update-in [name]
         (fnil (fn [m]
                 (update-in m [(count as)]
                            (fnil
                             (fn [xs]
                                (conj xs {:arglist as
                                          :guards (index-guards as gs)}))
                             [])))
               {})))

(defn necessary? [pmatrix col-idx]
  (every? (fn [{:keys [arglist guards]}]
               (let [asym (arglist col-idx)]
                 (some (fn [g] (type-spec? g)) (guards asym))))
          pmatrix))

(defn necessary-index [pmatrix arity]
  (loop [i 0]
    (cond
     (>= i arity) 0
     (necessary? pmatrix i) i
     :else (recur (inc i)))))

(defn defm* [mname & [arglist & r']]
  (let [mdata (mname @method-table)]
   (cond
    (vector? arglist) (let [[guards body] (if (== (first r') :guard)
                                            [(second r') (drop 2 r')]
                                            [nil (rest r')])]
                        (handle-p mdata arglist guards body))
    :else nil)))

(defmacro defm [& xs]
  (apply defm* xs))

(comment
  (do
    (reset! method-table {})
    (add-method 'foo '[a b] '[(isa? a A)] nil)
    (add-method 'foo '[a b] '[(isa? a C)] nil))

  (necessary? (get-in @method-table '[foo 2]) 1)

  (defpred even? even?)

  (defrecord Boo [])

  ;; could fix this with a macro
  ;; are there composability expectations?
  (fact is `even? `integer?)
  (fact is `integer? `number?)

  (run* [q]
    (implies `even? q))

  (defm foo [x] :guard [(even? x)]
    :two)
  (defm foo [0] :one)
  )