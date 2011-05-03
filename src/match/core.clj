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

;; -----------------------------------------------------------------------------
;; defm

(defn var->sym [v]
  (symbol (str (.ns v)) (name (.sym v))))

(defn get-implied-pred-fns [predsym]
  (map (comp var-get resolve)
       (run* [q]
         (implies predsym q))))

(defn guards-for [p gs]
  (reduce (fn [s g]
              (if (contains? (set g) p)
                (conj s g)
                s))
          [] gs))

(defn index-guards [ps gs]
  (reduce (fn [m p]
            (assoc m p (guards-for p gs)))
          {} ps))

;; TODO: these will come from the macro so will most like be namespaced
(defn type-spec? [[p]]
  (or (= p 'isa?)
      (= p 'number?)))

(defn literal? [p]
  (or (number? p)))

(defn is-pred? [[pa s] pb]
  (= pa pb))

(defn add-method [name ps gs body]
  (swap! method-table update-in [name (count ps)]
         (fnil (fn [xs]
                 (conj xs {:pvector ps
                           :guards (index-guards ps gs)}))
               [])))

(defn necessary? [pmatrix col-idx]
  (every? (fn [{:keys [pvector guards]}]
               (let [p (pvector col-idx)]
                 (or (literal? p)
                     (some (fn [g] (type-spec? g)) (guards p)))))
          pmatrix))

(defn necessary-index [pmatrix arity]
  (loop [col-idx 0]
    (cond
     (>= col-idx arity) 0
     (necessary? pmatrix col-idx) col-idx
     :else (recur (inc col-idx)))))

(defn specializer-for-pattern [p gs]
  (cond
   (literal? p) '=
   :else (first (first gs))))

(defn specializers [pmatrix col-idx]
  (reduce (fn [s {:keys [pvector guards] :as row}]
            (let [p (pvector col-idx)]
             (conj s (specializer-for-pattern p (guards p)))))
          #{} pmatrix))

;; destructuring would happen after we confirm the type, shazam!
(defn specialize [pmatrix col-idx spec]
  (reduce (fn [v row]
            (let [p ((:pvector row) col-idx)
                  gs (get-in row [:guards p])]
              (if (some (fn [g] (is-pred? g spec)) gs)
                (conj v
                      (-> row
                          (update-in [:pvector] (fn [v] (remove #(= % p) v)))
                          (assoc-in [:guards p] (remove (fn [g]
                                                          (is-pred? g spec))
                                                        gs))))
                v)))
          [] pmatrix))

(defn defpred* [& xs]
  (let [[predsym predfn] (map (comp var->sym resolve) xs)]
   `(swap! pred-table assoc '~predsym '~predfn)))

(defmacro defpred [& xs]
  (apply defpred* xs))

(comment
 (defn defm* [mname & [pvector & r']]
   (let [mdata (mname @method-table)]
     (cond
      (vector? pvector) (let [[guards body] (if (= (first r') :guard)
                                              [(second r') (drop 2 r')]
                                              [nil (rest r')])]
                          (handle-p mdata pvector guards body))
      :else nil)))

 (defmacro defm [& xs]
   (apply defm* xs)))

(comment
  (do
    (reset! method-table {})
    (add-method 'foo '[0 b] nil nil)
    (add-method 'foo '[x _] '[(number? x)] nil)
    (add-method 'foo '[a b] '[(isa? a A)] nil)
    (add-method 'foo '[a b] '[(isa? a C) (isa? b D)] nil))

  (necessary? (get-in @method-table '[foo 2]) 0) ; true
  (necessary? (get-in @method-table '[foo 2]) 1) ; false

  ;; TODO: filter the one's that don't match spec?
  (specialize (get-in @method-table '[foo 2]) 0 'isa?)

  (count (get-in @method-table '[foo 2])) ; 4
  (count (specialize (get-in @method-table '[foo 2]) 0 'isa?)) ; 2

  (specializers (get-in @method-table '[foo 2]) 0) ; #{isa? = number?}

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

  ;; more than one test, means we'll check things after the first dispatch?
  ;; remaining guards for an argument
  ;;
  )