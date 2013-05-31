(ns clojure.core.match.protocols)

;; =============================================================================
;; # Protocols

(defprotocol ISpecializeMatrix
  (specialize-matrix [this rows ocrs]))

(defprotocol IContainsRestPattern
  (contains-rest-pattern? [this]))

(defprotocol IMatchLookup
  "Allows arbitrary objects to act like a map-like object when pattern
  matched. Avoid extending this directly for Java Beans, see
  `match.java/bean-match`."
  (val-at [this k not-found]))

;; TODO: consider converting to multimethods to avoid this nonsense - David

(defprotocol INodeCompile
  (n-to-clj [this]))

(defprotocol IPatternCompile
  (to-source* [this ocr]))

(defprotocol IVecMod
  (prepend [this x])
  (drop-nth [this n])
  (swap [this n]))

(defprotocol IVectorPattern
  (split [this n]))
