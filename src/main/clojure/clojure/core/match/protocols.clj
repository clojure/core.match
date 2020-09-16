;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.match.protocols)

;; =============================================================================
;; # Protocols

(defprotocol ISpecializeMatrix
  (specialize-matrix [this matrix]))

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

(defprotocol ISyntaxTag
  (syntax-tag [this]))

;; markers

(definterface IExistentialPattern)

(definterface IPseudoPattern)
