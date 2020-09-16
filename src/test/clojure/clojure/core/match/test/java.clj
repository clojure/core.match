;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core.match.test.java
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.match]
        [clojure.core.match.java]
        [clojure.test]))

(bean-match java.util.Date)

(deftest bean-match-date
  (is (= 10 (matchm [(java.util.Date. 2009 10 1 12 30)]
              [{:year 2009 :month a}] a
              [{:year (:or 2010 2011) :month b}] b
              :else :wrong))))

(bean-match java.io.File)

(deftest bean-match-file
  (is (= (.getAbsolutePath (java.io.File. ".")) 
        (matchm [(java.io.File. ".")]
          [{:directory? true :absolute-path p}] p
          :else :wrong))))

