;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; WARNING: this namespace is experimental

(ns ^{:skip-wiki true}
  clojure.core.match.binary
  (:refer-clojure :exclude [compile])
  (:use [clojure.core.match :as m]))

(derive ::m/binary ::m/vector)

(defmethod check-size? ::m/binary
  [_] false)

(defmethod test-inline ::m/binary
  [_ ocr] `(instance? Long ~ocr))

(defmethod nth-inline ::m/binary
  [_ ocr i] `(bit-shift-right (bit-and ~ocr (bit-shift-left 1 ~i)) ~i))

(comment
  (let [x 5]
    (match [x]
      [([_ _ 1 1] ::m/binary)] :a0
      [([1 0 1 _] ::m/binary)] :a1
      :else :a2))
  
  (let [x 5]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (match [x]
           [([_ _ 1 1] ::m/binary)] :a0
           [([1 0 1 _] ::m/binary)] :a1
           :else :a2)))))
  )

(comment
  (match [dgram]
    [([(ip-version 4)
       ((hlen 4) :when [#(>= % 5) #(<= (* 4 %) drgramsize)])
       (srvc-type 8)
       (totlen 16)
       (id 16)
       (flgs 3)
       (fragoff 13)
       (ttl 8)
       (proto 8)
       (hdrchksum 16)
       (srcip 32)
       (destip 32)
       & restdgram] ::m/binary)])
  )

;; Erlang
;;
;; -define (IP_VERSION, 4).
;; -define (IP_MIN_HDR_LEN, 5).
;; DgramSize = byte_size (Dgram),
;; case Dgram of
;; <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
;; ID:16, Flgs:3, FragOff:13,
;; TTL:8, Proto:8, HdrChkSum:16,
;; SrcIP:32,
;; DestIP:32, RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
;; OptsLen = 4* (HLen - ?IP_MIN_HDR_LEN),
;; <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
;; ...
;; end.
