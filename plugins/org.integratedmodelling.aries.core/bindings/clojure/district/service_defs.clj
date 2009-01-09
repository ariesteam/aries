;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-DISTRICT.
;;;
;;; CLJ-DISTRICT is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; CLJ-DISTRICT is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-DISTRICT.  If not, see
;;; <http://www.gnu.org/licenses/>.

(ns district.service-defs
  (:refer-clojure))

(defmulti source-val (fn [benefit features] ()))
(defmulti sink-prob  (fn [benefit features] ()))
(defmulti usage-prob (fn [benefit features] ()))
(defmulti consumption-prob (fn [benefit features] ()))
(defmulti transition-prob  (fn [benefit src-features dest-features] ()))

;;;(source-val benefit features)
;    Expected units of service carrier provision. \textbf{(Source
;      value)}
;    \pause
;  \item
;;;(sink-prob  benefit features)
;    Likelihood of \textbf{destructively absorbing} a unit of the
;    service carrier. [0-1] \textbf{(Sink value)}
;    \pause
;  \item
;;;(usage-prob benefit features)
;    Likelihood of \textbf{non-destructively caching} a unit of the
;    service carrier. [0-1] \textbf{(Non-Destructive Use value)}
;    \pause
;;;(consumption-prob benefit features)
;    Likelihood of \textbf{destructively caching} a unit of the service
;    carrier. [0-1] \textbf{(Destructive Use value)}
;    \pause
;  \item
;
;;;(transition-prob benefit src-features dest-features)
;    \textbf{Sink value + Destructive Use value} represents the
;    likelihood that a unit of the service carrier will be absorbed by
;    the location as it flows through the network.
;    \pause
;  \item
;    Remaining outflow likelihood \textbf{(1 - Sink value - Destructive
;      Use value)} must be distributed among next step locations in the
;    network.
;    \pause
;  \item
;    Every half-edge must be assigned a likelihood value that a service
;    carrier unit may traverse it.
;    \pause
;  \item
;    This is first computed with a probabilistic function of the source
;    and destination feature distributions.
;    \pause
;  \item
;    Probabilities are normalized with respect to all other half-edges
;    which share a tail.
;    \pause
;  \item
;    Finally probabilities are weighted by the outflow probabilities at
;    their tails.
