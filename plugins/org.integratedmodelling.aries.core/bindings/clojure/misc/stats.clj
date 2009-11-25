;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-MISC.
;;;
;;; CLJ-MISC is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-MISC is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-MISC.  If not, see <http://www.gnu.org/licenses/>.

(ns misc.stats
  (:refer-clojure)
  (:use [misc.utils :only (seq2map maphash)]))

(defn mean
  [vals]
  (/ (reduce + vals) (count vals)))

(defn variance
  [vals]
  (let [mu (mean vals)]
    (mean (map #(Math/pow (- % mu) 2) vals))))

(defn stdev
  [vals]
  (Math/sqrt (variance vals)))

(defn normalize
  "Return x normalized in the range [0,1].
   x is assumed to be drawn from N(mu,sigma)."
  [x mu sigma]
  (/ (+ 1 (- (/ x sigma) mu)) 2))

(defn rv-from-scalar
  "Returns a new distribution with the same vals as X with probability 1.0 on x."
  [X x]
  (seq2map X (fn [[v p]] (vector v (if (= v x) 1.0 0.0)))))

(defn rv-zero?
  "Returns true if P(X=zero-val)=1.0 for the random variable X."
  [X zero-val]
  (== (X zero-val) 1.0))

;; FIXME finish stub
(defn rv-add
  "Returns the distribution of the sum of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn rv-sub
  "Returns the distribution of the difference of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn scalar-rv-sub
  "Returns the distribution of the random variable Y with its range values subtracted from x."
  [x Y]
  (maphash #(- x %) identity Y))

;; FIXME finish stub
(defn rv-mult
  "Returns the distribution of the product of two random variables X and Y."
  [X Y]
  X)

;; FIXME finish stub
(defn rv-div
  "Returns the distribution of the quotient of two random variables X and Y."
  [X Y]
  X)

(defn rv-scalar-mult
  "Returns the distribution of the random variable X with its range values multiplied by y."
  [X y]
  (maphash #(* % y) identity X))

(defn scalar-rv-mult
  "Returns the distribution of the random variable Y with its range values multiplied by x."
  [x Y]
  (maphash #(* x %) identity Y))

(defn rv-scalar-div
  "Returns the distribution of the random variable X with its range values divided by y."
  [X y]
  (maphash #(/ % y) identity X))

(defn scalar-rv-div
  "Returns the distribution of the random variable Y with its range values replaced by x/y."
  [x Y]
  (maphash #(/ x %) identity Y))

;; FIXME finish stub
(defn rv-gt
  ""
  [X Y]
  true)

;; FIXME finish stub
(defn rv-scalar-gt
  ""
  [X y]
  true)

;; FIXME finish stub
(defn scalar-rv-gt
  ""
  [x Y]
  true)

;; FIXME finish stub
(defn rv-min
  ""
  [X Y]
  X)

;; FIXME finish stub
(defn rv-eq
  ""
  [X Y]
  true)

;; FIXME finish stub
(defn rv-lt
  ""
  [X Y]
  true)
