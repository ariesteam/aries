;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-misc.
;;;
;;; clj-misc is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-misc is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-misc.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines functions for creating, querying, and
;;; manipulating deterministic numbers, which are exactly that. It is
;;; meant as a drop-in replacement library for clj-misc.varprop or
;;; clj-misc.randvars in the clj-span simulation system.

(ns clj-misc.numbers)

(defn create-from-states
  "Constructs a Number from n states and n probs, which is simply the
   expected value of the passed in distribution."
  [states probs]
  (reduce + (map * states probs)))

(defn create-from-ranges
  "Constructs a Number from n bounds and n-1 probs corresponding
   to a piecewise continuous uniform distribution with
   discontinuities (i.e. jumps) at the bounds. prob i represents the
   probability of being between bound i and bound i+1."
  [bounds probs]
  (let [midpoints (map (fn [next prev] (/ (+ next prev) 2.0)) (rest bounds) bounds)]
    (reduce + (map * midpoints probs))))

(def #^{:doc "The number 0.0."} _0_ 0.0)

(def #^{:doc "Returns the sum of two or more Numbers."} _+_ +)
(def #^{:doc "Returns the difference of two or more Numbers."} _-_ -)
(def #^{:doc "Returns the product of two or more Numbers."} _*_ *)
(def #^{:doc "Returns the quotient of two or more Numbers."} _d_ /)
(def #^{:doc "Compares two or more Numbers and returns true if they are in ascending order."} _<_ <)
(def #^{:doc "Compares two or more Numbers and returns true if they are in descending order."} _>_ >)
(def #^{:doc "Returns the smallest of two or more Numbers."} _min_ min)
(def #^{:doc "Returns the greatest of two or more Numbers."} _max_ max)

(def #^{:doc "Returns the sum of a Number and one or more scalars."} _+ +)
(def #^{:doc "Returns the difference of a Number and one or more scalars."} _- -)
(def #^{:doc "Returns the product of a Number and one or more scalars."} _* *)
(def #^{:doc "Returns the quotient of a Number and one or more scalars."} _d /)
(def #^{:doc "Compares a Number and one or more scalars and returns true if they are in ascending order."} _< <)
(def #^{:doc "Compares a Number and one or more scalars and returns true if they are in descending order."} _> >)
(def #^{:doc "Returns the smallest of a Number and one or more scalars."} _min min)
(def #^{:doc "Returns the greatest of a Number and one or more scalars."} _max max)

(def #^{:doc "Returns the sum of a scalar and one or more Numbers."} +_ +)
(def #^{:doc "Returns the difference of a scalar and one or more Numbers."} -_ -)
(def #^{:doc "Returns the product of a scalar and one or more Numbers."} *_ *)
(def #^{:doc "Returns the quotient of a scalar and one or more Numbers."} d_ /)
(def #^{:doc "Compares a scalar and one or more Numbers and returns true if they are in ascending order."} <_ <)
(def #^{:doc "Compares a scalar and one or more Numbers and returns true if they are in descending order."} >_ >)
(def #^{:doc "Returns the smallest of a scalar and one or more Numbers."} min_ min)
(def #^{:doc "Returns the greatest of a scalar and one or more Numbers."} max_ max)

(defn rv-fn
  "Calls (apply f Xs)."
  [f & Xs]
  (apply (eval f) Xs))

(def #^{:doc "Returns the mean of a Number, which is itself."} rv-mean identity)

(def #^{:doc "Returns the variance of a Number, which is always 0.0."} rv-variance (constantly 0.0))

(def #^{:doc "Returns the standard deviation of a Number, which is always 0.0."} rv-stdev (constantly 0.0))

(defn rv-sum
  "Returns the sum of a sequence of Numbers."
  [Xs]
  (reduce + Xs))

(defn rv-extensive-sampler
  "Returns the extensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (rv-sum (map (fn [[val frac]] (_* val frac)) coverage)))

(defn rv-intensive-sampler
  "Returns the intensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (let [frac-sum (reduce + (map second coverage))]
    (rv-sum (map (fn [[val frac]] (_* val (/ frac frac-sum))) coverage))))

(defn rv-distribution-sampler
  "Returns the distribution of the means of a coverage (i.e. a
   sequence of pairs of [value fraction-covered])."
  [coverage]
  (rv-intensive-sampler coverage))

(def #^{:doc "Extracts a deterministic value from a Number by simply returning it."} draw identity)
(def #^{:doc "Returns n instances (or an infinite lazy sequence) of the passed-in Number."} draw-repeatedly repeat)
