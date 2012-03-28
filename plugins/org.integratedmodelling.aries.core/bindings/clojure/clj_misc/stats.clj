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
;;; This namespace defines statistical functions.

(ns clj-misc.stats)

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
  (/ (inc (- (/ x sigma) mu)) 2))
