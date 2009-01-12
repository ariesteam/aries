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

(ns district.stats
  (:refer-clojure))

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
