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
;;; This namespace defines a number of functions for creating,
;;; querying, and manipulating points, which are defined to be pair
;;; vectors [i j].

(ns clj-misc.point-algebra
  (:use [clj-misc.utils :only (contains-item? breadth-first-search)]))

(defn in-bounds?
  "Returns true if point is within bounds [[min-x max-x][min-y max-y]]
   and false otherwise.  Bounds are inclusive below and exclusive
   above."
  [[x y] [[min-x max-x][min-y max-y]]]
  (and (>= x min-x)
       (<  x max-x)
       (>= y min-y)
       (<  y max-y)))

(defn in-bounds-full?
  "Returns true if point is within bounds [[min-x max-x][min-y
   max-y]...] and false otherwise.  Works for any dimension."
  [[x y & more :as point] bounds]
  (assert (and bounds (== (count point) (count bounds))))
  (not (some false?
             (map (fn [x [min-x max-x]] (and (>= x min-x) (< x max-x)))
                  point bounds))))

(defn make-bounded-neighbor-generator
  "Returns a function that, given a point, generates a sequence of all
   neighboring points whose values are within the bounds, specified as
   follows: [[min-rows max-rows][min-cols max-cols]]."
  [bounds]
  (fn [[x y]]
    (filter (fn [point] (in-bounds? point bounds))
            (for [row-mod [-1 0 +1] col-mod [-1 0 +1]
                  :when (not (and (zero? row-mod) (zero? col-mod)))]
                  [(+ x row-mod) (+ y col-mod)]))))

(defn nearest-point-where
  "Returns the nearest point to origin-point which satisfies the test
   criteria or nil if no such point can be found within the bounds
   [[min-x max-x] [min-y max-y]]."
  [test origin-point bounds]
  (breadth-first-search (list origin-point)
                        nil
                        (make-bounded-neighbor-generator bounds)
                        test))

(defn make-point-list
  "Return a list of [x y] points."
  [rows cols num-points]
  (loop [r rows
         c cols
         n num-points
         points nil]
    (if (== n 0)
      points
      (let [point [(rand-int r) (rand-int c)]]
        (if (contains-item? points point)
          (recur r c n points)
          (recur r c (dec n) (cons point points)))))))
