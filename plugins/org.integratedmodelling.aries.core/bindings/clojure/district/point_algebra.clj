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

(ns district.point-algebra
  (:refer-clojure))

(defn get-normal-val
  "Returns a value from X~N(0,1).
   Uses the Marsaglia polar method."
  []
  (let [v1 (- 1 (* 2 (rand 1.0)))
	v2 (- 1 (* 2 (rand 1.0)))
	s  (+ (Math/pow v1 2) (Math/pow v2 2))]
    (if (and (not= s 0.0) (< s 1.0))
      (let [theta (Math/sqrt (/ (* -2 (Math/log s)) s))]
	(* v2 theta))
      (get-normal-val))))

(defn add-points
  "Returns a new point whose values are the sum of the two passed in."
  [pointA pointB]
  (assert (and pointA pointB (== (count pointA) (count pointB))))
  (vec (map + pointA pointB)))

(defn subtract-points
  "Returns a new point whose values are the difference of the two passed in."
  [pointA pointB]
  (assert (and pointA pointB (== (count pointA) (count pointB))))
  (vec (map - pointA pointB)))

(defn square-point
  "Returns a new point whose values are the squares of the one passed in."
  [point]
  (vec (map #(Math/pow % 2) point)))

(defn div-point-by-scalar
  "Returns a new point whose values are divided by scalar."
  [point scalar]
  (assert (not= scalar 0))
  (vec (map #(/ % scalar) point)))

(defn div-points
  "Returns a new point whose values are the quotient of the two passed in."
  [pointA pointB]
  (assert (and pointA pointB (== (count pointA) (count pointB))))
  (vec (map (fn [val1 val2] (when-not (== val2 0) (/ val1 val2))) pointA pointB)))

(defn average-points
  "Returns a point vector whose values are the averages of the elements
   in point-list."
  [point-list]
  (assert (> (count point-list) 0))
  (let [number-of-points (count point-list)
	point-sum (reduce add-points point-list)]
    (vec (map #(/ % number-of-points) point-sum))))

(defn zero-point
  "Returns a new zero-valued n-dimensional point."
  [n]
  (vec (replicate n 0.0)))

(defn make-point
  "Return an n-element vector representing the coordinates of a point
   in n-dimensional space.  Currently, these are drawn from X~N(0,1)
   and can optionally be centered around a passed in n-dimensional
   origin point with a different mean and stdev."
  ([n]
     (vec (take n (repeatedly #(+ 0.0 (* 1.0 (get-normal-val)))))))
  ([n origin]
     (vec (add-points origin (take n (repeatedly #(+ 0.0 (* 1.0 (get-normal-val))))))))
  ([n origin mean]
     (vec (add-points origin (take n (repeatedly #(+ mean (* 1.0 (get-normal-val))))))))
  ([n origin mean stdev]
     (vec (add-points origin (take n (repeatedly #(+ mean (* stdev (get-normal-val)))))))))

(defn make-point-list
  "Return a list of k n-dimensional points centered around an
  optionally passed in n-dimensional origin point."
  ([k n]
     (take k (repeatedly #(make-point n))))
  ([k n origin]
     (take k (repeatedly #(make-point n origin))))
  ([k n origin mean]
     (take k (repeatedly #(make-point n origin mean))))
  ([k n origin mean stdev]
     (take k (repeatedly #(make-point n origin mean stdev)))))

(defn euclidean-distance
  "Returns the Euclidean distance between pointA and pointB."
  [pointA pointB]
  (assert (and pointA pointB (== (count pointA) (count pointB))))
  (Math/sqrt (reduce + (map (fn [a b] (Math/pow (- a b) 2)) pointA pointB))))
