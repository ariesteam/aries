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

(ns district.matrix-ops
  (:refer-clojure)
  (:use [district.utils :only (contains-item? breadth-first-search)]))

;;; Within this namespace, these definitions hold:
;;; POINT:  a 2 element vector [x y]
;;; MATRIX: a 2D Java array
;;;
;;; Currently matrix-mult and normalize-matrix depend on matrices
;;; obeying this definition.  All other functions will work for any
;;; sequence of sequences.

(defn in-bounds?
  "Returns true if point is within bounds [[min-x max-x][min-y max-y]]
   and false otherwise."
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

(defn get-neighbors
  "Return a sequence of neighboring points within the map bounds."
  [point rows cols]
  (let [[i j] point]
    (filter (fn [[i j]] (and (>= i 0) (>= j 0) (< i rows) (< j cols)))
            (map #(vector (+ i %1) (+ j %2))
		 [-1 -1 -1 0 0 1 1 1]
		 [-1 0 1 -1 1 -1 0 1]))))

(defn make-bounded-neighbor-generator
  "Returns a function that, given a point, generates a sequence of all
   neighboring points whose values are within the bounds, specified as
   follows: [[0 10][0 6]]."
  [bounds]
  (fn [[x y]]
    (filter (fn [point] (in-bounds? point bounds))
	    (for [row-mod [dec identity inc] col-mod [dec identity inc]
		  :when (not (and (= row-mod identity) (= col-mod identity)))]
	      [(row-mod x) (col-mod y)]))))

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

(defn print-matrix
  "Pretty prints a matrix to *out* according to format-string."
  ([matrix]
     (print-matrix matrix "%3s "))
  ([matrix format-string]
     (doseq [row (seq matrix)]
       (doseq [elt (seq row)]
         (printf format-string elt))
       (newline))))

(defn add-or-nil
  "Return a + b unless a or b are nil, in which case we return nil."
  [a b]
  (when-not (or (nil? a) (nil? b))
    (+ a b)))

(defn mult-or-nil
  "Return a * b unless a or b are nil, in which case we return nil."
  [a b]
  (when-not (or (nil? a) (nil? b))
    (* a b)))

(defn matrix-mult
  "Return a new matrix which is the scalar product of matrixA and
   matrixB (element by element).  This only works for 2D arrays.  Any
   nil elements will simply force the final element in that location
   to also be nil."
  [matrixA matrixB]
  (let [rows-A (alength matrixA)
	cols-A (alength (first matrixA))
	rows-B (alength matrixB)
	cols-B (alength (first matrixB))]
    (assert (and (== rows-A rows-B) (== cols-A cols-B)))
    (let [matrixC (aclone matrixA)]
      (dotimes [i rows-A]
	(dotimes [j cols-A]
	  (aset matrixC i j (mult-or-nil (aget matrixC i j) (aget matrixB i j)))))
      matrixC)))

(defn matrix-max
  "Returns the maximum value of a 2D array."
  [matrix]
  (apply max (for [row (seq matrix)] (apply max (seq row)))))

(defn normalize-matrix
  "Normalizes the values in a 2D array to the interval [0,1]."
  [matrix]
  (let [rows (alength matrix)
	cols (alength (first matrix))
	normalized-matrix (aclone matrix)
	max-val (matrix-max matrix)]
    (if (== max-val 0.0)
      matrix
      (dotimes [i rows]
	  (dotimes [j cols]
	      (aset normalized-matrix i j (/ (aget matrix i j) max-val)))))
    normalized-matrix))
