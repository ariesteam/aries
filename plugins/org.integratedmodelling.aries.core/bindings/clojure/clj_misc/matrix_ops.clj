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
;;; querying, and manipulating matrices, which are defined to be
;;; vectors of vectors.

(ns clj-misc.matrix-ops
  (:use [clojure.set    :only (map-invert)]
        [clj-misc.utils :only (constraints-1.0 def- p &)]))

(defn get-rows [matrix] (count matrix))
(defn get-cols [matrix] (count (first matrix)))

(defn is-matrix?
  [matrix]
  (and
   (vector? matrix)
   (vector? (first matrix))))

(defn make-matrix
  "Creates a rows x cols vector of vectors whose states are generated
   by calling val-fn on the [i j] coordinate pair."
  [rows cols val-fn]
  ;; Empirically, I've seen that for less than 100000 cells, map is
  ;; more efficient than pmap and vice versa, unless the matrix has
  ;; many more rows than cols.
  (vec ((if (and (>= (* rows cols) 100000)
                 (>  (* cols 10)   rows))
          pmap
          map)
        (fn [i] (vec (map (fn [j] (val-fn [i j]))
                          (range cols))))
        (range rows))))

(defn filter-matrix-for-coords
  [pred? matrix]
  (filter (fn [id] (pred? (get-in matrix id)))
          (for [i (range (get-rows matrix)) j (range (get-cols matrix))] [i j])))

(defn subtract-ids
  [[a b] [c d]]
  [(- a c) (- b d)])

(def- delta-codes {[ 0  1] (byte -1)    ; e
                   [-1  1] (byte -2)    ; se
                   [-1  0] (byte -4)    ; s
                   [-1 -1] (byte -8)    ; sw
                   [ 0 -1] (byte -16)   ; w
                   [ 1 -1] (byte -32)   ; nw
                   [ 1  0] (byte -64)   ; n
                   [ 1  1] (byte -128)}) ; ne
(def- undelta-codes (map-invert delta-codes))

(defn bitpack-route
  [id-seq]
  (into-array Byte/TYPE (map (& delta-codes subtract-ids) (rest id-seq) id-seq)))

(defn unbitpack-route
  [source-id bytecodes]
  (reduce (fn [ids delta] (conj ids (vec (map + delta (peek ids)))))
          [source-id]
          (map undelta-codes bytecodes)))

(defn seq2matrix
  "Creates a rows x cols vector of vectors whose states are
   the successive elements of aseq."
  [rows cols aseq]
  (constraints-1.0 {:pre [(== (count aseq) (* rows cols))]})
  (vec (map vec (partition cols aseq))))

(defn matrix2seq
  "Returns the contents of a matrix as a single sequence by
   concatenating all of its rows."
  [matrix]
  (apply concat matrix))

(defn matrix2coord-map
  [matrix]
  (zipmap (for [i (get-rows matrix) j (get-cols matrix)] [i j])
          (matrix2seq matrix)))

(defn coord-map2matrix
  [rows cols nil-val coord-map]
  (make-matrix rows cols #(get coord-map % nil-val)))

(defn coord-map2matrix-array
  [rows cols coord-map]
  (let [matrix (make-array (class (val (first coord-map))) rows cols)]
    (doseq [[i j :as key] (keys coord-map)]
      (aset matrix i j (coord-map key)))
    matrix))

(defn grids-align?
  "Verifies that all matrices have the same number of rows and
   columns."
  [& matrices]
  (and (== 1 (count (distinct (map get-rows matrices))))
       (== 1 (count (distinct (map get-cols matrices))))))

(defn map-matrix
  "Maps a function f over the values in matrix, returning a new
   matrix."
  ([f matrix]
     (vec (map (fn [row] (vec (map f row))) matrix)))
  ([f matrix & matrices]
     (constraints-1.0 {:pre [(apply grids-align? matrix matrices)]})
     (let [matrices (cons matrix matrices)]
       (make-matrix (get-rows matrix) (get-cols matrix)
                    (fn [coords] (apply f (map #(get-in % coords) matrices)))))))

(defn divides?
  "Is y divisible by x? (i.e. x is the denominator)"
  [x y]
  (zero? (mod y x)))

(defn least-common-multiple
  [x y]
  (first (filter (p divides? x) (iterate (p + y) y))))

(defn resample-matrix
  [new-rows new-cols aggregator-fn matrix]
  (constraints-1.0 {:pre [(every? #(and (pos? %) (integer? %)) [new-rows new-cols])]})
  (let [orig-rows             (get-rows matrix)
        orig-cols             (get-cols matrix)]
    (if (and (== orig-rows new-rows) (== orig-cols new-cols))
      matrix
      (let [lcm-rows              (least-common-multiple new-rows orig-rows)
            lcm-cols              (least-common-multiple new-cols orig-cols)
            upscale-factor-rows   (/ lcm-rows orig-rows)
            upscale-factor-cols   (/ lcm-cols orig-cols)
            lcm-matrix            (if (and (== upscale-factor-rows 1)
                                           (== upscale-factor-cols 1))
                                    matrix
                                    (make-matrix lcm-rows lcm-cols
                                                 (fn [[i j]] (get-in matrix [(int (/ i upscale-factor-rows))
                                                                             (int (/ j upscale-factor-cols))]))))
            downscale-factor-rows (/ lcm-rows new-rows)
            downscale-factor-cols (/ lcm-cols new-cols)]
        (if (and (== downscale-factor-rows 1)
                 (== downscale-factor-cols 1))
          lcm-matrix
          (let [offset-range-rows (range downscale-factor-rows)
                offset-range-cols (range downscale-factor-cols)]
            (make-matrix new-rows new-cols 
                         (fn [[i j]] (let [i-base (* i downscale-factor-rows)
                                           j-base (* j downscale-factor-cols)]
                                       (aggregator-fn (for [i-offset offset-range-rows j-offset offset-range-cols]
                                                        (get-in lcm-matrix [(+ i-base i-offset) (+ j-base j-offset)]))))))))))))

(defn in-bounds?
  "Returns true if the point is within the map bounds defined by
   [0 rows] and [0 cols], inclusive below and exclusive above."
  [rows cols [i j]]
  (and (>= i 0) (>= j 0) (< i rows) (< j cols)))

(defn get-neighbors
  "Return a sequence of neighboring points within the map bounds."
  [rows cols [i j]]
  (filter (p in-bounds? rows cols)
          (map #(vector (+ i %1) (+ j %2))
               [-1 -1 -1  0 0  1 1 1]
               [-1  0  1 -1 1 -1 0 1])))

(defn print-matrix
  ([matrix]
     (dotimes [i (get-rows matrix)]
       (dotimes [j (get-cols matrix)]
         (print (get-in matrix [i j]) ""))
       (newline)))
  ([matrix & matrices]
     (print-matrix matrix)
     (newline)
     (apply print-matrix matrices)))

(defn printf-matrix
  "Pretty prints a matrix to *out* according to format-string. Index
   [0,0] will be on the bottom left corner."
  ([matrix]
     (print-matrix matrix "%3s "))
  ([matrix format-string]
     (doseq [row (reverse (seq matrix))]
       (doseq [elt (seq row)]
         (printf format-string elt))
       (newline))))

(defn matrix-mult
  "Returns a new matrix whose values are the element-by-element
   products of the values in A and B."
  [A B]
  (map-matrix * A B))

(defn matrix-max
  "Returns the maximum value in the matrix."
  [matrix]
  (apply max (for [row (seq matrix)] (apply max (seq row)))))

(defn normalize-matrix
  "Normalizes the values in the matrix to the interval [0,1]."
  [matrix]
  (let [max-val (matrix-max matrix)]
    (if (zero? max-val)
      matrix
      (map-matrix #(/ % max-val) matrix))))

(defn find-line-between
  "Returns the sequence of all points [i j] intersected by the line
   from provider to beneficiary.  Since this is calculated over a
   regular integer-indexed grid, diagonal lines will be approximated
   by lines bending at right angles along the p-to-b line.  This
   calculation imagines the indeces of each point to be located at the
   center of a square of side length 1.  Note that the first point in
   each path will be the provider id, and the last will be the
   beneficiary id.  If provider=beneficiary, the path will contain
   only this one point."
  [[pi pj] [bi bj]]
  (let [m (if (not= pj bj) (/ (- bi pi) (- bj pj)))
        b (if m (- pi (* m pj)))
        f (fn [x] (+ (* m x) b))]
    (cond (nil? m) (map (fn [i] [i pj])
                        (if (< pi bi)
                          (range pi (inc bi))
                          (range pi (dec bi) -1)))

          (== m 0) (map (fn [j] [pi j])
                        (if (< pj bj)
                          (range pj (inc bj))
                          (range pj (dec bj) -1)))

          :otherwise (let [get-i-range
                           (cond (and (< pi bi) (< pj bj))
                                 (fn [j] (let [left-i  (int (Math/round (f (- j (if (== j pj) 0.0 0.5)))))
                                               right-i (int (Math/round (f (+ j (if (== j bj) 0.0 0.5)))))]
                                           (range left-i (inc right-i))))
                           
                                 (and (< pi bi) (> pj bj))
                                 (fn [j] (let [left-i  (int (Math/round (f (- j (if (== j bj) 0.0 0.5)))))
                                               right-i (int (Math/round (f (+ j (if (== j pj) 0.0 0.5)))))]
                                           (range right-i (inc left-i))))
                           
                                 (and (> pi bi) (< pj bj))
                                 (fn [j] (let [left-i  (int (Math/round (f (- j (if (== j pj) 0.0 0.5)))))
                                               right-i (int (Math/round (f (+ j (if (== j bj) 0.0 0.5)))))]
                                           (range left-i  (dec right-i) -1)))
                           
                                 (and (> pi bi) (> pj bj))
                                 (fn [j] (let [left-i  (int (Math/round (f (- j (if (== j bj) 0.0 0.5)))))
                                               right-i (int (Math/round (f (+ j (if (== j pj) 0.0 0.5)))))]
                                           (range right-i (dec left-i)  -1))))
                           j-range (if (< pj bj)
                                     (range pj (inc bj))
                                     (range pj (dec bj) -1))]
                       (for [j j-range i (get-i-range j)] [i j])))))

(defn find-bounding-box
  "Returns a new list of points which completely bounds the
   rectangular region defined by points and remains within the bounds
   [0-rows],[0-cols]."
  [points rows cols]
  (when (seq points)
    (let [row-coords (map first  points)
          col-coords (map second points)
          min-i (apply min row-coords)
          min-j (apply min col-coords)
          max-i (apply max row-coords)
          max-j (apply max col-coords)
          bottom (dec min-i)
          top    (inc max-i)
          left   (dec min-j)
          right  (inc max-j)]
      (concat
       (when (>= left   0)    (for [i (range min-i top) j [left]]     [i j]))
       (when (<  right  cols) (for [i (range min-i top) j [right]]    [i j]))
       (when (>= bottom 0)    (for [i [bottom] j (range min-j right)] [i j]))
       (when (<  top    rows) (for [i [top]    j (range min-j right)] [i j]))
       (when (and (>= left 0)     (<  top rows)) (list [top left]))
       (when (and (>= left 0)     (>= bottom 0)) (list [bottom left]))
       (when (and (<  right cols) (<  top rows)) (list [top right]))
       (when (and (<  right cols) (>= bottom 0)) (list [bottom right]))))))
