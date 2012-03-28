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
        [clj-misc.utils :only (constraints-1.0 def- p & remove-nil-val-entries magnitude between? metric-distance with-message)]))

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

(defn add-ids
  [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn subtract-ids
  [[a b] [c d]]
  [(- a c) (- b d)])

(defn multiply-ids
  [[a b] [c d]]
  [(* a c) (* b d)])

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
  ([matrix]
     (zipmap (for [i (range (get-rows matrix)) j (range (get-cols matrix))] [i j])
             (matrix2seq matrix)))
  ([nil-val matrix]
     (remove-nil-val-entries nil-val (matrix2coord-map matrix))))

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

(defn invert-matrix
  [matrix]
  (vec (for [i (range (get-cols matrix))]
         (vec (for [j (range (get-rows matrix))]
                (get-in matrix [j i]))))))

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

(defn numeric-extensive-sampler
  "Returns the extensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (reduce + (map (fn [[val frac]] (* val frac)) coverage)))

(defn numeric-intensive-sampler
  "Returns the intensive weighted sum of a coverage (i.e. a sequence
   of pairs of [value fraction-covered])."
  [coverage]
  (let [frac-sum (reduce + (map second coverage))]
    (reduce + (map (fn [[val frac]] (* val (/ frac frac-sum))) coverage))))

(defn cell-fractions-covered
  [i l]
  (if (< l 1.0)
    ;; upsampling
    (let [low  (* i l)
          high (+ low l)
          next (Math/ceil low)]
      (cond (== low next)  ;; starting on a whole number
            (list [(int low) l])

            (== high next) ;; ending on a whole number
            (list [(int low) l])

            (> high next)  ;; we are spanning a number boundary
            (list [(int low) (- next low)] [(int high) (- high next)])

            :otherwise     ;; just grabbing a sample from the middle somewhere
            (list [(int low) l])))
    ;; downsampling
    (loop [low  (* i l)
           high (+ low l)
           next (Math/ceil low)
           fractions ()]
      (cond (== low next) ;; we started on a whole number (since l >= 1, we capture the entire cell)
            (recur (inc next)
                   high
                   (inc (inc next))
                   (conj fractions [(int low) 1.0]))

            (>= next high) ;; we're done (grab this cell and return fractions)
            (conj fractions [(int (Math/floor low)) (- high low)])

            :otherwise ;; still scanning (grab this cell and go on to the next)
            (recur next
                   high
                   (inc next)
                   (conj fractions [(int (Math/floor low)) (- next low)]))))))

(defn get-matrix-coverage
  [matrix l w i j]
  (remove (fn [[v f]] (nil? v))
          (for [[i* l*] (cell-fractions-covered i l)
                [j* w*] (cell-fractions-covered j w)]
            [(get-in matrix [i* j*]) (* l* w*)])))

(defn resample-matrix
  [new-rows new-cols sampling-fn matrix]
  {:pre [(every? #(and (pos? %) (integer? %)) [new-rows new-cols])]}
  (let [orig-rows (get-rows matrix)
        orig-cols (get-cols matrix)]
    (with-message
      (str "\nResampling matrix from " orig-rows " x " orig-cols " to " new-rows " x " new-cols "...\n")
      #(format "  Distinct Layer Values: [Pre] %d [Post] %d"
               (count (distinct (matrix2seq matrix)))
               (count (distinct (matrix2seq %))))
      (if (and (== orig-rows new-rows)
               (== orig-cols new-cols))
        matrix
        (let [cell-length (float (/ orig-rows new-rows))
              cell-width  (float (/ orig-cols new-cols))]
          (make-matrix new-rows
                       new-cols
                       (fn [[i j]] (sampling-fn (get-matrix-coverage matrix cell-length cell-width i j)))))))))

(defn divides?
  "Is y divisible by x? (i.e. x is the denominator)"
  [x y]
  (zero? (rem y x)))

(defn least-common-multiple
  [x y]
  (first (filter (p divides? x) (iterate (p + y) y))))

(defn resample-matrix-slow
  [new-rows new-cols sampling-fn matrix]
  (constraints-1.0 {:pre [(every? #(and (pos? %) (integer? %)) [new-rows new-cols])]})
  (newline)
  (println "Distinct Layer Values (pre-resampling):" (count (distinct (matrix2seq matrix))))
  (let [orig-rows (get-rows matrix)
        orig-cols (get-cols matrix)]
    (println "Resampling matrix from" orig-rows "x" orig-cols "to" new-rows "x" new-cols)
    (if (and (== orig-rows new-rows) (== orig-cols new-cols))
      matrix
      (let [lcm-rows              (least-common-multiple new-rows orig-rows)
            lcm-cols              (least-common-multiple new-cols orig-cols)
            upscale-factor-rows   (/ lcm-rows orig-rows)
            upscale-factor-cols   (/ lcm-cols orig-cols)
            lcm-matrix            (do (println "Making the lcm-matrix...")
                                      (if (and (== upscale-factor-rows 1)
                                               (== upscale-factor-cols 1))
                                        matrix
                                        (make-matrix lcm-rows lcm-cols
                                                     (fn [[i j]] (get-in matrix [(int (/ i upscale-factor-rows))
                                                                                 (int (/ j upscale-factor-cols))])))))
            downscale-factor-rows (/ lcm-rows new-rows)
            downscale-factor-cols (/ lcm-cols new-cols)]
        (let [diff1 (seq (apply clojure.set/difference (map (& set matrix2seq) [matrix lcm-matrix])))
              diff2 (seq (apply clojure.set/difference (map (& set matrix2seq) [lcm-matrix matrix])))]
          (if (or diff1 diff2)
            (println "Some values changed during upsampling:\nLost:" diff1 "\nAdded:" diff2)
            (println "No value changes during upsampling.")))
        (println "Making the final matrix...")
        (if (and (== downscale-factor-rows 1)
                 (== downscale-factor-cols 1))
          lcm-matrix
          (let [offset-range-rows (range downscale-factor-rows)
                offset-range-cols (range downscale-factor-cols)]
            (make-matrix new-rows new-cols 
                         (fn [[i j]] (let [i-base (* i downscale-factor-rows)
                                           j-base (* j downscale-factor-cols)]
                                       (sampling-fn (for [i-offset offset-range-rows j-offset offset-range-cols]
                                                      [(get-in lcm-matrix [(+ i-base i-offset) (+ j-base j-offset)]) 1.0])))))))))))

(defn in-bounds?
  "Returns true if the point is within the map bounds defined by
   [0 rows] and [0 cols], inclusive below and exclusive above."
  [rows cols [i j]]
  (and (>= i 0) (>= j 0) (< i rows) (< j cols)))

(defn on-bounds?
  "Returns true if the point occurs anywhere on the bounds
   [[0 rows][0 cols]]."
  [rows cols [i j]]
  (or (== i 0)
      (== i (dec rows))
      (== j 0)
      (== j (dec cols))))

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
     (printf-matrix matrix "%3s "))
  ([matrix format-string]
     (doseq [row (reverse (seq matrix))]
       (doseq [elt (seq row)]
         (printf format-string elt))
       (newline))))

(defn print-matrix-rotated
  [matrix]
  (let [rows (get-rows matrix)
        cols (get-cols matrix)]
    (dotimes [j cols]
      (dotimes [i rows]
        (printf "%s" (get-in matrix [(- (dec rows) i) j])))
      (newline))))

(defn rotate-2d-vec
  [theta [y x]]
  (let [t (- theta)]
    [(- (* y (Math/cos t)) (* x (Math/sin t)))
     (+ (* y (Math/sin t)) (* x (Math/cos t)))]))

(defn matrix-mult
  "Returns a new matrix whose values are the element-by-element
   products of the values in A and B."
  [A B]
  (map-matrix * A B))

(defn matrix-min
  "Returns the minimum value in the matrix or the minimum value above
   threshold if passed in."
  ([matrix]
     (apply min (for [row (seq matrix)] (apply min (seq row)))))
  ([matrix threshold]
     (if-let [vals-above-threshold (seq (remove nil? (for [row (seq matrix)]
                                                       (if-let [vals-above-threshold (seq (filter #(> % threshold) row))]
                                                         (apply min vals-above-threshold)))))]
       (apply min vals-above-threshold)
       threshold)))

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

(defn dist-to-steps
  [dir dist w h]
  (/ dist (magnitude (map * dir [h w]))))

(defn find-point-at-dist-in-m
  [id dir dist w h]
  (let [num-steps  (dist-to-steps dir dist w h)
        step-delta (map #(int (* num-steps %)) dir)]
    (map + id step-delta)))

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

          (zero? m) (map (fn [j] [pi j])
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
   [0-rows],[0-cols], inclusive below, exclusive above."
  [rows cols points]
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

(defn get-slope
  [[y1 x1] [y2 x2]]
  (let [dx (- x2 x1)]
    (if-not (zero? dx)
      (/ (- y2 y1) dx))))

(defn get-y-intercept
  [m [y1 x1]]
  (if m (- y1 (* m x1))))

(defstruct line :slope :intercept)

(defn get-line
  [[x1 y1 :as A] B]
  (let [m (get-slope A B)]
    (struct-map line
      :slope     m
      :intercept (if m
                   (get-y-intercept m A)
                   x1))))

(defn get-line-fn
  ([{m :slope, b :intercept}]
     (if m
       (fn [x] (+ (* m x) b))
       (fn [x] nil)))
  ([A B]
     (if-let [m (get-slope A B)]
       (let [b (get-y-intercept m A)]
         (fn [x] (+ (* m x) b)))
       (fn [x] nil))))

(defn normal-slope
  [m]
  (cond (nil? m)   0
        (zero? m)  nil
        :otherwise (- (/ m))))

(defn project-onto-line
  [{m :slope, b :intercept} [y1 x1 :as A]]
  (cond (nil? m)   [y1 b]
        (zero? m)  [b x1]
        :otherwise (let [m*          (normal-slope m)
                         b*          (get-y-intercept m* A)
                         x-intersect (/ (- b b*) (- m* m))
                         y-intersect (+ (* m x-intersect) b)]
                     [y-intersect x-intersect])))

(defn find-points-within-box
  "Points must be specified in either clockwise or counterclockwise order."
  [[y1 x1 :as A] [y2 x2 :as B] [y3 x3 :as C] [y4 x4 :as D]]
  (let [min-x (min x1 x2 x3 x4)
        max-x (max x1 x2 x3 x4)
        min-y (min y1 y2 y3 y4)
        max-y (max y1 y2 y3 y4)
        f1    (get-line-fn A B)
        f2    (get-line-fn B C)
        f3    (get-line-fn C D)
        f4    (get-line-fn D A)]
    (apply concat
           (for [x (range min-x (inc max-x))]
             (let [[low-y high-y] (sort
                                   (map #(Math/round (float %))
                                        (filter (p between? min-y max-y)
                                                (distinct (remove nil?
                                                                  [(f1 x) (f2 x) (f3 x) (f4 x)])))))]
               (for [y (range low-y (inc (or high-y low-y)))] [y x]))))))

(defn find-nearest
  [test? rows cols id]
  (some first (map (p filter test?) (take-while seq (iterate (p find-bounding-box rows cols) (list id))))))

(defn find-nearest-boring
  [test? rows cols id]
  (if (test? id)
    id
    (loop [bounding-box (get-neighbors rows cols id)]
      (if (seq bounding-box)
        (if-let [goal-point (first (filter test? bounding-box))]
          goal-point
          (recur (find-bounding-box rows cols bounding-box)))))))

(defn within-range?
  [dist-in-m cell-width cell-height origin id]
  (<= (metric-distance cell-width cell-height origin id) dist-in-m))

(defn find-in-range
  [test? dist-in-m cell-width cell-height rows cols origin]
  (let [iters (inc (int (/ dist-in-m (min cell-width cell-height))))]
    (filter #(and (within-range? dist-in-m cell-width cell-height origin %)
                  (test? %))
            (apply concat
                   (take iters
                         (iterate (p find-bounding-box rows cols)
                                  (list origin)))))))

(defn get-bearing
  [source destination]
  (let [[dx dy :as delta-vec] (subtract-ids destination source)
        delta-mag             (magnitude delta-vec)]
    [(/ dx delta-mag) (/ dy delta-mag)]))
