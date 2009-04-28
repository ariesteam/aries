;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-GSSM.
;;;
;;; CLJ-GSSM is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-GSSM is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-GSSM.  If not, see <http://www.gnu.org/licenses/>.

(ns gssm.line-of-sight-model
  (:refer-clojure)
  (:use [misc.utils     :only (euclidean-distance)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]))

(defn distribute-raycast!
  [location-seq decay-rate]
  (let [source-val ((comp force :source first) location-seq)]
    (loop [traversed-locs   []
           untraversed-locs location-seq]
      (let [current-loc (first untraversed-locs)]
        (if (or (> (force (:sink current-loc)) 0.0) (empty? (rest untraversed-locs)))
          (dosync
           (commute (:carrier-cache current-loc) conj
                    (struct service-carrier
                            (* source-val (Math/pow decay-rate (count traversed-locs)))
                            traversed-locs))))
	(recur (conj traversed-locs current-loc)
	       (rest untraversed-locs))))))

(defn find-viewpaths
  "Returns a sequence of paths (one for each combination of p and b),
   where a path is represented by the sequence of all points [i j]
   intersected by the line from p to b for each p,b pair.  Since this
   is calculated over a regular integer-indexed grid, diagonal lines
   will be approximated by lines bending at right angles along the
   p-to-b line.  This calculation imagines the indeces of each point
   to be located at the center of a square of side length 1.  Note
   that the first point in each path will be its p, and the last will
   be its b.  If p=b, the path will contain only this one point."
  [providers beneficiaries]
  (for [p providers b beneficiaries]
    (let [[pi pj] (:id p)
          [bi bj] (:id b)
          m (if (not= pj bj) (/ (- bi pi) (- bj pj)))
          b (if m (- pi (* m pj)))
          f (fn [x] (+ (* m x) b))]
      (if m
        (let [j-range (if (< pj bj) (range pj (inc bj)) (range pj (dec bj) -1))]
          (if (== m 0)
            (for [j j-range] [pi j])
            (concat               
             (for [j j-range]
               (let [left-i  (Math/round (f (- j 1/2)))
                     right-i (Math/round (f (+ j 1/2)))
                     i-range (cond (and (< pi bi) (< pj bj)) (range left-i  (inc right-i))
                                   (and (< pi bi) (> pj bj)) (range right-i (inc left-i))
                                   (and (> pi bi) (< pj bj)) (range left-i  (dec right-i) -1)
                                   (and (> pi bi) (> pj bj)) (range right-i (dec left-i)  -1))]
                 (for [i i-range] [i j]))))))
        (let [i-range (if (< pi bi) (range pi (inc bi)) (range pi (dec bi) -1))]
          (for [i i-range] [i pj]))))))

(defn distance-within-range?
  [location-seq path-length decay-rate trans-threshold]
  (>
   (* ((comp force :source first) location-seq)
      (Math/pow decay-rate path-length))
   trans-threshold))

(defn no-elevation-interference?
  [location-seq path-length]
  (let [source-loc  (first location-seq)
        use-loc     (last  location-seq)
        source-elev ((:flow-features source-loc) "Elevation")
        rise        (- ((:flow-features use-loc) "Elevation") source-elev)
        run         (euclidean-distance (:id source-loc) (:id use-loc))
        view-slope  (/ rise run)
        step-size   (/ euclidean-distance path-length)]
    (every? (fn [[loc steps-from-source]]
              (< ((:flow-features loc) "Elevation")
                 (+ source-elev (* view-slope steps-from-source step-size))))
            (rest (butlast (zipmap location-seq (range (inc path-length))))))))

(defmethod distribute-flow! "LineOfSight"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (let [locations     (vals location-map)
        providers     (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries (filter #(> (force (:use %)) 0.0) locations)]
    (distribute-load-over-processors
     (fn [_ viewpath] (distribute-raycast! viewpath decay-rate))
     (filter #(let [path-length (dec (count %))]
                (and (distance-within-range? % path-length decay-rate trans-threshold)
                     (no-elevation-interference? % path-length)))
             (map #(map location-map %)
                  (find-viewpaths providers beneficiaries))))))
