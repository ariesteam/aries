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

(ns district.worldgen
  (:refer-clojure)
  (:use [district.breadcrumb-agents :only (make-agent-list agent-stuck? perform-next-action!)]
	[district.matrix-ops :only (make-point-list)]
	[district.utils :only (expand-runtime-encoded-vector)]))

(defn get-neighbor-val
  "Return the value of the first non-nil neighbor to location (i,j) or
   nil if all neighbors have nil values."
  [matrix i j rows cols]
  (or (when (>= (dec i) 0)
        (aget matrix (dec i) j))
      (when (< (inc i) rows)
        (aget matrix (inc i) j))
      (when (>= (dec j) 0)
        (aget matrix i (dec j)))
      (when (< (inc j) cols)
        (aget matrix i (inc j)))))

(defn empty-cells?
  "Return true if there are nil-valued cells in matrix and nil
   otherwise."
  [matrix rows cols]
  (some nil?
	(for [i (range rows) j (range cols)] (aget matrix i j))))

(defn fill-empty-cells!
  "Iterate over matrix, assigning each empty cell the value of its
   nearest neighbor.  Repeat until no empty cells are left."
  [matrix rows cols]
  (while (empty-cells? matrix rows cols)
	 (dotimes [i rows]
	     (dotimes [j cols]
		 (when (nil? (aget matrix i j))
		   (aset matrix i j (get-neighbor-val matrix i j rows cols))))))
  matrix)

(defn make-world
  "Returns a rows by cols matrix, which has been partitioned into a
   number of regions matching the passed region-specs (a vector of the
   form [:region-type1 num-instances1 :region-type2 num-instances2
   ...]  Ex: [:water 2 :forest 5]"
  [rows cols region-specs]
  (let [matrix (make-array Object rows cols)
	region-types (expand-runtime-encoded-vector region-specs)
	agent-starting-points (make-point-list rows cols (count region-types))]
    (loop [agents (make-agent-list region-types agent-starting-points)]
      (if (empty? agents)
	(fill-empty-cells! matrix rows cols)
	(recur (for [agent agents :when (not (agent-stuck? agent matrix rows cols))]
		 (perform-next-action! agent matrix rows cols)))))))
