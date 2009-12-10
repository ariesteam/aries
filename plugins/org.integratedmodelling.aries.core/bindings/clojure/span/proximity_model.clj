;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.proximity-model
  (:use [misc.matrix-ops :only (get-neighbors)]
	[span.randvars   :only (scalar-rv-subtract rv-multiply rv-scalar-multiply rv-scalar-divide rv-mean)]
	[span.model-api  :only (distribute-flow! decay undecay service-carrier distribute-load-over-processors)]
	[span.analyzer   :only (source-loc? sink-loc? use-loc?)]
	[span.params     :only (*trans-threshold*)]))

(defn- expand-box
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

;; FIXME convert step to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod decay "Proximity"
  [_ weight step] (rv-scalar-divide weight (* step step)))

;; FIXME convert step to distance metric based on map resolution and make this gaussian to 1/2 mile
(defmethod undecay "Proximity"
  [_ weight step] (rv-scalar-multiply weight (* step step)))

(defn- make-frontier-element
  [location-map location-id decayed-weight [sunk-weight route delayed-ops]]
  (let [location      (location-map location-id)
	sunk-weight   (if (sink-loc? location)
			(rv-multiply sunk-weight (scalar-rv-subtract 1.0 (:sink location)))
			sunk-weight)
	new-route     (conj route location)
	delayed-ops   (cond (use-loc? location)  (do (doseq [op delayed-ops] (op))
						     (swap! (:carrier-cache location) conj
							    (struct service-carrier decayed-weight new-route))
						     [])
			    (sink-loc? location) (conj delayed-ops
						       #(swap! (:carrier-cache location) conj
							       (struct service-carrier decayed-weight new-route)))
			    :otherwise           delayed-ops)]
    [location-id [sunk-weight new-route delayed-ops]]))

(defn- distribute-gaussian!
  [location-map rows cols source-location source-weight]
  (loop [decayed-sources (map #(decay source-weight %) (iterate inc 1))
	 frontier        (into {} (make-frontier-element location-map (:id source-location) source-weight [source-weight [] []]))]
    (when (seq frontier)
      (let [decayed-weight (first decayed-sources)]
	(if (> (rv-mean decayed-weight) *trans-threshold*)
	  (recur (rest decayed-sources)
		 (into {}
		       (remove nil?
			       (for [boundary-id (expand-box (keys frontier) rows cols)]
				 (when-let [frontier-options (remove nil? (map frontier (get-neighbors boundary-id rows cols)))]
				   (make-frontier-element location-map boundary-id decayed-weight
							  (apply max-key (fn [[s r d]] (rv-mean s)) frontier-options))))))))))))

(defmethod distribute-flow! "Proximity"
  [_ location-map rows cols]
  (distribute-load-over-processors
   (fn [_ source-location] (distribute-gaussian! location-map rows cols source-location (:source source-location)))
   (filter source-loc? (vals location-map))))
