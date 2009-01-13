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

(ns district.gssm
  (:refer-clojure)
  (:use [district.utils        :only (maphash)]
	[district.matrix-ops   :only (get-neighbors)]
	[district.service-defs :only (source-val compute-flows)]))

(defstruct location
  :coords :neighbors :features :sunk :used
  :consumed :carrier-bin :source :flows)

(defn make-location
  "Location constructor"
  [coords neighbors features benefit]
  (struct-map location
    :coords      coords
    :neighbors   neighbors
    :features    features
    :sunk        (ref 0.0)
    :used        (ref 0.0)
    :consumed    (ref 0.0)
    :carrier-bin (ref ())
    :source      (source-val benefit features)))

(defn extract-features
  "Returns a map of feature names to the observation values at i j."
  [observation i j cols]
  (maphash identity #(nth % (+ (* i cols) j)) observation))

(defn make-location-map
  "Returns a map of coords to location objects, one per matrix cell."
  [observation rows cols benefit]
  (loop [location-seq (for [i (range rows) j (range cols)]
			(make-location [i j]
				       (get-neighbors [i j] rows cols)
				       (extract-features observation i j cols)
				       benefit))
	 location-map {}]
    (if (empty? location-seq)
      location-map
      (let [this-location (first location-seq)]
	(recur (rest location-seq)
	       (assoc location-map
		 (:coords this-location) this-location))))))

(defn link-locations
  "Extracts the list of locations from location-map and returns a new
  list of locations in which each one's neighbors field has been
  transformed from a list of coords to a list of locations."
  [location-map]
  (map (fn [loc] (update-in loc [:neighbors] #(map location-map %)))
       (vals location-map)))

(defn add-flows
  "Call on the result of link-locations.  Adds to each location a
   flows field, which contains a delayed evaluation of the location's
   flow probabilities."
  [benefit locations]
  (map (fn [loc]
	 (assoc loc :flows
		(delay (compute-flows benefit
				      (:features loc)
				      (map :features (:neighbors loc))))))
       locations))

(defstruct service-carrier :weight :route)

(defn make-service-carrier
  "Service carrier constructor"
  [weight route]
  (struct-map service-carrier :weight weight :route route))

(defn start-carrier
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [loc carrier trans-threshold]
  (let [weight   (:weight carrier)
	flows    (force (:flows loc))
	sunk     (* weight (:sink flows))
	used     (* weight (:use flows))
	consumed (* weight (:consume flows))]
    (dosync
     (alter (:sunk        loc) +    sunk)
     (alter (:used        loc) +    used)
     (alter (:consumed    loc) +    consumed)
     (alter (:carrier-bin loc) conj carrier))
    (doseq [[loc trans-prob] (zipmap (:neighbors loc) (:out flows))]
	(let [trans-weight (* weight trans-prob)]
	  (when (> trans-weight trans-threshold)
	    (start-carrier loc
			   (make-service-carrier trans-weight
						 (conj (:route carrier) loc))
			   trans-threshold))))))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which all update properties of the locations.  When the
   simulation completes, the list of locations is returned."
  [benefit observation rows cols trans-threshold]
  (let [locations (add-flows benefit
			     (link-locations
			      (make-location-map observation rows cols benefit)))]
    (doseq [loc locations]
	(let [source (:source loc)]
	  (when (> source 0.0)
	    (start-carrier loc (make-service-carrier source [loc]) trans-threshold))))
    locations))

(defn- add-anyway
  "Sums the non-nil argument values."
  [x y]
  (cond (nil? x) y
	(nil? y) x
	:otherwise (+ x y)))

(defn find-provisionshed
  "Returns a map of {provider-coords -> benefit-provided}."
  [beneficiary-location]
  (let [flows (force (:flows beneficiary-location))
	absorption (+ (:use flows) (:consume flows))]
    (loop [carriers @(:carrier-bin beneficiary-location)
	   provider-contributions {}]
      (if (empty? carriers)
	provider-contributions
	(let [carrier (first carriers)]
	  (recur (rest carriers)
		 (update-in provider-contributions
			    [((comp :coords first :route) carrier)]
			    add-anyway (* absorption (:weight carrier)))))))))

(defn find-benefitshed
  "Returns a map of {beneficiary-coords -> benefit-received}."
  [provider-location all-locations]
  (loop [locations all-locations
	 beneficiary-acquisitions {}]
    (if (empty? locations)
      beneficiary-acquisitions
      (let [beneficiary-location (first locations)
	    flows (force (:flows beneficiary-location))
	    absorption (+ (:use flows) (:consume flows))]
	(recur (rest locations)
	       (assoc beneficiary-acquisitions
		 [(:coords beneficiary-location)]
		 (* absorption
		    (reduce + (map :weight
				   (filter #(= provider-location
					       ((comp first :route) %))
					   @(:carrier-bin beneficiary-location)))))))))))

(defn coord-map-to-matrix
  "Renders a map of {[i j] -> value} into a 2D matrix."
  [coord-map rows cols]
  (let [matrix (make-array Double/TYPE rows cols)]
    (doseq [[i j :as key] (keys coord-map)]
	(aset-double matrix i j (coord-map key)))
    matrix))
