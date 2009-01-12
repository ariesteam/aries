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
	[district.service-defs :only (source-val sink-prob usage-prob
				      consumption-prob transition-prob)]))

(defstruct location
  :coords :paths :features :sunk :used :consumed
  :carrier-bin :source :sink :usage :consumption)

(defn make-location
  "Location constructor"
  [coords paths features benefit]
  (struct-map location
    :coords      coords
    :paths       paths
    :features    features
    :sunk        (ref 0.0)
    :used        (ref 0.0)
    :consumed    (ref 0.0)
    :carrier-bin (ref ())
    :source      (delay (source-val       benefit features))
    :sink        (delay (sink-prob        benefit features))
    :usage       (delay (usage-prob       benefit features))
    :consumption (delay (consumption-prob benefit features))))

(defn extract-features
  "Returns a map of feature names to the observation values at i j."
  [observation i j cols]
  (maphash identity #(nth % (+ (* i cols) j)) observation))

(defn make-locations
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

(defn coords-to-paths
  "Translates a list of location coordinates into a list of pairs
  [location transition-prob] as described in embed-paths."
  [coords-list location-map benefit src-features]
  (map #(let [dest-loc (location-map %)
	      dest-features (:features dest-loc)]
	  [dest-loc (delay (transition-prob benefit src-features dest-features))])
       coords-list))

(defn embed-paths
  "Extracts the locations from location-map and adds path information
   to them.  A path is a list of pairs ([dest1 prob1] [dest2 prob2]
   ... [destN probN]), where destI is a location reachable from this
   one and probI represents the likelihood of a unit of the service
   carrier travelling from here to destI."
  [location-map benefit]
  (let [update-paths #(update-in % [:paths] coords-to-paths
				 location-map benefit (:features %))
	add-total-path-prob #(assoc % :total-path-prob
				    (delay (reduce +
						   (map (comp force second)
							(:paths %)))))]
    (map (comp add-total-path-prob update-paths) (vals location-map))))

(defstruct service-carrier
  :weight :route)

(defn make-service-carrier
  "Service carrier constructor"
  [weight route]
  (struct-map service-carrier
    :weight weight
    :route route))

(defn start-carrier
  "A service carrier adds parts of its weight to the location's sunk,
   used, and consumed values based on location-specific probabilities.
   It then adds itself to the location's carrier-bin list and
   propagates service carriers to every neighboring cell for which
   (> (* (- weight sunk consumed) (/ trans-prob total-path-prob))
      trans-threshold)."
  [loc carrier trans-threshold]
  (let [weight    (:weight carrier)
	sunk      (* weight (force (:sink loc)))
	used      (* weight (force (:usage loc)))
	consumed  (* weight (force (:consumption loc)))
	outweight (- weight sunk consumed)]
    (dosync
     (alter (:sunk        loc) +    sunk)
     (alter (:used        loc) +    used)
     (alter (:consumed    loc) +    consumed)
     (alter (:carrier-bin loc) conj carrier))
    (doseq [[neighbor-loc trans-prob] (:paths loc)]
	(let [trans-weight (* outweight
			      (/ (force trans-prob)
				 (force (:total-path-prob loc))))]
	  (when (> trans-weight trans-threshold)
	    (start-carrier neighbor-loc
			   (make-service-carrier trans-weight
						 (conj
						  (:route carrier)
						  neighbor-loc))
			   trans-threshold))))))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0."
  [benefit observation rows cols trans-threshold]
  (let [locations (embed-paths (make-locations observation rows cols benefit) benefit)]
    (doseq [loc locations]
	(let [source (force (:source loc))]
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
  (loop [absorption (+ (force (:consumption beneficiary-location))
		       (force (:usage beneficiary-location)))
	 carriers @(:carrier-bin beneficiary-location)
	 provider-contributions {}]
    (if (empty? carriers)
      provider-contributions
      (let [carrier (first carriers)]
	(recur absorption
	       (rest carriers)
	       (update-in provider-contributions
			  [((comp :coords first :route) carrier)]
			  add-anyway (* absorption (:weight carrier))))))))

(defn find-benefitshed
  "Returns a map of {beneficiary-coords -> benefit-received}."
  [provider-location all-locations]
  (loop [locations all-locations
	 beneficiary-acquisitions {}]
    (if (empty? locations)
      beneficiary-acquisitions
      (let [beneficiary-location (first locations)
	    absorption (+ (force (:consumption beneficiary-location))
			  (force (:usage beneficiary-location)))]
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
