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
  (:use [district.utils        :only (maphash seq2map seq2redundant-map)]
	[district.matrix-ops   :only (get-neighbors)]
	[district.service-defs :only (source-val compute-flows)]))

(defstruct location
  :id :neighbors :features :sunk :used
  :consumed :carrier-bin :source :flows)

(defn make-location
  "Location constructor"
  [id neighbors features benefit]
  (struct-map location
    :id          id
    :neighbors   neighbors
    :features    features
    :sunk        (ref 0.0)
    :used        (ref 0.0)
    :consumed    (ref 0.0)
    :carrier-bin (ref ())
    :source      (source-val benefit features)))

(defn- extract-features
  "Returns a map of feature names to the observed values at i j."
  [observation-states i j cols]
  (let [idx (+ (* i cols) j)]
    (maphash identity #(nth % idx) observation-states)))

(defmulti
  #^{:doc "Returns a map of ids to location objects, one per location in observation."}
  make-location-map (fn [observation benefit] (geospace/grid-extent? observation)))

(defmethod make-location-map true
  [observation benefit]
  (let [rows   (geospace/grid-rows observation)
	cols   (geospace/grid-columns observation)
	states (corescience/map-dependent-states observation)]
    (seq2map (for [i (range rows) j (range cols)]
	       (make-location [i j]
			      (get-neighbors [i j] rows cols)
			      (extract-features states i j cols)
			      benefit))
	     (fn [location] [(:id location) location]))))

(defmethod make-location-map false
  [observation benefit]
  {})
;;;  (let [districts (corescience/get-districts observation)]
;;;    (seq2map (map (fn [district id]
;;;		    (make-location id
;;;				   (corescience/get-poly-neighbors district)
;;;				   (corescience/get-district-features district)
;;;				   benefit))
;;;		  districts (range (count districts)))
;;;	     (fn [location] [(:id location) location]))))
;;; FIXME: affects: id, neighbors, features, source of location objects
;;; These corescience/* functions are not implemented.
;;; Location-map keys may not work with the return value of get-poly-neighbors.

(defn add-flows
  "Updates location-map such that each location's flows field will
   contain a delayed evaluation of its carrier flow probabilities."
  [benefit location-map]
  (maphash identity
	   #(assoc % :flows
		   (delay (compute-flows benefit
					 (:features %)
					 (map (comp :features location-map)
					      (:neighbors %)))))
	   location-map))

(defstruct service-carrier :weight :route)

(defn make-service-carrier
  "Service carrier constructor"
  [weight route]
  (struct-map service-carrier :weight weight :route route))

(defn propagate-carrier-tailrec
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [location-map location root-carrier trans-threshold]
  (loop [loc location
	 carrier root-carrier
	 open-list ()]
    (let [weight        (:weight carrier)
	  flows         (force (:flows loc))
	  sunk          (* weight (:sink flows))
	  used          (* weight (:use flows))
	  consumed      (* weight (:consume flows))
	  neighbors     (map location-map (:neighbors loc))
	  trans-weights (map #(* weight %) (:out flows))
	  trans-pairs   (concat (filter #(> (val %) trans-threshold)
					(zipmap neighbors trans-weights)) open-list)]
      (dosync
       (alter (:sunk        loc) +    sunk)
       (alter (:used        loc) +    used)
       (alter (:consumed    loc) +    consumed)
       (alter (:carrier-bin loc) conj carrier))
      (when (seq trans-pairs)
	(let [[next-loc trans-weight] (first trans-pairs)]
	  (recur next-loc
		 (make-service-carrier trans-weight
				       (conj (:route carrier) next-loc))
		 (rest trans-pairs)))))))

(defn propagate-carrier
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [location-map loc carrier trans-threshold]
  (let [weight        (:weight carrier)
	flows         (force (:flows loc))
	sunk          (* weight (:sink flows))
	used          (* weight (:use flows))
	consumed      (* weight (:consume flows))
	neighbors     (map location-map (:neighbors loc))
	trans-weights (map #(* weight %) (:out flows))
	trans-pairs   (filter #(> (val %) trans-threshold)
			      (zipmap neighbors trans-weights))]
    (dosync
     (alter (:sunk        loc) +    sunk)
     (alter (:used        loc) +    used)
     (alter (:consumed    loc) +    consumed)
     (alter (:carrier-bin loc) conj carrier))
    (doseq [[next-loc trans-weight] trans-pairs]
	(propagate-carrier location-map
			   next-loc
			   (make-service-carrier trans-weight
						 (conj (:route carrier) next-loc))
			   trans-threshold))))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which all update properties of the locations.  When the
   simulation completes, the network of locations is returned."
  [benefit observation trans-threshold]
  (let [location-map     (add-flows benefit (make-location-map observation benefit))
	src-locations    (filter #(> (:source %) 0.0) (vals location-map))
	num-locations    (count src-locations)
	completedThreads (atom 0)]
    (doseq [loc src-locations]
	(.start (Thread. (fn []
;;			   (println "Starting thread" (Thread/currentThread))
			   (propagate-carrier-tailrec location-map
						      loc
						      (make-service-carrier (:source loc) [loc])
						      trans-threshold)
;;			   (println "Stopping thread" (Thread/currentThread))
			   (swap! completedThreads inc)))))
    (while (< @completedThreads num-locations)
	   (Thread/sleep 100))
    location-map))

(defn- add-anyway
  "Sums the non-nil argument values."
  [x y]
  (cond (nil? x) y
	(nil? y) x
	:otherwise (+ x y)))

(defn find-provisionshed
  "Returns a map of {provider-id -> benefit-provided}."
  [beneficiary-location]
  (let [flows (force (:flows beneficiary-location))
	absorption (+ (:use flows) (:consume flows))]
    (seq2redundant-map @(:carrier-bin beneficiary-location)
		       (fn [carrier] [((comp :id first :route) carrier)
				      (* absorption (:weight carrier))])
		       add-anyway)))

(defn find-benefitshed
  "Returns a map of {beneficiary-id -> benefit-received}."
  [provider-location all-locations]
  (seq2map all-locations
	   (fn [location]
	     (let [flows (force (:flows location))
		   absorption (+ (:use flows) (:consume flows))]
	       [(:id location)
		(* absorption
		   (reduce + (map :weight
				  (filter #(= provider-location
					      ((comp first :route) %))
					  @(:carrier-bin location)))))]))))
