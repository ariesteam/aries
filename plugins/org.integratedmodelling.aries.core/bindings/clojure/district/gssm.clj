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
  (:use [district.utils          :only (maphash seq2map seq2redundant-map)]
	[district.matrix-ops     :only (get-neighbors)]
	[district.service-defs   :only (source-val compute-flows)]
	[district.discretization :only (discretization-table)]))

(defstruct location
  :id :neighbors :source-features :sink-features
  :sunk :used :consumed :carrier-bin :source :flows)

(defn make-location
  "Location constructor"
  [id neighbors source-features sink-features benefit-source-name source-inference-engine]
  (do
    (println "ID:" id)
    (println "Neighbors:" neighbors)
    (println "Source-Features:" source-features)
    (println "Sink-Features:" sink-features)
    (println "Benefit-Source-Name:" benefit-source-name)
    (println "Source-Inference-Engine:" source-inference-engine)
  (struct-map location
    :id              id
    :neighbors       neighbors
    :source-features source-features
    :sink-features   sink-features
    :sunk            (ref 0.0)
    :used            (ref 0.0)
    :consumed        (ref 0.0)
    :carrier-bin     (ref ())
    :source          (source-val benefit-source-name
				 source-inference-engine
				 source-features))))

(defn- discretize-value
  "Transforms the value to the string name of its corresponding
   discretized value, using the particular concept's transformation
   procedure in the global discretization-table."
  [concept-name value]
  ((discretization-table concept-name) value))

(defn- extract-features
  "Returns a map of feature names to the observed values at i j."
  [observation-states idx]
  (seq2map (seq observation-states)
	   (fn [[feature-name values]]
	     [feature-name (discretize-value feature-name (nth values idx))])))

(defmulti
  #^{:doc "Returns a map of ids to location objects, one per location in observation."}
  make-location-map (fn [benefit-source source-observation sink-observation]
		      (and (geospace/grid-extent? source-observation)
			   (geospace/grid-extent? sink-observation))))

(defmethod make-location-map true
  [benefit-source source-observation sink-observation]
  (let [rows                    (geospace/grid-rows source-observation)
	cols                    (geospace/grid-columns source-observation)
	benefit-source-name     (.getLocalName benefit-source)
	source-inference-engine (aries/make-bn-inference benefit-source)
	source-states           (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states source-observation))
	sink-states             (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states sink-observation))]
    (do
      (println "Rows: " rows)
      (println "Cols: " cols)
      (println "Benefit-Source-Name: " benefit-source-name)
      (println "Source-Inference-Engine: " source-inference-engine)
      (println "Source-States: " (seq (vals source-states)))
      (println "Sink-States: " (seq (vals (sink-states))))
    (seq2map (for [i (range rows) j (range cols)]
	       (let [feature-idx (+ (* i cols) j)]
		 (do (println "MAKING LOCATION [" i " " j "]")
		 (make-location [i j]
				(get-neighbors [i j] rows cols)
				(extract-features source-states feature-idx)
				(extract-features sink-states feature-idx)
				benefit-source-name
				source-inference-engine))))
	     (fn [location] [(:id location) location])))))

(defmethod make-location-map false
  [benefit-source source-observation sink-observation]
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
  [benefit-sink location-map]
  (let [benefit-sink-name     (.getLocalName benefit-sink)
	sink-inference-engine (aries/make-bn-inference benefit-sink)]
    (maphash identity
	     #(assoc % :flows
		     (delay (compute-flows benefit-sink-name
					   sink-inference-engine
					   (:sink-features %)
					   (map (comp :sink-features location-map) (:neighbors %)))))
	     location-map)))

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
	  trans-pairs   (lazy-cat (filter #(> (val %) trans-threshold)
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
  [benefit-source benefit-sink source-observation sink-observation trans-threshold]
  (let [location-map     (add-flows benefit-sink
				    (make-location-map benefit-source
						       source-observation
						       sink-observation))]
    location-map))

(defn simulate-service-flows-old
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which all update properties of the locations.  When the
   simulation completes, the network of locations is returned."
  [benefit-source benefit-sink source-observation sink-observation trans-threshold]
  (let [location-map     (add-flows benefit-sink
				    (make-location-map benefit-source
						       source-observation
						       sink-observation))
	src-locations    (filter #(> (:source %) 0.0) (vals location-map))
	num-locations    (count src-locations)
	completedThreads (atom 0)]
    (doseq [loc src-locations]
	(.start (Thread. (fn []
;			   (println "Starting thread" (Thread/currentThread))
			   (propagate-carrier-tailrec location-map
						      loc
						      (make-service-carrier (:source loc) [loc])
						      trans-threshold)
;			   (println "Stopping thread" (Thread/currentThread))
			   (swap! completedThreads inc)))))
    (while (< @completedThreads num-locations)
	   (Thread/sleep 500))
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
