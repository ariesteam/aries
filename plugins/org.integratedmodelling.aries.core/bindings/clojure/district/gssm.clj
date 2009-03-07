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
  :id :neighbors :source-features :sink-features :features
  :sunk :used :consumed :carrier-bin :source :flows)

(defn make-location
  "Location constructor"
  [id neighbors source-features sink-features features benefit-source-name source-inference-engine]
  (struct-map location
    :id              id
    :neighbors       neighbors
    :source-features source-features
    :sink-features   sink-features
    :features        features
    :sunk            (ref 0.0)
    :used            (ref 0.0)
    :consumed        (ref 0.0)
    :carrier-bin     (ref ())
    :source          (source-val benefit-source-name
				 source-inference-engine
				 source-features)))

(defn discretize-value
  "Transforms the value to the string name of its corresponding
   discretized value, using the particular concept's transformation
   procedure in the global discretization-table."
  [concept-name value]
  (let [transformer (discretization-table concept-name)]
    (or (transformer value) (transformer 0.0))))

(defn extract-features
  "Returns a map of feature names to the observed values at i j."
  [observation-states idx]
  (seq2map (seq observation-states)
	   (fn [[feature-name values]]
	     [feature-name (discretize-value feature-name (nth values idx))])))

(defn extract-features-undiscretized
  "Returns a map of feature names to the observed values at i j."
  [observation-states idx]
  (maphash identity #(nth % idx) observation-states))

(defmulti
  #^{:doc "Returns a map of ids to location objects, one per location in observation."}
  make-location-map (fn [benefit-source source-observation sink-observation]
		      (and (geospace/grid-extent? source-observation)
			   (geospace/grid-extent? sink-observation))))

(defn count-distinct-states
  [observation-states]
  (maphash identity
	   (fn [vals]
	     (let [distinct-vals (distinct vals)
		   num-distinct (count distinct-vals)]
	       (if (<= num-distinct 10)
		 (for [val distinct-vals]
		   [val (count (filter #(= % val) vals))])
		 (str num-distinct " distinct values..."))))
	   observation-states))

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
      (let [location-map
	    (seq2map (for [i (range rows) j (range cols)]
		       (let [feature-idx (+ (* i cols) j)]
			 (make-location [i j]
					(get-neighbors [i j] rows cols)
					(extract-features source-states feature-idx)
					(extract-features sink-states feature-idx)
					(extract-features-undiscretized
					 (merge source-states sink-states)
					 feature-idx)
					benefit-source-name
					source-inference-engine)))
		     (fn [location] [(:id location) location]))]
	(println "Rows: " rows)
	(println "Cols: " cols)
	(println "Benefit-Source-Name: " benefit-source-name)
	(println "Source-States: " (count-distinct-states source-states))
	(println "Sink-States: " (count-distinct-states sink-states))
	location-map))))

(defmethod make-location-map false
  [benefit-source source-observation sink-observation]
  {})

(defn add-flows
  "Updates location-map such that each location's flows field will
   contain a delayed evaluation of its carrier flow probabilities."
  [benefit-sink location-map]
  (let [benefit-sink-name     (.getLocalName benefit-sink)
	sink-inference-engine (aries/make-bn-inference benefit-sink)]
    (maphash identity
	     #(assoc % :flows
		     (compute-flows benefit-sink-name
				    sink-inference-engine
				    (:sink-features %)))
	     location-map)))

(defstruct service-carrier :weight :route)

(defn make-service-carrier
  "Service carrier constructor"
  [weight route]
  (struct-map service-carrier :weight weight :route route))

(defn memoize-by-first-arg
  [function]
  (let [cache (ref {})]
    (fn [& args]
      (or (@cache (first args))
          (let [result (apply function args)]
            (dosync
             (commute cache assoc (first args) result))
            result)))))

(def get-outflow-distribution
  (memoize-by-first-arg (fn [location location-map]
    (let [local-elev ((:features location) "Elevation")
	  neighbors (map location-map (:neighbors location))
	  neighbor-elevs (map #((:features %) "Elevation") neighbors)
	  min-elev (min local-elev (min neighbor-elevs))
	  num-downhill-paths (count (filter #(== min-elev %) neighbor-elevs))
	  path-weight (if (> num-downhill-paths 0) (/ (:out (force (:flows location)))
						      num-downhill-paths) 0.0)]
      (filter #(> (val %) 0.0)
	      (zipmap neighbors
		      (map #(if (== min-elev %) path-weight 0.0) neighbor-elevs)))))))

(defn propagate-carrier!
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [location-map location root-carrier trans-threshold]
  (loop [loc location
	 carrier root-carrier
	 open-list ()]
    (let [weight      (:weight carrier)
	  flows       (force (:flows loc))
	  out-pairs   (seq2map (get-outflow-distribution loc location-map)
			       (fn [[neighbor trans-prob]]
				 [neighbor (make-service-carrier (* weight trans-prob)
								 (conj (:route carrier) neighbor))]))
	  trans-pairs (lazy-cat (filter (fn [loc carrier]
					  (> (:weight carrier) trans-threshold)) out-pairs)
				open-list)]
      (dosync
       (commute (:sunk        loc) +    (* weight
					   (if (empty? out-pairs)
					     (+ (:sink flows) (:out flows))
					     (:sink flows))))
       (commute (:used        loc) +    (* weight (:use flows)))
       (commute (:consumed    loc) +    (* weight (:consume flows)))
       (commute (:carrier-bin loc) conj carrier))
      (when (seq trans-pairs)
	(let [[next-loc carrier] (first trans-pairs)]
	  (recur next-loc
		 carrier
		 (rest trans-pairs)))))))

(defmulti
  #^{:doc "Service-specific flow distribution function."}
  distribute-flow!
  (fn [benefit-source-name location-map trans-threshold] benefit-source-name))

(defmethod distribute-flow! :default
  [benefit-source-name _ _]
  (throw (Exception. (str "Service " benefit-source-name " is unrecognized."))))

(defmethod distribute-flow! "SensoryEnjoyment"
  [_ location-map trans-threshold]
;  [benefit-sink-name sink-features neighbor-sink-features flow-amount]
;  (let [num-neighbors (count neighbor-sink-features)
;	amt (/ flow-amount num-neighbors)]
;    (replicate num-neighbors amt)))
  location-map)

(defmethod distribute-flow! "ProximityToBeauty"
  [_ location-map trans-threshold]
;  [benefit-sink-name features neighbor-features flow-amount]
;  (let [elevs (map :elevation neighbor-features)
;	min-elev (let [e (min elevs)] (if (<= e (:elevation features)) e 0))
;	num-paths (count (filter #(== min-elev %) elevs))
;	path-weight (if (> num-paths 0) (/ 0.8 num-paths) 0.0)]
;    (map #(if (== min-elev %) path-weight 0.0) elevs)))
  location-map)

(defmethod distribute-flow! "ClimateStability"
  [_ location-map trans-threshold]
;  [benefit-sink-name features neighbor-features flow-amount]
;  (let [num-neighbors (count neighbor-features)
;	outval (/ 0.8 num-neighbors)]
;    (replicate num-neighbors outval)))
  location-map)

(defmethod distribute-flow! "FloodPrevention"
  [_ location-map trans-threshold]
  (let [src-locations    (filter #(> (force (:source %)) 0.0) (vals location-map))
	num-locations    (count src-locations)
	completedThreads (atom 0)]
    (doseq [loc src-locations]
	(.start (Thread. (fn []
			   (propagate-carrier! location-map
					       loc
					       (make-service-carrier (force (:source loc)) [loc])
					       trans-threshold)
			   (swap! completedThreads inc)))))
    (while (< @completedThreads num-locations)
	   (Thread/sleep 500))
    location-map))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which all update properties of the locations.  When the
   simulation completes, the network of locations is returned."
  [benefit-source benefit-sink source-observation sink-observation trans-threshold]
  (let [location-map (add-flows benefit-sink
				(make-location-map benefit-source
						   source-observation
						   sink-observation))]
    (distribute-flow! (.getLocalName benefit-source) location-map trans-threshold)))

(defn add-anyway
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
