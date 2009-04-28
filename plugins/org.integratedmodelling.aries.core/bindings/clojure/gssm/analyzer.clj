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

(ns gssm.analyzer
  (:refer-clojure)
  (:use [misc.utils :only (seq2map seq2redundant-map add-anyway)]))

(declare theoretical-source
	 theoretical-sink
	 theoretical-use
	 inaccessible-source
	 inaccessible-sink
	 inaccessible-use
	 possible-flow
	 possible-source
	 possible-inflow
	 possible-sink
	 possible-use
	 possible-outflow
	 blocked-flow
	 blocked-source
	 blocked-inflow
	 blocked-sink
	 blocked-use
	 blocked-outflow
	 actual-flow
	 actual-source
	 actual-inflow
	 actual-sink
	 actual-use
	 actual-outflow)

(comment
  menu {"View Theoretical Source"  #(theoretical-source       locations)
	"View Theoretical Sink"    #(theoretical-sink         locations flow-params)
	"View Theoretical Use"     #(theoretical-use          locations flow-params)
	"View Inacessible Source"  #(inacessible-source       locations)
	"View Inacessible Sink"    #(inacessible-sink         locations flow-params)
	"View Inacessible Use"     #(inacessible-use          locations flow-params)
	"View Possible Flow"       #(possible-flow            locations)
	"View Possible Source"     #(possible-source          locations)
	"View Possible Inflow"     #(possible-inflow          locations)
	"View Possible Sink"       #(possible-sink            locations flow-params)
	"View Possible Use"        #(possible-use             locations flow-params)
	"View Possible Outflow"    #(possible-outflow         locations flow-params)
	"View Blocked Flow"        #(blocked-flow             locations)
	"View Blocked Source"      #(blocked-source           locations)
	"View Blocked Inflow"      #(blocked-inflow           locations)
	"View Blocked Sink"        #(blocked-sink             locations flow-params)
	"View Blocked Use"         #(blocked-use              locations flow-params)
	"View Blocked Outflow"     #(blocked-outflow          locations flow-params)
	"View Actual Flow"         #(actual-flow              locations)
	"View Actual Source"       #(actual-source            locations)
	"View Actual Inflow"       #(actual-inflow            locations)
	"View Actual Sink"         #(actual-sink              locations flow-params)
	"View Actual Use"          #(actual-use               locations flow-params)
	"View Actual Outflow"      #(actual-outflow           locations flow-params)}

  (defn update-caches!
    "Adds to the sink-cache and use-cache the number of units each
     takes from the carrier's weight.  If their sum is greater than 0,
     stores the carrier in this location's carrier-cache.  Returns the
     number of units removed from the carrier's weight by this
     process."
    [location carrier {:keys [sink-type use-type use-effect]}]
    (let [weight      (:weight carrier)
	  sink        (force (:sink location))
	  use         (force (:use  location))
	  sink-amount (if (= sink-type :relative)
			(* weight sink)
			(min weight (- sink @(:sink-cache location))))
	  use-amount  (if (= use-type :relative)
			(* (- weight sink-amount) use)
			(min (- weight sink-amount) (- use @(:use-cache location))))]
      (dosync
       (if (> sink-amount 0.0)
	 (commute (:sink-cache location) + sink-amount))
       (if (> use-amount 0.0)
	 (commute (:use-cache location) + use-amount))
       (if (> (+ sink-amount use-amount) 0.0)
	 (commute (:carrier-cache location) conj carrier)))
      (if (= use-effect :destructive)
	(+ sink-amount use-amount)
	sink-amount)))

(defn find-potential-source
  "Returns a map of {location-id -> source-value}.
   Here, source-value is the result of the source BN."
  [locations]
  (seq2map locations #(vector (:id %) (force (:source %)))))

(defn find-potential-sink
  "Returns a map of {location-id -> sink-value}.
   If sink-type is absolute, the sink-value will simply be the result
   of the sink BN.  If it is relative, the sink-value will be the sink
   BN result * the total potential source-value."
  [locations {sink-type :sink-type}]
  (if (= sink-type :absolute)
    (seq2map locations #(vector (:id %) (force (:sink %))))
    (let [potential-source (reduce + (map #(force (:source %)) locations))]
      (seq2map locations #(vector (:id %) (* (force (:sink %)) potential-source))))))

(defn find-potential-use
  "Returns a map of {location-id -> use-value}.
   If use-type is absolute, the use-value will simply be the result of
   the use BN.  If it is relative, the use-value will be the use BN
   result * the total potential source-value."
  [locations {use-type :use-type}]
  (if (= use-type :absolute)
    (seq2map locations #(vector (:id %) (force (:use %))))
    (let [potential-source (reduce + (map #(force (:source %)) locations))]
      (seq2map locations #(vector (:id %) (* (force (:use %)) potential-source))))))

(defn find-potential-flow
  "Make a list of all uptake and target locations.
   For each one:
     If an uptake or target location is in the flowshed of a location,
     remove that uptake or target from the list.  Continue until list
     is empty.  Merge all flowsheds created in this way wth +."
  [locations flow-params]
  {}
)

(defn find-realized-source
  "Returns a map of {source-id -> asset-provided}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the amount of the asset contributed by each source
   location to any other location in the network by assigning to each
   one the sum of all carrier weights in all the locations'
   carrier-caches whose routes begin at the source location."
  [all-locations]
  (apply merge-with +
	 (for [location all-locations]
	   (find-sourceshed location))))

(defn find-realized-sink
  "Returns a map of {sink-id -> asset-depleted}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the amount of the asset flow depleted by each sink
   location in the network by assigning to each one the following
   value:

   1) If sink-type = :relative
        realized-sink = sink * asset-received
   2) If sink-type = :absolute
        realized-sink = min(sink, asset-received)"
  [all-locations {sink-type :sink-type}]
  (seq2map (filter #(> (force (:sink %)) 0.0) all-locations)
	   #(vector (:id %) (find-local-sink-amount % sink-type))))

(defn find-realized-uptake
  "Returns a map of {uptake-id -> asset-depleted}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the amount of the asset flow depleted by each uptake
   location in the network by assigning to each one the following
   value:

   1) If uptake-type = :relative
        realized-uptake = uptake * (asset-received - realized-sink)
   2) If uptake-type = :absolute
        realized-uptake = min(uptake, (asset-received - realized-sink))"
  [all-locations {uptake-type :uptake-type sink-type :sink-type}]
  (seq2map (filter #(> (force (:uptake %)) 0.0) all-locations)
	   #(vector (:id %) (find-local-uptake-amount % uptake-type sink-type))))

(defn find-realized-flow
  "Returns a map of {location-id -> flow-density}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the flow-density of each location in the network as a
   three-step process:

   1) Transform each carrier in each location into a map of
      {route-loc1 weight, route-loc2 weight, ... route-locN weight}

   2) Merge all the maps in each location s.t. each location now
      contains one map with the union of all keys, whose values are
      the sums of the submap values.

   3) Merge all the maps across the locations s.t. the final map
      contains the union of all keys, whose values are the sums of the
      submap values.

   This final map now contains an entry for every location, which is
   part of any carrier's route, whose value represents the total
   amount of service flow through that location."
  [all-locations]
  (apply merge-with +
	 (for [location all-locations]
	   (find-flowshed location))))

(defn find-unrealized-source
  [locations]
  (merge-with (fn [p r] (max 0.0 (- p r)))
	      (find-potential-source locations)
	      (find-realized-source locations)))

(defn find-unrealized-sink
  [locations flow-params]
  (merge-with (fn [p r] (max 0.0 (- p r)))
	      (find-potential-sink locations)
	      (find-realized-sink locations flow-params)))

(defn find-unrealized-use
  [locations flow-params]
  (merge-with (fn [p r] (max 0.0 (- p r)))
	      (find-potential-use locations)
	      (find-realized-use locations flow-params)))

(defn find-unrealized-flow
  [locations flow-params]
  (merge-with (fn [p r] (max 0.0 (- p r)))
	      (find-potential-flow locations)
	      (find-realized-flow locations flow-params)))

(defn find-sourceshed
  "Returns a map of {source-id -> asset-provided}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the amount of the asset contributed by each source
   location to this one by assigning to each one the sum of all
   carrier weights in this location's carrier-cache whose routes begin
   at the source location."
  [location]
  (seq2redundant-map @(:carrier-cache location)
		     (fn [carrier] [((comp :id first :route) carrier)
				    (:weight carrier)])
		     add-anyway))

(defn find-sinkshed
  "Foreach carrier route in an uptake or target carrier-cache
     walk from head to tail along the route
       each time a sink or uptake is encountered
         calculate effective weight at that location as a function of
         tail distance and upstream depletion-amount
           calculate sunk weight as (* (/ effective-weight assets-received) local-sink-amount)
           calculate used weight as (* (/ effective-weight assets-received) local-uptake-amount)
      store sink and uptake effects in a map for each such location
   merge this depletion map w/ that for the other routes"
  [location {sink-type :sink-type}]
)

(defn find-useshed
  "Returns a map of {beneficiary-id -> benefit-received}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport benefit to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from provider to beneficiary) and Weight (amount of benefit
   that reaches the beneficiary).  We compute the benefitshed of a
   provider location by assigning to each beneficiary location the sum
   of all carrier weights (in that beneficiary's carrier-cache) whose
   routes begin at the provider location."
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
					  @(:carrier-cache location)))))]))))

(defn find-flowshed
  "Returns a map of {location-id -> flow-density}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the flow-density of each location contributing to this
   one as a two-step process:

   1) Transform each carrier in the location's carrier-cache into a
      map of {route-loc1 weight, route-loc2 weight, ... route-locN
      weight}

   2) Merge all the maps s.t. each location now contains one map with
      the union of all keys, whose values are the sums of the submap
      values.

   This final map now contains an entry for every location, which is
   part of any carrier's route, whose value represents the total
   amount of asset flow through that location."
  [location]
  (apply merge-with +
	 (for [carrier @(:carrier-cache location)]
	   (let [weight (:weight carrier)]
	     (seq2map (:route carrier)
		      (fn [route-step] [(:id route-step) weight]))))))

(defn- find-asset-received
  "Returns the total asset value received by the location.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the asset received by simply summing the weights of all
   carriers in this location's carrier-cache."
  [location]
  (reduce + (map :weight @(:carrier-cache location))))

(defn find-all-asset-received
  "Returns a map of {location-id -> asset-received}.

   Explanation:

   A location's carrier-cache contains the sequence of all carriers
   which transport assets to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from its source to this location) and Weight (amount of the
   asset that reaches this point along the specified Route).

   We compute the asset received by all locations by assigning to each
   one the sum of the carrier weights in its carrier-cache."
  [all-locations]
  (seq2map all-locations #(vector (:id %) (find-asset-received %))))

(defn- find-local-sink-amount
  [location sink-type]
  (if (= sink-type :relative)
    (*   (force (:sink location)) (find-asset-received location))
    (min (force (:sink location)) (find-asset-received location))))

(defn- find-local-uptake-amount
  [location uptake-type sink-type]
  (if (= uptake-type :relative)
    (*   (force (:uptake location)) (- (find-asset-received location)
				       (find-local-sink-amount sink-type)))
    (min (force (:uptake location)) (- (find-asset-received location)
				       (find-local-sink-amount sink-type)))))

)