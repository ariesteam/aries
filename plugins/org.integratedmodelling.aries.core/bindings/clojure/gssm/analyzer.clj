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

(defn theoretical-source
  "Returns a map of {location-id -> source-value}.
   Here, source-value is the result of the source BN."
  [locations]
  (seq2map locations #(vector (:id %) (force (:source %)))))

(defn theoretical-sink
  "Returns a map of {location-id -> sink-value}.
   If sink-type is absolute, the sink-value will simply be the result
   of the sink BN.  If it is relative, the sink-value will be the sink
   BN result * the total theoretical source-value."
  [locations {sink-type :sink-type}]
  (seq2map locations #(vector (:id %) (force (:sink %)))))
;;	   (if (= sink-type :absolute)
;;	     #(vector (:id %) (force (:sink %)))))
;;	     (let [theoretical-source (reduce + (map (comp force :source) locations))]
;;	       #(vector (:id %) (* (force (:sink %)) theoretical-source))))))

(defn theoretical-use
  "Returns a map of {location-id -> use-value}.
   If use-type is absolute, the use-value will simply be the result of
   the use BN.  If it is relative, the use-value will be the use BN
   result * the total theoretical source-value."
  [locations {use-type :use-type}]
  (seq2map locations #(vector (:id %) (force (:use %)))))
;;	   (if (= use-type :absolute)
;;           #(vector (:id %) (force (:use %)))))
;;	     (let [theoretical-source (reduce + (map (comp force :source) locations))]
;;	       #(vector (:id %) (* (force (:use %)) theoretical-source))))))

(defn- possible-local-flows
  "Returns a map of {location-id -> flow-density}.
   We compute the flow-density of each location contributing to this
   one as a two-step process:
   1) Transform each carrier in the location's carrier-cache into a
      map of {route-loc1 weight/decay-rate^(N-1), route-loc2
      weight/decay-rate^(N-2), ... route-loc(N-1) weight/decay-rate,
      route-locN weight}
   2) Merge all the maps s.t. each location now contains one map with
      the union of all keys, whose values are the sums of the submap
      values.
   This final map now contains an entry for every location, which is
   part of any carrier's route, whose value represents the total
   amount of asset flow through that location."
  [location decay-rate]
  (apply merge-with +
	 (for [carrier @(:carrier-cache location)]
	   (let [weight (:weight carrier)
		 route  (:route  carrier)]
	     (zipmap (map :id (reverse route))
		     (map #(/ weight (Math/pow decay-rate %))
			  (range (count route))))))))

(defn possible-flow
  "Returns a map of {location-id -> flow-density}.
   We compute the flow-density of each location in the network as a
   three-step process:
   1) Transform each carrier in the location's carrier-cache into a
      map of {route-loc1 weight/decay-rate^(N-1), route-loc2
      weight/decay-rate^(N-2), ... route-loc(N-1) weight/decay-rate,
      route-locN weight}
   2) Merge all the maps in each location s.t. each location now
      contains one map with the union of all keys, whose values are
      the sums of the submap values.
   3) Merge all the maps across the locations s.t. the final map
      contains the union of all keys, whose values are the sums of the
      submap values.
   This final map now contains an entry for every location, which is
   part of any carrier's route, whose value represents the total
   amount of service flow through that location."
  [locations {decay-rate :decay-rate}]
  (apply merge-with +
	 (for [location (filter #(> (force (:use %)) 0.0) locations)]
	   (possible-local-flows location decay-rate))))

(defn- possible-local-sources
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to this one by assigning to each one the sum of all undecayed
   carrier weights in this location's carrier-cache whose routes begin
   at the source location."
  [location]
  (seq2redundant-map @(:carrier-cache location)
		     (fn [carrier]
		       [((comp :id first :route) carrier)
			(:weight carrier)])
		     add-anyway))

(defn possible-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to any other location in the network by assigning to each one the
   sum of all carrier weights in all the locations' carrier-caches
   whose routes begin at the source location."
  [locations]
  (apply merge-with +
	 (for [location (filter #(> (force (:use %)) 0.0) locations)]
	   (possible-local-sources location))))

(defn- possible-local-inflow
  "Returns the total asset amount flowing into the location.
   We compute the local-inflow by simply summing the weights of all
   carriers in this location's carrier-cache.  Inflow is only mappable
   for sink and use locations."
  [location]
  (reduce + (map :weight @(:carrier-cache location))))

(defn possible-inflow
  "Returns a map of {location-id -> asset-encountered}.
   We compute the inflow distribution by assigning to each location
   the sum of the carrier weights in its carrier-cache.  Inflow is
   only mappable for sink and use locations."
  [locations]
  (seq2map (filter #(> (+ (force (:sink %)) (force (:use %))) 0.0) locations)
	   #(vector (:id %) (possible-local-inflow %))))

(defn- possible-local-sink
  "We compute the amount of the asset flow sunk as follows:
   1) If sink-type = :relative
        possible-sink = sink * possible-inflow
   2) If sink-type = :absolute
        possible-sink = min(sink, possible-inflow)"
  [location sink-type]
  (if (= sink-type :relative)
    (*   (force (:sink location)) (possible-local-inflow location))
    (min (force (:sink location)) (possible-local-inflow location))))

(defn possible-sink
  "Returns a map of {location-id -> asset-sunk}.
   We compute the amount of the asset flow sunk by each location in
   the network by assigning to each one the following value:
   1) If sink-type = :relative
        possible-sink = sink * possible-inflow
   2) If sink-type = :absolute
        possible-sink = min(sink, possible-inflow)"
  [locations {sink-type :sink-type}]
  (seq2map (filter #(> (force (:sink %)) 0.0) locations)
	   #(vector (:id %) (possible-local-sink % sink-type))))

(defn- possible-local-use
  "We compute the amount of the asset flow used as follows:
   1) If use-type = :relative
        possible-use = use * (possible-inflow - possible-sink)
   2) If use-type = :absolute
        possible-use = min(use, (possible-inflow - possible-sink))"
  [location use-type sink-type]
  (if (= use-type :relative)
    (*   (force (:use location)) (- (possible-local-inflow location)
				    (possible-local-sink location sink-type)))
    (min (force (:use location)) (- (possible-local-inflow location)
				    (possible-local-sink location sink-type)))))

(defn possible-use
  "Returns a map of {location-id -> asset-used}.
   We compute the amount of the asset flow used by each location in
   the network by assigning to each one the following value:
   1) If use-type = :relative
        possible-use = use * (possible-inflow - possible-sink)
   2) If use-type = :absolute
        possible-use = min(use, (possible-inflow - possible-sink))"
  [locations {use-type :use-type sink-type :sink-type}]
  (seq2map (filter #(> (force (:use %)) 0.0) locations)
	   #(vector (:id %) (possible-local-use % use-type sink-type))))

(defn- possible-local-outflow
  "Returns the total asset amount flowing out of the location.
   We compute the local-outflow by subtracting the sink and
   use (if :benefit-type = :rival) values from the inflow.  Outflow is
   only mappable for sink and use locations."
  [location sink-type use-type]
  (- (possible-local-inflow location)
     (possible-local-sink location sink-type)
     (possible-local-use location use-type sink-type)))

(defn possible-outflow
  "Returns a map of {location-id -> asset-uncaptured}.
   We compute the outflow distribution by assigning to each location
   its inflow minus the sink and use (if :benefit-type = :rival)
   values.  Outflow is only mappable for sink and use locations."
  [locations {use-type :use-type sink-type :sink-type}]
  (seq2map (filter #(> (+ (force (:sink %)) (force (:use %))) 0.0) locations)
	   #(vector (:id %) (possible-local-outflow % sink-type use-type))))

(defn inaccessible-source
  "Returns a map of {location-id -> inaccessible-source}.
   Inaccessible-source is the amount of the theoretical-source which
   cannot be used by any location either due to propagation decay,
   lack of use capacity, or lack of flow pathways to use locations."
  [locations]
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-source locations)
	      (possible-source locations)))

(defn inaccessible-sink
  "Returns a map of {location-id -> inaccessible-sink}.
   Inaccessible-sink is the amount of the theoretical-sink which
   cannot be utilized by each location either due to propagation decay
   of the asset or lack of flow pathways to sink locations."
  [locations flow-params]
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-sink locations flow-params)
	      (possible-sink locations flow-params)))

(defn inaccessible-use
  "Returns a map of {location-id -> inaccessible-use}.
   Inaccessible-use is the amount of the theoretical-use which cannot
   be utilized by each location either due to propagation decay of the
   asset or lack of flow pathways to use locations."
  [locations flow-params]
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-use locations flow-params)
	      (possible-use locations flow-params)))

(defn carriers-encountered
  [locations]
  (seq2map locations #(vector (:id %) (double (count @(:carrier-cache %))))))

(declare
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
	"View Possible Flow"       #(possible-flow            locations flow-params)
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

(defn find-potential-flow
  "Make a list of all use and target locations.
   For each one:
     If an use or target location is in the flowshed of a location,
     remove that use or target from the list.  Continue until list
     is empty.  Merge all flowsheds created in this way wth +."
  [locations flow-params]
  {}
)

(defn find-sinkshed
  "Foreach carrier route in an use or target carrier-cache
     walk from head to tail along the route
       each time a sink or use is encountered
         calculate effective weight at that location as a function of
         tail distance and upstream depletion-amount
           calculate sunk weight as (* (/ effective-weight assets-received) local-sink-amount)
           calculate used weight as (* (/ effective-weight assets-received) local-use-amount)
      store sink and use effects in a map for each such location
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
  [provider-location locations]
  (seq2map locations
	   (fn [location]
	     (let [flows (force (:flows location))
		   absorption (+ (:use flows) (:consume flows))]
	       [(:id location)
		(* absorption
		   (reduce + (map :weight
				  (filter #(= provider-location
					      ((comp first :route) %))
					  @(:carrier-cache location)))))]))))


)
