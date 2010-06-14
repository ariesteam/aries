;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-span.
;;;
;;; clj-span is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-span is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-span.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines functions for analyzing the location
;;; sequence returned by clj-span.core/simulate-service-flows.  Each
;;; public function may be applied independently of the others and
;;; will generate a map of {[i j] -> value} pairs.

(ns clj-span.analyzer
  (:use [clj-misc.utils      :only (seq2map memoize-by-first-arg)]
	[clj-misc.matrix-ops :only (matrix2seq)]
	[clj-misc.randvars   :only (rv-mean
				    rv-zero
				    rv-pos
				    scalar-rv-subtract
				    rv-add
				    rv-subtract
				    rv-multiply
				    rv-divide)]
	[clj-span.model-api  :only (decay undecay)]
	[clj-span.params     :only (*trans-threshold*
				    *sink-type*
				    *use-type*
				    *benefit-type*)]))

(defn source-loc? [location] (not= rv-zero (:source location)))
(defn sink-loc?   [location] (not= rv-zero (:sink   location)))
(defn use-loc?    [location] (not= rv-zero (:use    location)))

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
  [locations flow-model]
  (apply merge-with rv-add
	 (for [location (filter use-loc? locations) carrier @(:carrier-cache location)]
	   (let [weight (:weight carrier)]
	     (zipmap (map :id (rseq (:route carrier)))
		     (cons weight (map #(undecay flow-model weight %) (iterate inc 1))))))))

(defn- possible-local-inflow
  "Returns the total asset amount flowing into the location.  We
   compute the local-inflow by simply summing the weights of all
   carriers in this location's carrier-cache.  Inflow is only mappable
   for sink and use locations."
  [location]
  (reduce rv-add rv-zero (map :weight @(:carrier-cache location))))
(def possible-local-inflow (memoize possible-local-inflow))

(defn- possible-inflow
  "Returns a map of {location-id -> asset-encountered}.
   We compute the inflow distribution by assigning to each location
   the sum of the carrier weights in its carrier-cache.  Inflow is
   only mappable for sink and use locations."
  [locations]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (possible-local-inflow %))))

(defn- possible-local-sink
  "We compute the amount of the asset flow sunk as follows:
   1) If sink-type = :relative
        possible-sink = sink * possible-inflow
   2) If sink-type = :absolute
        possible-sink = min(sink, possible-inflow)"
  [location]
  (if (= *sink-type* :relative)
    (rv-multiply (:sink location) (possible-local-inflow location))
    (min-key rv-mean (:sink location) (possible-local-inflow location))))
(def possible-local-sink (memoize possible-local-sink))

(defn possible-sink
  "Returns a map of {location-id -> asset-sunk}.
   We compute the amount of the asset flow sunk by each location in
   the network by assigning to each one the following value:
   1) If sink-type = :relative
        possible-sink = sink * possible-inflow
   2) If sink-type = :absolute
        possible-sink = min(sink, possible-inflow)"
  [locations]
  (seq2map (filter sink-loc? locations)
	   #(vector (:id %) (possible-local-sink %))))

(defn- possible-local-use
  "We compute the amount of the asset flow used as follows:
   1) If use-type = :relative
        possible-use = use * (possible-inflow - possible-sink)
   2) If use-type = :absolute
        possible-use = min(use, (possible-inflow - possible-sink))"
  [location]
  (let [inflow-remaining (rv-pos
			  (rv-subtract (possible-local-inflow location)
				       (possible-local-sink location)))]
    (if (= *use-type* :relative)
      (rv-multiply (:use location) inflow-remaining)
      (min-key rv-mean (:use location) inflow-remaining))))
(def possible-local-use (memoize possible-local-use))

(defn possible-use
  "Returns a map of {location-id -> asset-used}.
   We compute the amount of the asset flow used by each location in
   the network by assigning to each one the following value:
   1) If use-type = :relative
        possible-use = use * (possible-inflow - possible-sink)
   2) If use-type = :absolute
        possible-use = min(use, (possible-inflow - possible-sink))"
  [locations]
  (seq2map (filter use-loc? locations)
	   #(vector (:id %) (possible-local-use %))))

(defn- possible-local-outflow
  "Returns the total asset amount flowing out of the location.
   We compute the local-outflow by subtracting the sink and
   use (if :benefit-type = :rival) values from the inflow.  Outflow is
   only mappable for sink and use locations."
  [location]
  (let [inflow-remaining (rv-pos
			  (rv-subtract (possible-local-inflow location)
				       (possible-local-sink location)))]
    (if (= *benefit-type* :rival)
      (rv-pos (rv-subtract inflow-remaining (possible-local-use location)))
      inflow-remaining)))

(defn- possible-outflow
  "Returns a map of {location-id -> asset-uncaptured}.
   We compute the outflow distribution by assigning to each location
   its inflow minus the sink and use (if :benefit-type = :rival)
   values.  Outflow is only mappable for sink and use locations."
  [locations]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (possible-local-outflow %))))

(defn inaccessible-source
  "Returns a map of {location-id -> inaccessible-source}.
   Inaccessible-source is the amount of the theoretical-source which
   cannot be used by any location either due to propagation decay,
   lack of use capacity, or lack of flow pathways to use locations."
  [locations]
  (merge-with (fn [t p] (rv-pos (rv-subtract t p)))
	      (theoretical-source locations)
	      (possible-source locations)))

(defn inaccessible-sink
  "Returns a map of {location-id -> inaccessible-sink}.
   Inaccessible-sink is the amount of the theoretical-sink which
   cannot be utilized by each location either due to propagation decay
   of the asset or lack of flow pathways to sink locations."
  [locations]
  (merge-with (fn [t p] (rv-pos (rv-subtract t p)))
	      (theoretical-sink locations)
	      (possible-sink locations)))

(defn inaccessible-use
  "Returns a map of {location-id -> inaccessible-use}.
   Inaccessible-use is the amount of the theoretical-use which cannot
   be utilized by each location either due to propagation decay of the
   asset or lack of flow pathways to use locations."
  [locations]
  (merge-with (fn [t p] (rv-pos (rv-subtract t p)))
	      (theoretical-use locations)
	      (possible-use locations)))

(declare actual-local-inflow actual-local-outflow)

(defn- apply-local-effects
  [location weight flow-model]
  (if (sink-loc? location)
    (if (and (use-loc? location) (= *benefit-type* :rival))
      (if (or (= *sink-type* :absolute) (= *use-type* :absolute))
	(rv-multiply (actual-local-outflow location flow-model)
		     (rv-divide weight (actual-local-inflow location flow-model)))
	(-> weight
	    (rv-multiply (scalar-rv-subtract 1.0 (:sink location)))
	    (rv-multiply (scalar-rv-subtract 1.0 (:use location)))))
      (if (= *sink-type* :absolute)
	(rv-multiply (actual-local-outflow location flow-model)
		     (rv-divide weight (actual-local-inflow location flow-model)))
	(rv-multiply weight (scalar-rv-subtract 1.0 (:sink location)))))
    (if (and (use-loc? location) (= *benefit-type* :rival))
      (if (= *use-type* :absolute)
	(rv-multiply (actual-local-outflow location flow-model)
		     (rv-divide weight (actual-local-inflow location flow-model)))
	(rv-multiply weight (scalar-rv-subtract 1.0 (:use location))))
      weight)))

(defn rerun-actual-route
  "Reruns a carrier's route and returns a vector of the weights along
   it with the effects of sinks and rival uses accounted for."
  [{:keys [weight route]} flow-model]
  (let [steps (dec (count route))]
    (if (zero? steps)
      [weight]
      (loop [step                   1
	     current-loc            (second route)
	     route-remaining        (drop 2 route)
	     prev-inflows           [(undecay flow-model weight steps)]
	     prev-undecayed-outflow (apply-local-effects (first route) (first prev-inflows) flow-model)
	     prev-decayed-outflow   prev-undecayed-outflow]
	(if (<= (rv-mean prev-decayed-outflow) *trans-threshold*)
	  prev-inflows
	  (let [current-inflow (decay flow-model prev-undecayed-outflow step)]
	    (if (== step steps)
	      (conj prev-inflows current-inflow)
	      (let [current-undecayed-outflow (apply-local-effects current-loc prev-undecayed-outflow flow-model)]
		(recur (inc step)
		       (first route-remaining)
		       (rest route-remaining)
		       (conj prev-inflows current-inflow)
		       current-undecayed-outflow
		       (decay flow-model current-undecayed-outflow step))))))))))
(def rerun-actual-route (memoize-by-first-arg rerun-actual-route))

(defn actual-flow
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
  [locations flow-model]
  (apply merge-with rv-add
	 (for [location (filter use-loc? locations) carrier @(:carrier-cache location)]
	   (zipmap (map :id (:route carrier))
		   (rerun-actual-route carrier flow-model)))))

(defn actual-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to any other location in the network by assigning to each one the
   sum of all carrier weights in all the locations' carrier-caches
   whose routes begin at the source location.

   FIXME: This function double-counts in situations of rival use.  It
          also reports inflow attributed to each source point, not the
          actual use, which may be less."
  [locations flow-model]
  (apply merge-with rv-add
	 (let [get-source-id (comp :id first :route)]
	   (for [location (filter use-loc? locations) carrier @(:carrier-cache location)]
	     {(get-source-id carrier) (peek (rerun-actual-route carrier flow-model))}))))

(defn- actual-local-inflow
  "Returns the total asset amount flowing into the location.  We
   compute the local-inflow by simply summing the weights of all
   carriers in this location's carrier-cache.  Inflow is only mappable
   for sink and use locations."
  [location flow-model]
  (reduce rv-add rv-zero (map #(peek (rerun-actual-route % flow-model)) @(:carrier-cache location))))
(def actual-local-inflow (memoize-by-first-arg actual-local-inflow))

(defn- actual-inflow
  "Returns a map of {location-id -> asset-encountered}.
   We compute the inflow distribution by assigning to each location
   the sum of the carrier weights in its carrier-cache.  Inflow is
   only mappable for sink and use locations."
  [locations flow-model]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (actual-local-inflow % flow-model))))

(defn- actual-local-sink
  "We compute the amount of the asset flow sunk as follows:
   1) If sink-type = :relative
        actual-sink = sink * actual-inflow
   2) If sink-type = :absolute
        actual-sink = min(sink, actual-inflow)"
  [location flow-model]
  (if (= *sink-type* :relative)
    (rv-multiply (:sink location) (actual-local-inflow location flow-model))
    (min-key rv-mean (:sink location) (actual-local-inflow location flow-model))))
(def actual-local-sink (memoize-by-first-arg actual-local-sink))

(defn actual-sink
  "Returns a map of {location-id -> asset-sunk}.
   We compute the amount of the asset flow sunk by each location in
   the network by assigning to each one the following value:
   1) If sink-type = :relative
        actual-sink = sink * actual-inflow
   2) If sink-type = :absolute
        actual-sink = min(sink, actual-inflow)"
  [locations flow-model]
  (seq2map (filter sink-loc? locations)
	   #(vector (:id %) (actual-local-sink % flow-model))))

(defn- actual-local-use
  "We compute the amount of the asset flow used as follows:
   1) If use-type = :relative
        actual-use = use * (actual-inflow - actual-sink)
   2) If use-type = :absolute
        actual-use = min(use, (actual-inflow - actual-sink))"
  [location flow-model]
  (let [inflow-remaining (rv-pos
			  (rv-subtract (actual-local-inflow location flow-model)
				       (actual-local-sink location flow-model)))]
    (if (= *use-type* :relative)
      (rv-multiply (:use location) inflow-remaining)
      (min-key rv-mean (:use location) inflow-remaining))))
(def actual-local-use (memoize-by-first-arg actual-local-use))

(defn actual-use
  "Returns a map of {location-id -> asset-used}.
   We compute the amount of the asset flow used by each location in
   the network by assigning to each one the following value:
   1) If use-type = :relative
        actual-use = use * (actual-inflow - actual-sink)
   2) If use-type = :absolute
        actual-use = min(use, (actual-inflow - actual-sink))"
  [locations flow-model]
  (seq2map (filter use-loc? locations)
	   #(vector (:id %) (actual-local-use % flow-model))))

(defn- actual-local-outflow
  "Returns the total asset amount flowing out of the location.  We
   compute the local-outflow by subtracting the sink and
   use (if :benefit-type = :rival) values from the inflow.  Outflow is
   only mappable for sink and use locations."
  [location flow-model]
  (let [inflow-remaining (rv-pos
			  (rv-subtract (actual-local-inflow location flow-model)
				       (actual-local-sink location flow-model)))]
    (if (= *benefit-type* :rival)
      (rv-pos (rv-subtract inflow-remaining (actual-local-use location flow-model)))
      inflow-remaining)))
(def actual-local-outflow (memoize-by-first-arg actual-local-outflow))

(defn- actual-outflow
  "Returns a map of {location-id -> asset-uncaptured}.
   We compute the outflow distribution by assigning to each location
   its inflow minus the sink and use (if :benefit-type = :rival)
   values.  Outflow is only mappable for sink and use locations."
  [locations flow-model]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (actual-local-outflow % flow-model))))

(defn blocked-flow
  "Returns a map of {location-id -> blocked-flow}.
   Blocked-flow is the amount of the possible-flow which cannot be
   realized due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-flow locations flow-model)
	      (actual-flow locations flow-model)))

(defn blocked-source
  "Returns a map of {location-id -> blocked-source}.
   Blocked-source is the amount of the possible-source which cannot be
   used by any location due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-source locations)
	      (actual-source locations flow-model)))

(defn- blocked-inflow
  "Returns a map of {location-id -> blocked-inflow}.
   Blocked-inflow is the amount of the possible-inflow which cannot be
   realized due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-inflow locations)
	      (actual-inflow locations flow-model)))

(defn blocked-sink
  "Returns a map of {location-id -> blocked-sink}.
   Blocked-sink is the amount of the possible-sink which cannot be
   realized due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-sink locations)
	      (actual-sink locations flow-model)))

(defn blocked-use
  "Returns a map of {location-id -> blocked-use}.
   Blocked-use is the amount of the possible-use which cannot be
   realized due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-use locations)
	      (actual-use locations flow-model)))

(defn- blocked-outflow
  "Returns a map of {location-id -> blocked-outflow}.
   Blocked-outflow is the amount of the possible-outflow which cannot be
   realized due to upstream sinks or uses."
  [locations flow-model]
  (merge-with (fn [p a] (rv-pos (rv-subtract p a)))
	      (possible-outflow locations)
	      (actual-outflow locations flow-model)))
