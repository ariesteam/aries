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
  (:use [misc.utils  :only (seq2map
			    seq2redundant-map
			    add-anyway)]
	[gssm.params :only (*source-threshold*
			    *sink-threshold*
			    *use-threshold*
			    *decay-rate*
			    *trans-threshold*
			    *sink-type*
			    *use-type*
			    *benefit-type*)]))

(defn source-loc?
  [loc]
  (> (force (:source loc)) *source-threshold*))
(def source-loc? (memoize source-loc?))

(defn sink-loc?
  [loc]
  (> (force (:sink loc)) *sink-threshold*))
(def sink-loc? (memoize sink-loc?))

(defn use-loc?
  [loc]
  (> (force (:use loc)) *use-threshold*))
(def use-loc? (memoize use-loc?))

(defn theoretical-source
  "Returns a map of {location-id -> source-value}.
   Here, source-value is the result of the source BN."
  [locations]
  (seq2map (filter source-loc? locations)
	   #(vector (:id %) (force (:source %)))))

(defn theoretical-sink
  "Returns a map of {location-id -> sink-value}.
   If sink-type is absolute, the sink-value will simply be the result
   of the sink BN.  If it is relative, the sink-value will be the sink
   BN result * the total theoretical source-value."
  [locations]
  (seq2map (filter sink-loc? locations)
	   (if (= *sink-type* :absolute)
	     #(vector (:id %) (force (:sink %)))
	     (let [total-theoretical-source (reduce + (map (comp force :source) (filter source-loc? locations)))]
	       #(vector (:id %) (* (force (:sink %)) total-theoretical-source))))))

(defn theoretical-use
  "Returns a map of {location-id -> use-value}.
   If use-type is absolute, the use-value will simply be the result of
   the use BN.  If it is relative, the use-value will be the use BN
   result * the total theoretical source-value."
  [locations]
  (seq2map (filter use-loc? locations)
	   (if (= *use-type* :absolute)
	     #(vector (:id %) (force (:use %)))
	     (let [total-theoretical-source (reduce + (map (comp force :source) (filter source-loc? locations)))]
	       #(vector (:id %) (* (force (:use %)) total-theoretical-source))))))

(defn- possible-local-flow
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
  [location decay-factors]
  (apply merge-with +
	 (for [carrier @(:carrier-cache location)]
	   (let [weight (:weight carrier)]
	     (zipmap (map :id (rseq (:route carrier)))
		     (map #(* weight %) decay-factors))))))

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
  [locations]
  (let [decay-factors (iterate #(/ % *decay-rate*) 1.0)]
    (apply merge-with +
	   (for [location (filter use-loc? locations)]
	     (possible-local-flow location decay-factors)))))

(defn- possible-local-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to this one by assigning to each one the sum of all undecayed
   carrier weights in this location's carrier-cache whose routes begin
   at the source location."
  [location]
  (seq2redundant-map @(:carrier-cache location)
		     (fn [carrier]
		       [((comp :id first :route) carrier)
			(double (:weight carrier))])
		     add-anyway))

(defn possible-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to any other location in the network by assigning to each one the
   sum of all carrier weights in all the locations' carrier-caches
   whose routes begin at the source location.

   FIXME: This function double-counts in situations of rival use.  It
          also reports inflow attributed to each source point, not the
          actual use, which may be less."
  [locations]
  (apply merge-with +
	 (for [location (filter use-loc? locations)]
	   (possible-local-source location))))

(comment

  (defn- cumulative-provision
    [weight route]
    (val (reduce (fn [[step total] location]
		   [(inc step)
		    (if (use-loc? location)
		      (+ total (* weight (Math/pow *decay-rate* step)))
		      total)])
		 [0 0.0]
		 route)))

  (defn possible-source
    [carriers]
    (seq2redundant-map carriers
		       (fn [{:keys [weight route]}]
			 [(:id (first route))
			  (cumulative-provision weight route)])
		       add-anyway))
)

(defn- possible-local-inflow
  "Returns the total asset amount flowing into the location.  We
   compute the local-inflow by simply summing the weights of all
   carriers in this location's carrier-cache.  Inflow is only mappable
   for sink and use locations."
  [location]
  (double (reduce + (map :weight @(:carrier-cache location)))))
(def possible-local-inflow (memoize possible-local-inflow))

(defn possible-inflow
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
    (*   (force (:sink location)) (possible-local-inflow location))
    (min (force (:sink location)) (possible-local-inflow location))))
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
  (if (= *use-type* :relative)
    (*   (force (:use location)) (- (possible-local-inflow location)
				    (possible-local-sink location)))
    (min (force (:use location)) (- (possible-local-inflow location)
				    (possible-local-sink location)))))
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
  (- (possible-local-inflow location)
     (possible-local-sink location)
     (if (= *benefit-type* :rival)
       (possible-local-use location)
       0.0)))

(defn possible-outflow
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
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-source locations)
	      (possible-source locations)))

(defn inaccessible-sink
  "Returns a map of {location-id -> inaccessible-sink}.
   Inaccessible-sink is the amount of the theoretical-sink which
   cannot be utilized by each location either due to propagation decay
   of the asset or lack of flow pathways to sink locations."
  [locations]
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-sink locations)
	      (possible-sink locations)))

(defn inaccessible-use
  "Returns a map of {location-id -> inaccessible-use}.
   Inaccessible-use is the amount of the theoretical-use which cannot
   be utilized by each location either due to propagation decay of the
   asset or lack of flow pathways to use locations."
  [locations]
  (merge-with (fn [t p] (max 0.0 (- t p)))
	      (theoretical-use locations)
	      (possible-use locations)))

(declare actual-local-inflow actual-local-outflow)

(defn rerun-actual-route
  "Reruns a carrier's route and returns a vector of the weights along
   it with the effects of sinks and rival uses accounted for."
  [carrier]
  (loop [path           (:route carrier)
	 acc            []
	 carrier-weight (/ (:weight carrier)
			   (Math/pow *decay-rate* (dec (count path))))]
    (if (empty? (rest path))
      (conj acc carrier-weight)
      (if (== carrier-weight 0.0)
	(conj acc 0.0)
	(recur (rest path)
	       (conj acc carrier-weight)
	       (let [loc (first path)
		     amount-propagated (if (and (not (sink-loc? loc))
						(or (= *benefit-type* :non-rival)
						    (not (use-loc? loc))))
					 (* carrier-weight *decay-rate*)
					 (if (or (and (= *sink-type* :absolute) (sink-loc? loc))
						 (and (= *use-type* :absolute) (= *benefit-type* :rival) (use-loc? loc)))
					   (let [total-encountered    (actual-local-inflow loc)
						 contributing-percent (/ carrier-weight total-encountered)]
					     (* (actual-local-outflow loc) contributing-percent *decay-rate*))
					   (* carrier-weight
					      (- 1.0 (force (:sink loc)))
					      (if (= *benefit-type* :rival) ; use-type must be :relative
						(- 1.0 (force (:use loc)))
						1.0)
					      *decay-rate*)))]
		 (comment
		   amount-propagated (if (> (force (:sink loc)) 0.0)
				       (if (= sink-type :absolute)
					 XXX OPTION ABS ; carriers have been sorted and its dependencies have been cached
					 (if (> (force (:use loc)) 0.0)
					   (if (= benefit-type :rival)
					     (if (= use-type :absolute)
					       XXX OPTION ABS
					       XXX OPTION REL - sink and use)
					     XXX OPTION REL - sink only)
					   XXX OPTION REL - sink only))
				       (if (> (force (:use loc)) 0.0)
					 (if (= benefit-type :rival)
					   (if (= use-type :absolute)
					     XXX OPTION ABS
					     XXX OPTION REL - use only)
					   XXX OPTION SIMPLE)
					 XXX OPTION SIMPLE))
		   )
		 (if (< amount-propagated *trans-threshold*)
		   0.0
		   amount-propagated)))))))
(def rerun-actual-route (memoize rerun-actual-route))

(defn- actual-local-flow
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
  [location]
  (apply merge-with +
	 (for [carrier @(:carrier-cache location)]
	   (zipmap (map :id (:route carrier))
		   (rerun-actual-route carrier)))))

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
  [locations]
  (apply merge-with +
	 (for [location (filter use-loc? locations)]
	   (actual-local-flow location))))

(defn- actual-local-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to this one by assigning to each one the sum of all undecayed
   carrier weights in this location's carrier-cache whose routes begin
   at the source location."
  [location]
  (seq2redundant-map @(:carrier-cache location)
		     (fn [carrier]
		       [((comp :id first :route) carrier)
			(double (peek (rerun-actual-route carrier)))])
		     add-anyway))

(defn actual-source
  "Returns a map of {location-id -> asset-provided}.
   We compute the amount of the asset-provided by each source location
   to any other location in the network by assigning to each one the
   sum of all carrier weights in all the locations' carrier-caches
   whose routes begin at the source location.

   FIXME: This function double-counts in situations of rival use.  It
          also reports inflow attributed to each source point, not the
          actual use, which may be less."
  [locations]
  (apply merge-with +
	 (for [location (filter use-loc? locations)]
	   (actual-local-source location))))

(defn- actual-local-inflow
  "Returns the total asset amount flowing into the location.  We
   compute the local-inflow by simply summing the weights of all
   carriers in this location's carrier-cache.  Inflow is only mappable
   for sink and use locations."
  [location]
  (double
   (reduce +
	   (for [c @(:carrier-cache location)]
	     (peek (rerun-actual-route c))))))
(def actual-local-inflow (memoize actual-local-inflow))

(defn actual-inflow
  "Returns a map of {location-id -> asset-encountered}.
   We compute the inflow distribution by assigning to each location
   the sum of the carrier weights in its carrier-cache.  Inflow is
   only mappable for sink and use locations."
  [locations]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (actual-local-inflow %))))

(defn- actual-local-sink
  "We compute the amount of the asset flow sunk as follows:
   1) If sink-type = :relative
        actual-sink = sink * actual-inflow
   2) If sink-type = :absolute
        actual-sink = min(sink, actual-inflow)"
  [location]
  (if (= *sink-type* :relative)
    (*   (force (:sink location)) (actual-local-inflow location))
    (min (force (:sink location)) (actual-local-inflow location))))
(def actual-local-sink (memoize actual-local-sink))

(defn actual-sink
  "Returns a map of {location-id -> asset-sunk}.
   We compute the amount of the asset flow sunk by each location in
   the network by assigning to each one the following value:
   1) If sink-type = :relative
        actual-sink = sink * actual-inflow
   2) If sink-type = :absolute
        actual-sink = min(sink, actual-inflow)"
  [locations]
  (seq2map (filter sink-loc? locations)
	   #(vector (:id %) (actual-local-sink %))))

(defn- actual-local-use
  "We compute the amount of the asset flow used as follows:
   1) If use-type = :relative
        actual-use = use * (actual-inflow - actual-sink)
   2) If use-type = :absolute
        actual-use = min(use, (actual-inflow - actual-sink))"
  [location]
  (if (= *use-type* :relative)
    (*   (force (:use location)) (- (actual-local-inflow location)
				    (actual-local-sink location)))
    (min (force (:use location)) (- (actual-local-inflow location)
				    (actual-local-sink location)))))
(def actual-local-use (memoize actual-local-use))

(defn actual-use
  "Returns a map of {location-id -> asset-used}.
   We compute the amount of the asset flow used by each location in
   the network by assigning to each one the following value:
   1) If use-type = :relative
        actual-use = use * (actual-inflow - actual-sink)
   2) If use-type = :absolute
        actual-use = min(use, (actual-inflow - actual-sink))"
  [locations]
  (seq2map (filter use-loc? locations)
	   #(vector (:id %) (actual-local-use %))))

(defn- actual-local-outflow
  "Returns the total asset amount flowing out of the location.  We
   compute the local-outflow by subtracting the sink and
   use (if :benefit-type = :rival) values from the inflow.  Outflow is
   only mappable for sink and use locations."
  [location]
  (- (actual-local-inflow location)
     (actual-local-sink location)
     (if (= *benefit-type* :rival)
       (actual-local-use location)
       0.0)))
(def actual-local-outflow (memoize actual-local-outflow))

(defn actual-outflow
  "Returns a map of {location-id -> asset-uncaptured}.
   We compute the outflow distribution by assigning to each location
   its inflow minus the sink and use (if :benefit-type = :rival)
   values.  Outflow is only mappable for sink and use locations."
  [locations]
  (seq2map (filter #(or (sink-loc? %) (use-loc? %)) locations)
	   #(vector (:id %) (actual-local-outflow %))))

(defn blocked-flow
  "Returns a map of {location-id -> blocked-flow}.
   Blocked-flow is the amount of the possible-flow which cannot be
   realized due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-flow locations)
	      (actual-flow locations)))

(defn blocked-source
  "Returns a map of {location-id -> blocked-source}.
   Blocked-source is the amount of the possible-source which cannot be
   used by any location due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-source locations)
	      (actual-source locations)))

(defn blocked-inflow
  "Returns a map of {location-id -> blocked-inflow}.
   Blocked-inflow is the amount of the possible-inflow which cannot be
   realized due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-inflow locations)
	      (actual-inflow locations)))

(defn blocked-sink
  "Returns a map of {location-id -> blocked-sink}.
   Blocked-sink is the amount of the possible-sink which cannot be
   realized due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-sink locations)
	      (actual-sink locations)))

(defn blocked-use
  "Returns a map of {location-id -> blocked-use}.
   Blocked-use is the amount of the possible-use which cannot be
   realized due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-use locations)
	      (actual-use locations)))

(defn blocked-outflow
  "Returns a map of {location-id -> blocked-outflow}.
   Blocked-outflow is the amount of the possible-outflow which cannot be
   realized due to upstream sinks or uses."
  [locations]
  (merge-with (fn [p a] (max 0.0 (- p a)))
	      (possible-outflow locations)
	      (actual-outflow locations)))
