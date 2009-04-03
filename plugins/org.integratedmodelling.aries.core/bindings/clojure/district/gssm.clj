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
		       (let [feature-idx (+ (* i cols) j)
			     source-features (extract-features source-states feature-idx)]
			 (struct-map location
			   :id              [i j]
			   :neighbors       (get-neighbors [i j] rows cols)
			   :source-features source-features
			   :sink-features   (extract-features sink-states feature-idx)
			   :features        (extract-features-undiscretized
					     (merge source-states sink-states)
					     feature-idx)
			   :sunk            (ref 0.0)
			   :used            (ref 0.0)
			   :consumed        (ref 0.0)
			   :carrier-bin     (ref ())
			   :source          (delay (source-val benefit-source-name
							       source-inference-engine
							       source-features)))))
		     (fn [loc] [(:id loc) loc]))]
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
		     (delay (compute-flows benefit-sink-name
					   sink-inference-engine
					   (:sink-features %))))
	     location-map)))

(defstruct service-carrier :weight :route)

(defn update-local-accumulators!
  [location carrier]
  (let [weight  (:weight carrier)
	flows   (force (:flows location))
	sink    (:sink flows)
	use     (:use flows)
	consume (:consume flows)]
    (dosync
     (if (> sink 0.0)
       (commute (:sunk        location) + (* weight sink)))
     (when (> (+ use consume) 0.0)
       (commute (:used        location) + (* weight use))
       (commute (:consumed    location) + (* weight consume))
       (commute (:carrier-bin location) conj carrier)))))

(defn distribute-raycast!
  [location-seq trans-threshold]
  (loop [location-seq location-seq
	 carrier (struct service-carrier
			 (force (:source (first location-seq)))
			 [(first location-seq)])]
    (when (seq location-seq)
      (let [loc    (first location-seq)
	    weight (:weight carrier)
	    flows  (force (:flows loc))]
	(update-local-accumulators! loc carrier)
	(let [outweight (* weight (:out flows))]
	  (if (> outweight trans-threshold)
	    (recur (rest location-seq)
		   (struct service-carrier
			   outweight
			   (conj (:route carrier) (second location-seq))))))))))

(defn find-viewpaths
  "Returns a sequence of paths (one for each combination of p and b),
   where a path is represented by the sequence of all points [i j]
   intersected by the line from p to b for each p,b pair.  Since this
   is calculated over a regular integer-indexed grid, diagonal lines
   will be approximated by lines bending at right angles along the
   p-to-b line.  This calculation imagines the indeces of each point
   to be located at the center of a square of side length 1.  Note
   that the first point in each path will be its p, and the last will
   be its b.  If p=b, the path will contain only this one point."
  [providers beneficiaries]
  (for [p providers b beneficiaries]
    (let [[pi pj] (:id p)
	  [bi bj] (:id b)
	  m (if (not= pj bj) (/ (- bi pi) (- bj pj)))
	  b (if m (- pi (* m pj)))
	  f (fn [x] (+ (* m x) b))]
      (if m
	(let [j-range (if (< pj bj) (range pj (inc bj)) (range pj (dec bj) -1))]
	  (if (== m 0)
	    (for [j j-range] [pi j])
	    (concat		  
	     (for [j j-range]
	       (let [left-i  (Math/round (f (- j 1/2)))
		     right-i (Math/round (f (+ j 1/2)))
		     i-range (cond (and (< pi bi) (< pj bj)) (range left-i  (inc right-i))
				   (and (< pi bi) (> pj bj)) (range right-i (inc left-i))
				   (and (> pi bi) (< pj bj)) (range left-i  (dec right-i) -1)
				   (and (> pi bi) (> pj bj)) (range right-i (dec left-i)  -1))]
		 (for [i i-range] [i j]))))))
	(let [i-range (if (< pi bi) (range pi (inc bi)) (range pi (dec bi) -1))]
	  (for [i i-range] [i pj]))))))

(defn expand-box
  "Returns a new list of points which completely bounds the
   rectangular region defined by points and remains within the bounds
   [0-rows],[0-cols]."
  [points rows cols]
  (when (seq points)
    (let [row-coords (map #(get % 0) points)
	  col-coords (map #(get % 1) points)
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
       (when (and (>= left 0)     (<  top rows)) (list [left  top]))
       (when (and (>= left 0)     (>= bottom 0)) (list [left  bottom]))
       (when (and (<  right cols) (<  top rows)) (list [right top]))
       (when (and (<  right cols) (>= bottom 0)) (list [right bottom]))))))

(defn manhattan-distance
  [[i1 j1] [i2 j2]]
  (+ (Math/abs (- i1 i2)) (Math/abs (- j1 j2))))

(defn nearest-neighbor
  [loc-id frontier]
  (let [neighbor-distances (map (fn [[l c :as f]] [f (manhattan-distance loc-id (:id l))]) frontier)
	one-step-away  (first (some (fn [[f dist]] (and (== dist 1) f)) neighbor-distances))
	two-steps-away (first (some (fn [[f dist]] (and (== dist 2) f)) neighbor-distances))]
    (or one-step-away two-steps-away)))

(defn distribute-gaussian!
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [location-map location root-carrier decay-rate trans-threshold rows cols]
  (loop [frontier {location root-carrier}]
    (when (seq frontier)
      (doseq [[loc carrier] frontier] (update-local-accumulators! loc carrier))
      (recur
       (let [new-frontier-locs (map location-map
				    (expand-box (map (comp :id first) frontier)
						rows cols))]
	 (filter #(and (val %) (> (:weight (val %)) trans-threshold))
		 (seq2map new-frontier-locs
			  (fn [new-frontier-loc]
			    [new-frontier-loc
			     (let [[l c] (nearest-neighbor
					  (:id new-frontier-loc)
					  frontier)]
			       (if (and l c)
				 (struct service-carrier
					 (* (:weight c) (:out (force (:flows l))) decay-rate)
					 (conj (:route c) new-frontier-loc))))]))))))))

(defn memoize-by-first-arg
  [function]
  (let [cache (atom {})]
    (fn [& args]
      (or (@cache (first args))
          (let [result (apply function args)]
	    (swap! cache assoc (first args) result)
            result)))))

(def get-outflow-distribution
  (memoize-by-first-arg (fn [location location-map]
    (let [local-elev ((:features location) "Elevation")
	  neighbors (map location-map (:neighbors location))
	  neighbor-elevs (map #((:features %) "Elevation") neighbors)
	  min-elev (apply min (cons local-elev neighbor-elevs))
	  num-downhill-paths (count (filter #(== min-elev %) neighbor-elevs))]
      (if (> num-downhill-paths 0)
	(let [path-weight (/ (:out (force (:flows location))) num-downhill-paths)]
	  (filter #(> (val %) 0.0)
		  (zipmap neighbors
			  (map #(if (== min-elev %) path-weight 0.0) neighbor-elevs)))))))))

(defn distribute-downhill!
  "A service carrier distributes its weight between being sunk or
   consumed by the location or flowing on to its neighbors based on
   location-specific flow probabilities.  It then stores itself in the
   location's carrier-bin and propagates service carriers to every
   neighbor where (> (* weight trans-prob) trans-threshold)."
  [location-map location root-carrier trans-threshold]
  (loop [loc location
	 carrier root-carrier
	 open-list ()]
    (let [weight    (:weight carrier)
	  flows     (force (:flows loc))
	  out-pairs (seq2map (get-outflow-distribution loc location-map)
			     (fn [[neighbor trans-prob]]
			       [neighbor (struct service-carrier
						 (* weight trans-prob)
						 (conj (:route carrier) neighbor))]))
	  new-open-list (lazy-cat (filter (fn [loc carrier] (> (:weight carrier) trans-threshold))
					  out-pairs)
				  open-list)]
      (update-local-accumulators! loc carrier)
      (if (empty? out-pairs)
	(dosync (commute (:sunk loc) + (* weight (:out flows)))))
      (when (seq new-open-list)
	(let [[next-loc carrier] (first new-open-list)]
	  (recur next-loc
		 carrier
		 (rest new-open-list)))))))

(defn distribute-load-over-processors
  [action-fn arg-seq]
  (let [num-processors (.availableProcessors (Runtime/getRuntime))
	agents (map agent (replicate (* 2 num-processors) nil))]
    (map #(send %1 action-fn %2) (cycle agents) arg-seq)
    (apply await agents)))

(defmulti
  #^{:doc "Service-specific flow distribution function."}
  distribute-flow!
  (fn [benefit-source-name location-map trans-threshold rows cols] benefit-source-name))

(defmethod distribute-flow! :default
  [benefit-source-name _ _ _ _]
  (throw (Exception. (str "Service " benefit-source-name " is unrecognized."))))

(defmethod distribute-flow! "SensoryEnjoyment"
  [_ location-map trans-threshold _ _]
  (let [locations     (vals location-map)
	providers     (filter #(> (force (:source %)) 0.0) locations)
	beneficiaries (filter #(> (:use (force (:flows %))) 0.0) locations)]
    (distribute-load-over-processors
     (fn [_ path] (distribute-raycast! (map location-map path) trans-threshold))
     (find-viewpaths providers beneficiaries))))

(defmethod distribute-flow! "ProximityToBeauty"
  [_ location-map trans-threshold rows cols]
  (let [decay-rate 0.75]  ; yes, I hard-coded this. It must be used with trans-threshold 9.0.
                          ; This makes Source:Low=3 steps(1/4mi), Moderate=6 steps(1/2mi), High=8 steps(2/3mi)
    (distribute-load-over-processors
     (fn [_ loc]
       (distribute-gaussian! location-map
			     loc
			     (struct service-carrier (force (:source loc)) [loc])
			     decay-rate
			     trans-threshold
			     rows
			     cols))
     (filter #(> (force (:source %)) 0.0) (vals location-map)))))

(defmethod distribute-flow! "ClimateStability"
  [_ location-map _ _ _]
  (let [locations (vals location-map)
	total-sequestration (reduce + (map #(force (:source %)) locations))
	use-dist (map #(:consume (force (:flows %))) locations)
	total-use (let [use (reduce + use-dist)] (if (== use 0.0) 1.0 use))
	fractional-use-dist (map #(/ % total-use) use-dist)]
    (dosync
     (map (fn [loc fractional-use]
	    (commute (:consumed loc) + (* fractional-use total-sequestration)))
	  locations fractional-use-dist))))

(defmethod distribute-flow! "FloodPrevention"
  [_ location-map trans-threshold _ _]
  (distribute-load-over-processors
   (fn [_ loc]
     (distribute-downhill! location-map
			   loc
			   (struct service-carrier (force (:source loc)) [loc])
			   trans-threshold))
   (filter #(> (force (:source %)) 0.0) (vals location-map))))

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
    (distribute-flow! (.getLocalName benefit-source)
		      location-map
		      trans-threshold
		      (geospace/grid-rows source-observation)
		      (geospace/grid-columns source-observation))
    location-map))

(defn add-anyway
  "Sums the non-nil argument values."
  [x y]
  (cond (nil? x) y
	(nil? y) x
	:otherwise (+ x y)))

(defn find-provisionshed
  "Returns a map of {provider-id -> benefit-provided}.

   Explanation:

   A location's carrier-bin contains the sequence of all carriers
   which transport benefit to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from provider to beneficiary) and Weight (amount of benefit
   that reaches the beneficiary).  We compute the provisionshed of a
   beneficiary location by assigning to each provider location the sum
   of all carrier weights (in that beneficiary's carrier-bin) whose
   routes begin at the provider location."
  [beneficiary-location]
  (let [flows (force (:flows beneficiary-location))
	absorption (+ (:use flows) (:consume flows))]
    (seq2redundant-map @(:carrier-bin beneficiary-location)
		       (fn [carrier] [((comp :id first :route) carrier)
				      (* absorption (:weight carrier))])
		       add-anyway)))

(defn find-benefitshed
  "Returns a map of {beneficiary-id -> benefit-received}.

   Explanation:

   A location's carrier-bin contains the sequence of all carriers
   which transport benefit to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from provider to beneficiary) and Weight (amount of benefit
   that reaches the beneficiary).  We compute the benefitshed of a
   provider location by assigning to each beneficiary location the sum
   of all carrier weights (in that beneficiary's carrier-bin) whose
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
					  @(:carrier-bin location)))))]))))

(defn find-critical-regions
  "Returns a map of {location-id -> criticality}.

   Explanation:

   A location's carrier-bin contains the sequence of all carriers
   which transport benefit to this location.  Each carrier is
   represented as a pair of Route (sequence of location ids in its
   path from provider to beneficiary) and Weight (amount of benefit
   that reaches the beneficiary).  We compute the criticality of each
   location as a three-step process:

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
   amount of service throughput that depends on that location."
  [all-locations]
  (apply merge-with +
	 (for [location all-locations]
	   (apply merge-with +
		  (for [carrier @(:carrier-bin location)]
		    (let [weight (:weight carrier)]
		      (seq2map (:route carrier)
			       (fn [route-step] [(:id route-step) weight]))))))))


(comment
  FIXME
  distribute-gaussian! uses a hard-coded decay-rate and requires a correspondingly specific trans-threshold
  distribute-raycast! should use a decay-rate for visual distance fade-out
  distribute-raycast! needs to remove repeated subpaths
  delay sunk/used/consumed calculations (which are O(n)) until property viewing time. not during simulation!
)
