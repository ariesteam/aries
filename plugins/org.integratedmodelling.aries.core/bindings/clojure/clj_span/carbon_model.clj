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
;;; This namespace defines the carbon model.
;;;
;;; Description:
;;;
;;;   Let the study area be partitioned into $N$ regions.
;;;   Those regions which sequester CO_2 shall be designated Sources.
;;;   Those which emit CO_2 by natural ecological processes shall be designated Sinks.
;;;   Those which emit CO_2 by anthropogenic processes shall be designated rival Uses.
;;;
;;;   The flow of service (i.e. CO_2 emissions absorption through
;;;   sequestration) from Source to Use locations occurs through
;;;   atmospheric mixing, which we model by connecting all Source,
;;;   Sink, and Use points to one another equally.
;;;
;;;   Let the superscripts t,p,a,i,b denote the Theoretical, Possible,
;;;   Actual, Inaccessible, and Blocked variants of the Source, Sink,
;;;   and Use values in this model.
;;;
;;;   Let $S^\tau_i$, $K^\tau_i$, and $U^\tau_i$ denote the Source,
;;;   Sink, and Use values of location $i$ respectively, where $\tau
;;;   \in {t,p,a,i,b}$.
;;;
;;;   Let $S^\tau = \sum_{i=1}^N S^\tau_i$
;;;
;;;   Let $K^\tau = \sum_{i=1}^N K^\tau_i$
;;;
;;;   Let $U^\tau = \sum_{i=1}^N U^\tau_i$
;;;
;;;   Then the study area shall be designated "Carbon Neutral" if:
;;;
;;;     $S^\tau \ge K^\tau + U^\tau$
;;;
;;;
;;;   ... write more here ...
;;;
;;; * Divides all CO2 sequestration among users by relative
;;;   consumption (i.e. Emissions)
;;;
;;; * If there is more production (total source) than consumption
;;;   (total sink), then the excess is not assigned to any user.  As a
;;;   result, inaccessible-source (theoretical - possible) will show
;;;   the excess CO2 sequestration/storage values.
;;;
;;;
;;; New Approach (MCMC)
;;;
;;;   Because the total source value is dependent on a particular
;;;   state of the source-values sequence, these must be computed
;;;   together.  This same requirement holds for both sinks and uses.
;;;   Unfortunately, this means that we are dealing with a VERY large
;;;   joint probability distribution between the states of the
;;;   source-values, sink-values, and use-values.  The only
;;;   computationally tractable approach here is to generate a sample
;;;   of all possible combinations of the values from these sequences
;;;   and then to compute the service flows using these sampled
;;;   deterministic values.  We cannot hold all of these in memory,
;;;   and computing all of them is likely to take a VERY long time
;;;   with potentially little marginal payoff as the number of samples
;;;   analyzed grows large.  Therefore, we are going to draw sample
;;;   worlds from these source, sink, and use distributions, calculate
;;;   the flow distributions (in terms of service carrier weights),
;;;   and terminate the sampling procedure when the results converge
;;;   within some epsilon of one another.

(ns clj-span.carbon-model
  (:use [clj-misc.utils      :only (p sum def- with-progress-bar-cool)]
        [clj-misc.randvars   :only (_0_ *_ _d draw make-randvar)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.matrix-ops :only (filter-matrix-for-coords make-matrix coord-map2matrix get-rows get-cols)]))

(def- *num-world-samples* 10)
(def- *sample-prob*       (/ 1.0 *num-world-samples*))

(defn- get-carrier-cache
  [source-percents actual-sinks-by-source possible-use actual-use use-percent]
  (doall
   (map (fn [source-percent actual-sinks-for-this-source]
          [(* source-percent possible-use) ;; possible-weight
           (* source-percent actual-use)   ;; actual-weight
           (doall (map #(* use-percent %) actual-sinks-for-this-source))]) ;; sink-effects
        source-percents
        actual-sinks-by-source)))

(defn- split-source
  [source-values sink-capacities total-source total-sink]
  (if (or (zero? total-source)
          (zero? total-sink))
    [source-values (take (count sink-capacities) (repeat 0.0))]
    (let [source-sink-ratio (/ total-source total-sink)
          sink-source-ratio (/ source-sink-ratio)

          source-multiplier (- 1.0 (min 1.0 sink-source-ratio))
          sink-multiplier          (min 1.0 source-sink-ratio)

          source-left       (map #(* % source-multiplier) source-values)
          sink-gained       (map #(* % sink-multiplier)   sink-capacities)]
      [source-left sink-gained])))

(defn- sample-world
  [source-dists sink-dists use-dists]
  ;; Draw a sample world.
  (println "Drawing a sample world...")
  (let [source-samples  (map draw source-dists)
        sink-samples    (map draw sink-dists)
        use-samples     (map draw use-dists)

        ;; Perform a lot of arithmetic.
        total-source    (sum source-samples)
        total-sink      (sum sink-samples)
        total-use       (sum use-samples)

        [source-unsunk actual-sinks]  (split-source source-samples sink-samples total-source        total-sink)
        [_             possible-uses] (split-source source-samples use-samples  total-source        total-use)
        [_             actual-uses]   (split-source source-unsunk  use-samples  (sum source-unsunk) total-use)

        source-percents (map #(/ % total-source) source-samples)
        use-percents    (map #(/ % total-use)    use-samples)

        actual-sinks-by-source (for [s source-percents]
                                 (for [a actual-sinks]
                                   (* a s)))]

    ;; Construct the service carrier lists for each use location and return them.
    (println "Computing" (count use-dists) "carrier lists...")
    (with-progress-bar-cool
      :keep
      (count use-dists)
      (pmap (p get-carrier-cache source-percents actual-sinks-by-source)
            possible-uses
            actual-uses
            use-percents))))

(defn- draw-sample-worlds
  "Collect some samples of the world defined by the source, sink, and
   use distributions.  Each sample is represented as a sequence of
   carrier-caches (one per use point).  To save time and memory, these
   caches do not contain complete carriers but rather triplets
   [possible-weight actual-weight sink-effects-seq] (one per source
   point)."
  [source-layer sink-layer use-layer source-points sink-points use-points ha-per-cell]
  (println "Setting up the world distributions...")
  ;; Extract the above-threshold values from the layers and scale by their cell sizes.
  (let [source-dists (map #(*_ ha-per-cell (get-in source-layer %)) source-points) ;; t/yr
        sink-dists   (map #(*_ ha-per-cell (get-in sink-layer   %)) sink-points)   ;; t/yr
        use-dists    (map #(*_ ha-per-cell (get-in use-layer    %)) use-points)]   ;; t/yr
    (for [_ (range *num-world-samples*)]
      (sample-world source-dists sink-dists use-dists))))

(defn- combine-worlds
  "Each sample world is represented as a sequence of
   carrier-caches (one per use point).  To save time and memory, these
   caches do not contain complete carriers but rather triplets
   [possible-weight actual-weight sink-effects-seq] (one per source
   point).  This function collapses two such sample worlds into one,
   by generating a probability distribution for the triplet values
   associated with each source point."
  [world1 world2]
  (println "Next merge...")
  (doall
   (pmap (fn [carrier-cache1 carrier-cache2]
           ;; Per use point.  Generate a combined carrier-cache.
           (doall
            (map (fn [[possible-weight1 actual-weight1 sink-effects-seq1]
                      [possible-weight2 actual-weight2 sink-effects-seq2]]
                   ;; Per source point.  Generate a combined triplet.
                   [(merge-with + possible-weight1 {possible-weight2 *sample-prob*})
                    (merge-with + actual-weight1   {actual-weight2   *sample-prob*})
                    (doall (map #(merge-with + %1 %2)
                                sink-effects-seq1
                                (map #(array-map % *sample-prob*) sink-effects-seq2)))])
                 carrier-cache1
                 carrier-cache2)))
         world1
         world2)))

(defn- combine-sample-worlds
  "Now we compress these samples into a single sequence of
   carrier-caches which summarize the sample results using discrete
   probability distributions."
  [source-points sink-points use-points world-samples]
  (println "Combining sample worlds...")
  (reduce combine-worlds
          (for [_ use-points]
            (for [_ source-points]
              [(make-randvar :discrete 0 ())
               (make-randvar :discrete 0 ())
               (take (count sink-points)
                     (repeat (make-randvar :discrete 0 ())))]))
          world-samples))

(defn- cacheify-world
  "Convert a sample world representation into a sequence of
   service-carrier structs by use-point."
  [ha-per-cell source-points sink-points use-points world-sample]
  (println "Packing the results into proper service-carrier structs...")
  (zipmap use-points
          (pmap (fn [carrier-cache]
                  (doall
                   (map (fn [source-id [possible-weight actual-weight sink-effects-seq]]
                          (struct-map service-carrier
                            :source-id       source-id
                            :route           nil
                            :possible-weight (_d possible-weight ha-per-cell) ;; t/ha*yr
                            :actual-weight   (_d actual-weight   ha-per-cell) ;; t/ha*yr
                            :sink-effects    (zipmap sink-points ;; t/ha*yr
                                                     (map #(_d % ha-per-cell)
                                                          sink-effects-seq))))
                        source-points
                        carrier-cache)))
                world-sample)))

;; FIXME: This algorithm eats up too much memory (related to storing
;; the sink-effects-seq, I believe).  Do something more intelligent.
(defmethod distribute-flow "CO2Removed"
  [_ _ cell-width cell-height source-layer sink-layer use-layer _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative use
   values after being initially reduced by the sink values due to
   landscape emissions."

  (println "\nRunning Carbon flow model.")

  (let [rows          (get-rows source-layer)
        cols          (get-cols source-layer)
        source-points (filter-matrix-for-coords (p not= _0_) source-layer)
        sink-points   (filter-matrix-for-coords (p not= _0_) sink-layer)
        use-points    (filter-matrix-for-coords (p not= _0_) use-layer)]

    (println "Source points:" (count source-points))
    (println "Sink points:  " (count sink-points))
    (println "Use points:   " (count use-points))

    (if (and (seq source-points) (seq use-points))

      (let [ha-per-cell (* cell-width cell-height (Math/pow 10.0 -4.0))
            use-caches  (cacheify-world ha-per-cell
                                        source-points
                                        sink-points
                                        use-points
                                        (combine-sample-worlds source-points
                                                               sink-points
                                                               use-points
                                                               (draw-sample-worlds source-layer
                                                                                   sink-layer
                                                                                   use-layer
                                                                                   source-points
                                                                                   sink-points
                                                                                   use-points
                                                                                   ha-per-cell)))]

        ;; Pack the results into a 2D matrix for analysis and resampling.
        (println "Simulation complete. Returning the cache-layer.")
        [(coord-map2matrix rows cols nil use-caches)
         (make-matrix rows cols (constantly _0_))
         (make-matrix rows cols (constantly _0_))])

      ;; Either source or use points are lacking, so no service is provided.
      ;; Note that in this case, the final results will show up like so:
      ;;
      ;; Theoretical Source = Inaccessible Source
      ;; Theoretical Sink = Inaccessible Sink
      ;; Theoretical Use = Inaccessible Use
      ;;
      ;; All of the remaining maps will be _0_ everywhere:
      ;;
      ;; Possible/Actual/Blocked Source
      ;; Actual Sink
      ;; Possible/Actual/Blocked Use
      ;; Possible/Actual/Blocked Flow
      (do
        (println "Either Source or Use is zero everywhere. Therefore no service flow will occur.")
        [(make-matrix rows cols (constantly nil))
         (make-matrix rows cols (constantly _0_))
         (make-matrix rows cols (constantly _0_))]))))
