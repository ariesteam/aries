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
;;;
;;; New Approach (MCMC)
;;;
;;;   Because the total source value is dependent on a
;;;   particular state of the source-values sequence, these
;;;   must be computed together.  The same point holds for both
;;;   sinks and uses.  Unfortunately, this means that we are
;;;   dealing with a VERY large joint probability distribution
;;;   between the states of the source-values, sink-values, and
;;;   use-values.  The only correct approach is to generate all
;;;   combinations of values from these sequences and then to
;;;   compute the service flows using these deterministic
;;;   values.  We cannot hold all of these in memory, and
;;;   computing all of them is likely to take a VERY long time
;;;   with very little marginal payoff as the number of samples
;;;   analyzed grows large.  Therefore, we are going to draw
;;;   sample worlds from these source, sink, and use
;;;   distributions, calculate the flow distributions (in terms
;;;   of service carrier weights), and terminate the sampling
;;;   procedure when the results converge within some epsilon
;;;   of one another.
;;; * Routes run from Source to Use (no Sinks in this model)
;;;
;;; * Contain positive utility values only
;;;
;;; * Divides all CO2 sequestration among users by relative
;;;   consumption (i.e. Emissions)
;;;
;;; * If there is more production (total source) than consumption
;;;   (total sink), then the excess is not assigned to any user.  As a
;;;   result, inaccessible-source (theoretical - possible) will show
;;;   the excess CO2 sequestration/storage values.

(ns clj-span.carbon-model
  (:use [clj-misc.utils      :only (p sum def-)]
        [clj-misc.randvars   :only (_0_ draw make-randvar)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.matrix-ops :only (filter-matrix-for-coords make-matrix coord-map2matrix get-rows get-cols)]))

(defn- split-source
  [source-values sink-capacities total-source total-sink]
  (let [source-sink-ratio (do (println "Source/Sink Ratio") (time (/ total-source total-sink)))
        sink-source-ratio (do (println "Sink/Source Ratio") (time (/ source-sink-ratio)))

        source-multiplier (do (println "Source Multiplier") (time (- 1.0 (min 1.0 sink-source-ratio))))
        sink-multiplier   (do (println "Sink   Multiplier") (time        (min 1.0 source-sink-ratio)))

        source-left       (do (println "Source Left")       (time (doall (map #(* % source-multiplier) source-values))))
        sink-gained       (do (println "Sink Gained")       (time (doall (map #(* % sink-multiplier)   sink-capacities))))]
    [source-left sink-gained]))

(defn- sample-world
  [source-dists sink-dists use-dists get-carrier-list]
  ;; Draw a sample world.
  (let [source-samples (doall (map draw source-dists))
        sink-samples   (doall (map draw sink-dists))
        use-samples    (doall (map draw use-dists))

        ;; Perform a lot of arithmetic.
        total-source    (do (println "Total Source")    (time (sum source-samples)))
        total-sink      (do (println "Total Sink")      (time (sum sink-samples)))
        total-use       (do (println "Total Use")       (time (sum use-samples)))

        source-percents (do (println "Source Percents") (time (doall (map #(/ % total-source) source-samples))))
        use-percents    (do (println "Use    Percents") (time (doall (map #(/ % total-use)    use-samples))))

        [source-unsunk actual-sinks]  (split-source source-samples sink-samples total-source        total-sink)
        [source-unused possible-uses] (split-source source-samples use-samples  total-source        total-use)
        [source-excess actual-uses]   (split-source source-unsunk  use-samples  (sum source-unsunk) total-use)

        actual-sinks-by-source (for [s source-percents]
                                 (for [a actual-sinks]
                                   (* a s)))]

    ;; Construct the service carrier lists for each use location and return them.
    (println "Computing" (count use-dists) "carrier lists...")
    (time
     (let [carrier-lists (map (p get-carrier-list source-percents actual-sinks-by-source) possible-uses actual-uses use-percents)]
       (doseq [_ carrier-lists] (print "*") (flush))
       (println "\nAll done.")
       carrier-lists))))

(defn- combine-worlds
  "Each world is a sequence of carrier-lists (which are themselves
   sequences of maps), one per use point.  Each of these carrier-lists
   represents a distribution of triplets (possible-use, actual-use,
   sink-effects) over the source points.  This function collapses two
   such carrier-lists into one, by generating a probability
   distribution for the triplet values associated with each source
   point."
  [sample-prob world1 world2]
  (map (fn [carrier-list1 carrier-list2]
         (map (fn [[possible-weight1 actual-weight1 sink-effects1]
                   [possible-weight2 actual-weight2 sink-effects2]]
                [(merge-with + possible-weight1 {possible-weight2 sample-prob})
                 (merge-with + actual-weight1   {actual-weight2   sample-prob})
                 (map #(merge-with + %1 %2)
                      sink-effects1
                      (map #(array-map % sample-prob) sink-effects2))])
              carrier-list1
              carrier-list2))
       world1
       world2))

(def- *num-world-samples* 10)

(defmethod distribute-flow "CO2Removed"
  [_ animation? cell-width cell-height source-layer sink-layer use-layer _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative use
   values after being initially reduced by the sink values."

  (println "Running Carbon flow model.")

  (let [source-points (filter-matrix-for-coords (p not= _0_) source-layer)
        sink-points   (filter-matrix-for-coords (p not= _0_) sink-layer)
        use-points    (filter-matrix-for-coords (p not= _0_) use-layer)
        rows          (get-rows source-layer)
        cols          (get-cols source-layer)]

    (println "Source points:" (count source-points))
    (println "Sink points:  " (count sink-points))
    (println "Use points:   " (count use-points))

    (if (and (seq source-points) (seq use-points))

      ;; Extract the above-threshold values from the layers.
      (let [source-dists (map (p get-in source-layer) source-points)
            sink-dists   (map (p get-in sink-layer)   sink-points)
            use-dists    (map (p get-in use-layer)    use-points)

            get-carrier-list (fn [source-percents actual-sinks-by-source possible-use actual-use use-percent]
                               (doall (map (fn [source-id source-percent actual-sinks-for-this-source]
                                             [(* source-percent possible-use)                                 ;; possible-weight
                                              (* source-percent actual-use)                                   ;; actual-weight
                                              (doall (map #(* use-percent %) actual-sinks-for-this-source))]) ;; sink-effects
                                           source-points
                                           source-percents
                                           actual-sinks-by-source)))

            sample-prob  (/ 1.0 *num-world-samples*)

            ;; Collect some samples of the world defined by the
            ;; source, sink, and use distributions.  The result of
            ;; each sample is a sequence of carrier-lists (one per use
            ;; point).
            world-samples (pmap sample-world (repeat *num-world-samples*
                                                     [source-dists sink-dists use-dists get-carrier-list]))

            ;; Now we compress these samples into a single sequence of
            ;; carrier-lists which summarize the sample results using
            ;; discrete probability distributions.
            partial-randvar-carrier-lists (do
                                            (println "Aggregating samples...")
                                            (time
                                             (reduce (p combine-worlds sample-prob)
                                                     (repeat (count source-points) [(make-randvar :discrete 0 ())
                                                                                    (make-randvar :discrete 0 ())
                                                                                    (repeat (count sink-points)
                                                                                            (make-randvar :discrete 0 ()))])
                                                     world-samples)))

            ;; Convert our results to sequences of proper service-carrier structs.
            complete-randvar-carrier-lists (do
                                             (println "Packing the results into proper service-carrier structs.")
                                             (time
                                              (for [carrier-list partial-randvar-carrier-lists]
                                                (map (fn [source-id [possible-weight actual-weight sink-effects]]
                                                       (struct-map service-carrier
                                                         :source-id       source-id
                                                         :route           nil
                                                         :possible-weight possible-weight
                                                         :actual-weight   actual-weight
                                                         :sink-effects    (zipmap sink-points sink-effects)))
                                                     source-points
                                                     carrier-list))))]

        ;; Pack the results into a 2D matrix for analysis and resampling.
        (println "Simulation complete. Returning the cache-layer.")
        [(coord-map2matrix rows cols nil (zipmap use-points complete-randvar-carrier-lists))
         (make-matrix rows cols (constantly _0_))
         (make-matrix rows cols (constantly _0_))])

      ;; Either source or use points are lacking, so no service is provided.
      (do
        (println "Either Source or Use is zero everywhere. Therefore no service flow will occur.")
        [(make-matrix rows cols (constantly nil))
         (make-matrix rows cols (constantly _0_))
         (make-matrix rows cols (constantly _0_))]))))
