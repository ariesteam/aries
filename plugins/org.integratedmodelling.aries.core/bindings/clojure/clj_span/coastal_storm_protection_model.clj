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
;;; This namespace defines the coastal-storm-protection model.
;;;
;;;  Lookup the storm name.
;;;  Create a function to determine the wave's new orientation.
;;;  Discover the storm direction.
;;;  Project a swath of carriers 100km wide perpendicular to the storm direction.
;;;  Deplete all sinks that the wave intersects.
;;;  Move the carriers together to their new positions along the wavefront and repeat the sink depletion process.
;;;  If users are encountered, store a carrier on the user and keep going (non-rival use).
;;;  If a carrier's possible-weight falls below the threshold, stop the carrier.
;;;  Exit when all carriers have finished moving.
;;;
;;; Scale Factors:
;;;
;;; If you double the resolution along both axes (i.e. 4x number of
;;; cells), this will require:
;;;
;;;   4x memory and time to create cache, possible flow, and actual flow layers
;;;   4x memory and time to find and store source and use points
;;;   4x time to compute bearing to users
;;;   2x time to compute mean-storm-bearing (for wave orientation)
;;;   2x memory and time to generate wave carriers
;;;   4x time and memory to run the wave simulation
;;;   4x memory and time to show the flow animations
;;;   4x memory and time to return result matrices
;;;
;;; Thus, this algorithm scales linearly in time and space
;;; requirements with the number of cells analyzed.

(ns clj-span.coastal-storm-protection-model
  (:use [clj-span.gui        :only (draw-ref-layer)]
        [clj-span.params     :only (*trans-threshold*)]
        [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.utils      :only (seq2map p & angular-distance magnitude)]
        [clj-misc.matrix-ops :only (get-rows
                                    get-cols
                                    make-matrix
                                    map-matrix
                                    matrix2seq
                                    add-ids
                                    subtract-ids
                                    in-bounds?
                                    on-bounds?
                                    bitpack-route
                                    rotate-2d-vec
                                    filter-matrix-for-coords
                                    get-bearing
                                    get-neighbors
                                    dist-to-steps
                                    find-point-at-dist-in-m
                                    find-line-between)]
        [clj-misc.randvars   :only (_0_ _+_ *_ _d rv-fn rv-above?)]))

(defn handle-sink-effects
  [current-id possible-weight actual-weight eco-sink-layer geo-sink-layer km2-per-cell]
  (let [eco-sink-height  (get-in eco-sink-layer current-id)
        geo-sink-height  (get-in geo-sink-layer current-id)
        eco-sink?        (not= _0_ eco-sink-height)
        geo-sink?        (not= _0_ geo-sink-height)
        eco-sink-cap     (if eco-sink? (*_ km2-per-cell eco-sink-height))
        geo-sink-cap     (if geo-sink? (*_ km2-per-cell geo-sink-height))
        post-geo-possible-weight (if geo-sink?
                                   (rv-fn (fn [p g] (- p (min p g))) possible-weight geo-sink-cap)
                                   possible-weight)
        post-geo-actual-weight   (if geo-sink?
                                   (rv-fn (fn [a g] (- a (min a g))) actual-weight geo-sink-cap)
                                   actual-weight)
        post-eco-actual-weight   (if (and eco-sink? (not= _0_ post-geo-actual-weight))
                                   (rv-fn (fn [a e] (- a (min a e))) post-geo-actual-weight eco-sink-cap)
                                   post-geo-actual-weight)
        eco-sink-effects         (if (and eco-sink? (not= _0_ post-geo-actual-weight))
                                   {current-id (rv-fn min post-geo-actual-weight eco-sink-cap)})]
    [post-geo-possible-weight post-eco-actual-weight eco-sink-effects]))

(defn handle-local-users!
  [current-location use-layer cache-layer wave-cell-depth
   {:keys [route possible-weight actual-weight] :as post-sink-carrier}]
  (let [vulnerability-unscaled (get-in use-layer current-location)]
    (if (not= _0_ vulnerability-unscaled)
      (let [vulnerability             (_d vulnerability-unscaled wave-cell-depth)
            possible-damage-inflicted (_*_ possible-weight vulnerability)
            actual-damage-inflicted   (_*_ actual-weight   vulnerability)]
        (dosync (alter (get-in cache-layer current-location) conj
                       (assoc post-sink-carrier
                         :route           (bitpack-route route)
                         :possible-weight possible-damage-inflicted
                         :actual-weight   actual-damage-inflicted)))))))

(defn apply-local-effects!
  [storm-orientation eco-sink-layer geo-sink-layer use-layer cache-layer
   possible-flow-layer actual-flow-layer km2-per-cell wave-cell-depth rows cols
   {:keys [route possible-weight actual-weight sink-effects] :as storm-carrier}]
  (let [current-location (peek route)
        next-location    (add-ids current-location storm-orientation)
        [new-possible-weight new-actual-weight new-sink-effects] (handle-sink-effects current-location
                                                                                      possible-weight
                                                                                      actual-weight
                                                                                      eco-sink-layer
                                                                                      geo-sink-layer
                                                                                      km2-per-cell)
        post-sink-carrier (assoc storm-carrier
                            :possible-weight new-possible-weight
                            :actual-weight   new-actual-weight
                            :sink-effects    (merge-with _+_ sink-effects new-sink-effects))]
    (dosync
     (alter (get-in possible-flow-layer current-location) _+_ possible-weight)
     (alter (get-in actual-flow-layer   current-location) _+_ actual-weight))
    (handle-local-users! current-location use-layer cache-layer wave-cell-depth post-sink-carrier)
    (if (and (in-bounds? rows cols next-location)
             (rv-above? new-possible-weight *trans-threshold*))
      (assoc post-sink-carrier :route (conj route next-location)))))

(def *animation-sleep-ms* 100)

;; FIXME: This is really slow. Speed it up.
(defn run-animation [panel]
  (send-off *agent* run-animation)
  (Thread/sleep *animation-sleep-ms*)
  (doto panel (.repaint)))

(defn end-animation [panel] panel)

;; FIXME: My simulation doesn't handle diagonally oriented waves
;;        properly. We need a better movement algorithm that sweeps
;;        out the cells correctly.
(defn distribute-wave-energy!
  [storm-centerpoint storm-bearing storm-carriers get-next-bearing eco-sink-layer geo-sink-layer
   use-layer cache-layer possible-flow-layer actual-flow-layer animation? km2-per-cell wave-cell-depth rows cols]
  (println "Moving the wave energy toward the coast...")
  (let [possible-flow-animator (if animation? (agent (draw-ref-layer "Possible Flow" possible-flow-layer :flow 1)))
        actual-flow-animator   (if animation? (agent (draw-ref-layer "Actual Flow"   actual-flow-layer   :flow 1)))]
    (when animation?
      (send-off possible-flow-animator run-animation)
      (send-off actual-flow-animator   run-animation))
    (doseq [_ (take-while (& seq last)
                          (iterate
                           (fn [[storm-centerpoint storm-bearing storm-carriers]]
                             (let [next-storm-centerpoint (add-ids storm-centerpoint storm-bearing)
                                   next-storm-bearing     (get-next-bearing next-storm-centerpoint storm-bearing)
                                   next-storm-carriers    (doall (remove nil? (pmap (p apply-local-effects!
                                                                                       storm-bearing
                                                                                       eco-sink-layer
                                                                                       geo-sink-layer
                                                                                       use-layer
                                                                                       cache-layer
                                                                                       possible-flow-layer
                                                                                       actual-flow-layer
                                                                                       km2-per-cell
                                                                                       wave-cell-depth
                                                                                       rows
                                                                                       cols)
                                                                                    storm-carriers)))]
                               (if (and next-storm-bearing
                                        (not (on-bounds? rows cols storm-centerpoint)))
                                 [next-storm-centerpoint next-storm-bearing next-storm-carriers]
                                 (println "Location-dependent termination:" next-storm-bearing storm-centerpoint))))
                           [storm-centerpoint storm-bearing storm-carriers]))]
      (print "*") (flush))
    (when animation?
      (send-off possible-flow-animator end-animation)
      (send-off actual-flow-animator   end-animation)))
  ;;(shutdown-agents)))
  (println "\nAll done."))

(defn get-wave-cell-depth
  [mean-storm-bearing wave-depth cell-width cell-height]
  (let [wave-cell-depth (dist-to-steps mean-storm-bearing wave-depth cell-width cell-height)]
    (println "Wave Cell Depth:" wave-cell-depth)
    wave-cell-depth))

(defn find-wave-cells
  [storm-centerpoint mean-storm-bearing wave-width cell-width cell-height rows cols]
  (print "Constructing the wave...") (flush)
  (let [wave-orientation (rotate-2d-vec mean-storm-bearing (/ Math/PI -2)) ;; rotate 90 degrees left
        wave-reach       (/ wave-width 2)
        wave-left-edge   (find-point-at-dist-in-m storm-centerpoint
                                                  wave-orientation
                                                  wave-reach
                                                  cell-width
                                                  cell-height)
        wave-right-edge  (find-point-at-dist-in-m storm-centerpoint
                                                  (map - wave-orientation)
                                                  wave-reach
                                                  cell-width
                                                  cell-height)
        wave-cells       (filter (p in-bounds? rows cols)
                                 (find-line-between wave-left-edge wave-right-edge))]
    (println "done." (str "[" (count wave-cells) " cells]"))
    wave-cells))

(defn get-bearing-seq
  [get-next-bearing source-id initial-bearing]
  (map second
       (iterate
        (fn [[id bearing]] (let [next-id (add-ids id bearing)]
                             [next-id (get-next-bearing next-id bearing)]))
        [source-id initial-bearing])))

(defn mean-bearing
  [bearings]
  (let [bearing-sum       (reduce (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)]) bearings)
        bearing-magnitude (magnitude bearing-sum)]
    (map #(/ % bearing-magnitude) bearing-sum)))

(defn determine-mean-storm-bearing
  [get-next-bearing storm-centerpoint storm-bearing rows cols]
  (print "Determining wave orientation...") (flush)
  (let [search-distance        (int (/ (magnitude [rows cols]) 10.0)) ;; 1/10 # of cells on a diag across the matrix
        storm-bearing-reversed (get-next-bearing storm-centerpoint (map - storm-bearing))
        bearings-forward       (take search-distance (get-bearing-seq get-next-bearing storm-centerpoint storm-bearing))
        bearings-behind        (take search-distance (get-bearing-seq get-next-bearing storm-centerpoint storm-bearing-reversed))
        storm-track-sample     (concat (reverse (map (p map -) bearings-behind)) bearings-forward)
        mean-storm-bearing     (mean-bearing storm-track-sample)]
    (println "done." (str "[" (count storm-track-sample) " samples]"))
    mean-storm-bearing))

;; FIXME: It would be good to have a faster way to compute
;;        this. Perhaps a sampling method?
(defn find-bearing-to-users
  [storm-centerpoint use-points]
  (print "Determining storm direction...") (flush)
  (let [bearing-to-users (mean-bearing
                          (map (p get-bearing storm-centerpoint) use-points))]
    (println "done.")
    bearing-to-users))

(defn get-next-bearing
  [on-track? rows cols curr-id prev-bearing]
  (if-let [on-track-neighbors (seq (filter on-track? (get-neighbors rows cols curr-id)))]
    (let [bearing-changes (seq2map (map #(subtract-ids % curr-id) on-track-neighbors)
                                   (fn [bearing-to-neighbor]
                                     [(angular-distance prev-bearing bearing-to-neighbor)
                                      bearing-to-neighbor]))]
      (bearing-changes (apply min (keys bearing-changes))))))

;; FIXME: Make these a function of the source value.
(def *wave-width* 100000) ;; in meters
(def *wave-depth* 5000)   ;; in meters

(defmethod distribute-flow "CoastalStormMovement"
  [_ animation? cell-width cell-height source-layer eco-sink-layer use-layer
   {storm-track-layer "StormTrack", geo-sink-layer "GeomorphicFloodProtection"}]
  (println "Running Coastal Storm Protection flow model.")
  (let [rows                (get-rows source-layer)
        cols                (get-cols source-layer)
        cache-layer         (make-matrix rows cols (fn [_] (ref ())))
        possible-flow-layer (make-matrix rows cols (fn [_] (ref _0_)))
        actual-flow-layer   (make-matrix rows cols (fn [_] (ref _0_)))
        [source-points use-points] (pmap (p filter-matrix-for-coords (p not= _0_))
                                         [source-layer use-layer])]
    (println "Source points:" (count source-points))
    (println "Use points:   " (count use-points))
    (let [storm-centerpoint  (first source-points)
          on-track?          #(not= _0_ (get-in storm-track-layer %))
          get-next-bearing   (p get-next-bearing on-track? rows cols)
          storm-bearing      (get-next-bearing storm-centerpoint (find-bearing-to-users storm-centerpoint use-points))
          mean-storm-bearing (determine-mean-storm-bearing get-next-bearing storm-centerpoint storm-bearing rows cols)
          wave-cells         (find-wave-cells storm-centerpoint mean-storm-bearing *wave-width* cell-width cell-height rows cols)
          wave-cell-depth    (get-wave-cell-depth mean-storm-bearing *wave-depth* cell-width cell-height)
          km2-per-cell       (* cell-width cell-height (Math/pow 10.0 -6.0))
          wave-energy        (*_ (* km2-per-cell wave-cell-depth) (get-in source-layer storm-centerpoint))
          storm-carriers     (map #(struct-map service-carrier
                                     :source-id       %
                                     :route           [%]
                                     :possible-weight wave-energy
                                     :actual-weight   wave-energy
                                     :sink-effects    {})
                                  wave-cells)]
      (distribute-wave-energy! storm-centerpoint
                               storm-bearing
                               storm-carriers
                               get-next-bearing
                               eco-sink-layer
                               geo-sink-layer
                               use-layer
                               cache-layer
                               possible-flow-layer
                               actual-flow-layer
                               animation?
                               km2-per-cell
                               wave-cell-depth
                               rows
                               cols)
      (println "Users affected:" (count (filter (& seq deref) (matrix2seq cache-layer))))
      (println "Simulation complete. Returning the cache-layer.")
      [(map-matrix (& seq deref) cache-layer)
       (map-matrix deref possible-flow-layer)
       (map-matrix deref actual-flow-layer)])))
