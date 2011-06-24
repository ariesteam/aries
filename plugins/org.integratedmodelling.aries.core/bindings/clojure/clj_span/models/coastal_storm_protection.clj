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

(ns clj-span.models.coastal-storm-protection
  (:use [clj-span.params     :only (*trans-threshold*)]
        [clj-span.core       :only (distribute-flow! service-carrier)]
        [clj-misc.utils      :only (p
                                    my->>
                                    seq2map
                                    angular-distance
                                    angular-rotation
                                    to-radians
                                    reduce-true
                                    with-progress-bar*
                                    with-message)]
        [clj-misc.matrix-ops :only (add-ids
                                    subtract-ids
                                    in-bounds?
                                    on-bounds?
                                    rotate-2d-vec
                                    get-bearing
                                    get-neighbors
                                    dist-to-steps
                                    find-point-at-dist-in-m
                                    find-line-between)]
        [clj-misc.varprop    :only (_0_ _+_ _*_ *_ _d rv-fn _>)]))

(defn handle-sink-effects
  [current-id possible-weight actual-weight eco-sink-layer geo-sink-layer m2-per-cell]
  (let [eco-sink-height  (get-in eco-sink-layer current-id)
        geo-sink-height  (get-in geo-sink-layer current-id)
        eco-sink?        (not= _0_ eco-sink-height)
        geo-sink?        (not= _0_ geo-sink-height)
        eco-sink-cap     (if eco-sink? (*_ m2-per-cell eco-sink-height))
        geo-sink-cap     (if geo-sink? (*_ m2-per-cell geo-sink-height))
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
  [current-location use-layer cache-layer storm-surge-cell-depth
   {:keys [route possible-weight actual-weight] :as post-sink-carrier}]
  (let [vulnerability-unscaled (get-in use-layer current-location)]
    (if (not= _0_ vulnerability-unscaled)
      (let [vulnerability             (_d vulnerability-unscaled storm-surge-cell-depth)
            possible-damage-inflicted (_*_ possible-weight vulnerability)
            actual-damage-inflicted   (_*_ actual-weight   vulnerability)]
        (dosync (alter (get-in cache-layer current-location) conj
                       (assoc post-sink-carrier
                         ;;:route           (bitpack-route route) ;; Deprecated
                         :route           nil
                         :possible-weight possible-damage-inflicted
                         :actual-weight   actual-damage-inflicted)))))))

(defn get-bearing-seq
  [get-next-bearing source-id initial-bearing]
  (take-while (fn [[id bearing]] bearing)
              (iterate
               (fn [[id bearing]] (let [next-id (add-ids id bearing)]
                                    [next-id (get-next-bearing next-id bearing)]))
               [source-id initial-bearing])))

(defstruct storm-surge :carriers :path :cell-depth)

(defn find-storm-surge-cells
  [storm-centerpoint sample-bearing storm-surge-width cell-width cell-height rows cols]
  (let [storm-surge-orientation (rotate-2d-vec (to-radians 90.0) sample-bearing) ;; rotate 90 degrees left
        storm-surge-reach       (* storm-surge-width 0.5)
        storm-surge-left-edge   (find-point-at-dist-in-m storm-centerpoint
                                                         storm-surge-orientation
                                                         storm-surge-reach
                                                         cell-width
                                                         cell-height)
        storm-surge-right-edge  (find-point-at-dist-in-m storm-centerpoint
                                                         (map - storm-surge-orientation)
                                                         storm-surge-reach
                                                         cell-width
                                                         cell-height)
        left-storm-surge-cells  (filter (p in-bounds? rows cols)
                                        (find-line-between storm-surge-left-edge storm-centerpoint))
        right-storm-surge-cells (filter (p in-bounds? rows cols)
                                        (rest (find-line-between storm-centerpoint storm-surge-right-edge)))]
    (concat
     (map (fn [id] [id :left  (subtract-ids id storm-centerpoint)]) left-storm-surge-cells)
     (map (fn [id] [id :right (subtract-ids id storm-centerpoint)]) right-storm-surge-cells))))

(def *storm-surge-width* 100000.0) ;; in meters
(def *storm-surge-depth* 5000.0)   ;; in meters

(defn make-storm-surge
  [source-layer get-next-bearing storm-centerpoint storm-bearing sample-bearing m2-per-cell cell-width cell-height rows cols]
  (with-message "Constructing storm surge..."
    #(let [n (count (:carriers %))] (str "done. (" n (if (== n 1) " carrier)" " carriers)")))
    (let [storm-surge-cell-depth (dist-to-steps sample-bearing
                                                *storm-surge-depth*
                                                cell-width
                                                cell-height)
          storm-surge-volume     (*_ (* m2-per-cell storm-surge-cell-depth)
                                     (get-in source-layer storm-centerpoint))
          storm-surge-cells      (find-storm-surge-cells storm-centerpoint
                                                         sample-bearing
                                                         *storm-surge-width*
                                                         cell-width
                                                         cell-height
                                                         rows
                                                         cols)
          storm-carriers         (map (fn [[id side offset]]
                                        (struct-map service-carrier
                                          :source-id          id
                                          :route              [id]
                                          :possible-weight    storm-surge-volume ;; m^3 of storm surge
                                          :actual-weight      storm-surge-volume ;; m^3 of storm surge
                                          :sink-effects       {}
                                          :side               side
                                          :offset             offset))
                                      storm-surge-cells)]
      (struct-map storm-surge
        :carriers   storm-carriers
        :path       (map first (rest (get-bearing-seq get-next-bearing storm-centerpoint storm-bearing)))
        :cell-depth storm-surge-cell-depth))))

(defn mean-bearing
  [bearings]
  (let [[x-total y-total] (reduce (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)]) bearings)
        num-bearings      (count bearings)]
    [(/ x-total num-bearings) (/ y-total num-bearings)]))

(defstruct storm-track-sample :head :bearing :length)

(defn advance-storm-track-sample
  [{:keys [head bearing length]}]
  (if-let [new-head (seq (rest head))]
    (let [samples    (take length new-head)
          new-length (count samples)]
      (if (< new-length length)
        ;; The window is shrinking, so balance it on both sides.
        (struct-map storm-track-sample
          :head    (rest new-head)
          :bearing (mean-bearing (map second (rest samples)))
          :length  (- length 2))
        ;; The sample window simply slides forward by one step.
        (struct-map storm-track-sample
          :head    new-head
          :bearing (mean-bearing (map second samples))
          :length  length)))))

(def *max-sample-window-size* 10000.0) ; in meters

(defn make-storm-track-sample
  [get-next-bearing storm-centerpoint storm-bearing cell-width cell-height rows cols]
  (with-message "Constructing storm track sample..."
    #(let [n (:length %) b (:bearing %)]
       (str "done. (Bearing " b " from " n (if (== n 1) " sample)" " samples)")))
    (if-let [storm-bearing-reversed (get-next-bearing storm-centerpoint (map - storm-bearing))]
      (let [num-samples      (dist-to-steps storm-bearing-reversed (* 0.5 *max-sample-window-size*) cell-width cell-height)
            reversed-samples (my->> storm-bearing-reversed
                                    (get-bearing-seq get-next-bearing storm-centerpoint)
                                    (take num-samples))
            num-behind       (count reversed-samples)
            sample-head      (my->> (last reversed-samples)
                                    ((fn [[id next-bearing]] [(add-ids id next-bearing) (map - next-bearing)]))
                                    (apply get-bearing-seq get-next-bearing))
            expected-samples (inc (* num-behind 2))
            samples          (take expected-samples sample-head)
            acquired-samples (count samples)
            sample-head      (drop (- expected-samples acquired-samples) sample-head)
            samples          (drop (- expected-samples acquired-samples) samples)
            bearing          (mean-bearing (map second samples))]
        (struct-map storm-track-sample
          :head    sample-head
          :bearing bearing
          :length  (count samples)))
      (struct-map storm-track-sample
        :head    (get-bearing-seq get-next-bearing storm-centerpoint storm-bearing)
        :bearing storm-bearing
        :length  1))))

(defn find-cells-along-arc
  [origin theta {:keys [side offset route]}]
  ;;(if (or (and (pos? theta) (= side :right)) ;; left turn: only advance right-side cells
  ;;        (and (neg? theta) (= side :left))) ;; right turn: only advance left-side cells
  (if (zero? theta)
    ;; Take a step forward in the direction of the storm bearing.
    [nil (list (add-ids origin offset))]
    ;; Rotate around to catch up to the new storm surge front.
    (let [new-offset (let [[dy dx] (rotate-2d-vec theta offset)]
                       [(int (Math/round dy)) (int (Math/round dx))])]
      [new-offset (rest (find-line-between (peek route) (add-ids origin new-offset)))])))

(defn explore-next-segment!
  [next-storm-centerpoint storm-surge-turning-angle eco-sink-layer use-layer
   geo-sink-layer cache-layer possible-flow-layer actual-flow-layer
   m2-per-cell rows cols trans-threshold-volume cell-depth storm-carrier]
  (let [[new-offset next-storm-segment] (find-cells-along-arc next-storm-centerpoint
                                                              storm-surge-turning-angle
                                                              storm-carrier)]
    (reduce-true (fn [{:keys [route possible-weight actual-weight sink-effects] :as storm-carrier} current-location]
                   (if (in-bounds? rows cols current-location)
                     (let [[new-possible-weight new-actual-weight new-sink-effects] (handle-sink-effects current-location
                                                                                                         possible-weight
                                                                                                         actual-weight
                                                                                                         eco-sink-layer
                                                                                                         geo-sink-layer
                                                                                                         m2-per-cell)
                           post-sink-carrier (assoc storm-carrier
                                               :route           (conj route current-location)
                                               :possible-weight new-possible-weight
                                               :actual-weight   new-actual-weight
                                               :sink-effects    (merge-with _+_ sink-effects new-sink-effects))]
                       (dosync
                        (alter (get-in possible-flow-layer current-location) _+_ possible-weight)
                        (alter (get-in actual-flow-layer   current-location) _+_ actual-weight))
                       (handle-local-users! current-location use-layer cache-layer cell-depth post-sink-carrier)
                       (if (_> new-possible-weight trans-threshold-volume)
                         post-sink-carrier))))
                 (if new-offset (assoc storm-carrier :offset new-offset) storm-carrier)
                 next-storm-segment)))

;; Logic for advance-storm!:
;;
;; To move the carriers from the first wave position to the new wave position, do this:
;; 0) Terminate and return nil if the storm-track-sample cannot be advanced.
;; 1) For each carrier in the wave (in parallel):
;;    1) Find its new location by using its offset and side as well as the storm-surge-turning-angle.
;;    2) Find the cells that it will pass through from its current location to its new one (again possible using the turning angle).
;;    3) Push the carrier through each of those locations, updating sinks, uses, and the possible-flow, actual-flow, and cache layers.
;;    4) If a carrier runs out of possible-weight or hits the map bounds, return nil.
;; 2) Collect up the new carriers and remove the nils.
;; 3) If there are no carriers left, terminate and return nil.
;; 4) Otherwise, construct a new storm-surge object.
;; 5) Return [new-storm-track-sample new-storm-surge].
(defn advance-storm!
  [eco-sink-layer use-layer geo-sink-layer cache-layer possible-flow-layer actual-flow-layer
   m2-per-cell rows cols trans-threshold-volume [storm-track-sample {:keys [carriers path cell-depth]}]]
  (if-let [next-storm-track-sample (advance-storm-track-sample storm-track-sample)]
    (let [next-storm-centerpoint    (first path)
          storm-surge-turning-angle (angular-rotation (:bearing storm-track-sample)
                                                      (:bearing next-storm-track-sample))]
      (if-let [new-carriers (seq (doall (remove nil? (pmap (p explore-next-segment!
                                                              next-storm-centerpoint
                                                              storm-surge-turning-angle
                                                              eco-sink-layer
                                                              use-layer
                                                              geo-sink-layer
                                                              cache-layer
                                                              possible-flow-layer
                                                              actual-flow-layer
                                                              m2-per-cell
                                                              rows
                                                              cols
                                                              trans-threshold-volume
                                                              cell-depth)
                                                           carriers))))]
        [next-storm-track-sample
         (struct-map storm-surge
           :carriers   new-carriers
           :path       (rest path)
           :cell-depth cell-depth)]))))

(defn run-storm-surge-simulation!
  [source-layer eco-sink-layer use-layer geo-sink-layer cache-layer
   possible-flow-layer actual-flow-layer cell-width cell-height rows
   cols storm-centerpoint storm-bearing get-next-bearing]
  (let [m2-per-cell        (* cell-width cell-height)
        storm-track-sample (make-storm-track-sample get-next-bearing
                                                    storm-centerpoint
                                                    storm-bearing
                                                    cell-width
                                                    cell-height
                                                    rows
                                                    cols)
        storm-surge        (make-storm-surge source-layer
                                             get-next-bearing
                                             storm-centerpoint
                                             storm-bearing
                                             (:bearing storm-track-sample)
                                             m2-per-cell
                                             cell-width
                                             cell-height
                                             rows
                                             cols)]
    (with-message "Moving the storm surge toward the coast...\n" "All done."
      (with-progress-bar*
        10 ;; show a * every 10 iterations
        (take-while seq (iterate (p advance-storm!
                                    eco-sink-layer
                                    use-layer
                                    geo-sink-layer
                                    cache-layer
                                    possible-flow-layer
                                    actual-flow-layer
                                    m2-per-cell
                                    rows
                                    cols
                                    (* *trans-threshold* m2-per-cell))
                                 [storm-track-sample storm-surge]))))))

;; FIXME: It would be good to have a faster way to compute
;;        this. Perhaps a sampling method?
(defn find-bearing-to-users
  [storm-centerpoint use-points]
  (with-message "Computing direction to users..."
    (fn [[dy dx]] (format "done. (Bearing [%.2f %.2f])" dy dx))
    (mean-bearing (map (p get-bearing storm-centerpoint) use-points))))

(defn get-next-bearing
  [on-track? rows cols curr-id prev-bearing]
  (if-not (on-bounds? rows cols curr-id)
    (let [prev-id            (subtract-ids curr-id prev-bearing)
          on-track-neighbors (filter #(and (on-track? %) (not= prev-id %))
                                     (get-neighbors rows cols curr-id))]
      (if (seq on-track-neighbors)
        (let [bearing-changes (seq2map (map #(subtract-ids % curr-id) on-track-neighbors)
                                       (fn [bearing-to-neighbor]
                                         [(angular-distance prev-bearing bearing-to-neighbor)
                                          bearing-to-neighbor]))]
          (bearing-changes (apply min (keys bearing-changes))))))))

;; FIXME: Theoretical source is a point, but we generate a storm surge
;;        as a line of cells.  This makes the Theoretical and
;;        Inacessible Source maps looks wrong in the result dataset.
;; FIXME: Try a sphere or circle instead of a wave line.
;; FIXME: Try accounting for rotational cyclone dynamics.
(defmethod distribute-flow! "CoastalStormMovement"
  [_ cell-width cell-height rows cols cache-layer possible-flow-layer
   actual-flow-layer source-layer eco-sink-layer use-layer source-points
   _ use-points {storm-track-layer "StormTrack", geo-sink-layer "GeomorphicWaveReduction"}]
  (let [storm-centerpoint (first source-points)
        on-track?         #(not= _0_ (get-in storm-track-layer %))
        get-next-bearing  (p get-next-bearing on-track? rows cols)]
    (if-let [storm-bearing (get-next-bearing storm-centerpoint (find-bearing-to-users storm-centerpoint use-points))]
      (run-storm-surge-simulation! source-layer
                                   eco-sink-layer
                                   use-layer
                                   geo-sink-layer
                                   cache-layer
                                   possible-flow-layer
                                   actual-flow-layer
                                   cell-width
                                   cell-height
                                   rows
                                   cols
                                   storm-centerpoint
                                   storm-bearing
                                   get-next-bearing)
      (println "Either the storm source point" storm-centerpoint "is on the map boundary or no storm tracks lead away from it."))))
