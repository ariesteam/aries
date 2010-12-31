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
;;; This namespace defines the subsistence-fisheries model.
;;;
;;; Procedure:
;;; 0. Setup source caches at all source points.
;;; 1. For each use point, find its nearest path point and the closest shore point.
;;; 2. Follow the path, using a closed list and the shore orientation as a shortest-distance heuristic.
;;; 3. Make a map of these shore paths for each use point.
;;; 4. Enter the water and start depleting the source caches.
;;; 5. A fisherman will continue to work in the same area until either the source cache runs out or his demand is met.
;;; 6. If the fisherman still has unmet demand after his source cache is depleted,
;;;    radiate into all neighboring, fish-producing pixels, split the demand evenly (naive, I know), and continue.
;;; 7. Once all demand is met, end the simulation and return the cache-layer.

(ns clj-span.subsistence-fisheries-model
  (:use [clj-span.model-api  :only (distribute-flow service-carrier)]
        [clj-misc.utils      :only (p
                                    &
                                    seq2map
                                    mapmap
                                    angular-distance
                                    with-progress-bar
                                    iterate-while-seq
                                    shortest-path)]
        [clj-misc.matrix-ops :only (get-rows
                                    get-cols
                                    make-matrix
                                    map-matrix
                                    subtract-ids
                                    filter-matrix-for-coords
                                    get-neighbors
                                    get-bearing
                                    find-nearest
                                    find-line-between)]
        [clj-misc.randvars   :only (_0_ _d *_ _*_ rv-fn)]))

(defstruct fisherman :need :route :cache)

;; FIXME: These fishermen are not constrained by distance and can only
;;        travel through cells containing fish.
(defn go-fish!
  [fish-supply fish-left? rows cols {:keys [need route cache] :as fisherman}]
  (dosync
   (let [current-id       (peek route)
         local-supply-ref (fish-supply current-id)
         local-supply     (deref local-supply-ref)
         need-remaining   (if (not= _0_ local-supply) ;; Check for fish locally.
                            ;; There are fish here. Let's get some.
                            (let [fish-caught    (rv-fn min local-supply need)
                                  fish-remaining (rv-fn (fn [s n] (- s (min s n))) local-supply need)
                                  need-remaining (rv-fn (fn [s n] (- n (min s n))) local-supply need)]
                              ;; Reduce the local-supply-ref by this amount.
                              (alter local-supply-ref (constantly fish-remaining))
                              ;; Store a service-carrier in your cache.
                              (alter cache conj (struct-map service-carrier
                                                  :source-id       current-id
                                                  :route           (rseq route)
                                                  :possible-weight fish-caught
                                                  :actual-weight   fish-caught))
                              need-remaining)
                            need)]
     ;; On to the next.
     (if (not= _0_ need-remaining)
       (if-let [fishable-neighbors (seq (filter fish-left? (get-neighbors rows cols current-id)))]
         (let [split-need (_d need-remaining (count fishable-neighbors))]
           (for [id fishable-neighbors]
             (assoc fisherman
               :need  split-need
               :route (conj route id)))))))))

(defn send-forth-fishermen!
  [fishermen source-kg rows cols]
  (let [fish-supply (mapmap identity ref source-kg)
        fish-left?  #(not= _0_ (deref (fish-supply %)))]
    (println "Fishing time...")
    (with-progress-bar
      (iterate-while-seq
       (p apply concat (p pmap (p go-fish! fish-supply fish-left? rows cols)))
       fishermen))
    (println "All done.")))

(defn make-fishermen
  [use-kg fishing-routes cache-layer]
  (map (fn [use-val route]
         (struct-map fisherman
           :need  use-val
           :route route
           :cache (get-in cache-layer (first route))))
       use-kg
       fishing-routes))

(defn find-shortest-paths-to-coast
  [path? fishing-spot? rows cols use-points]
  (println "Finding paths to coast...")
  (with-progress-bar
    (pmap
     #(let [path-root      (find-nearest path?         rows cols %)
            fishing-spot   (find-nearest fishing-spot? rows cols %)
            bearing        (get-bearing path-root fishing-spot)
            follow-path    (fn [id] (filter path? (get-neighbors rows cols id)))
            follow-bearing (fn [id neighbors] (let [neighbor-bearings  (map (fn [nid] (subtract-ids nid id)) neighbors)
                                                    bearing-deviations (map (p angular-distance bearing) neighbor-bearings)
                                                    min-deviation      (apply min bearing-deviations)]
                                                (keys (filter (fn [[_ dev]] (== dev min-deviation))
                                                              (zipmap neighbors bearing-deviations)))))]
        (vec (concat (find-line-between % path-root)
                     (rest (shortest-path path-root follow-path fishing-spot? follow-bearing)))))
     use-points))
  (println "All done."))

(defmethod distribute-flow "SubsistenceFishAccessibility"
  [_ cell-width cell-height source-layer _ use-layer
   {path-layer "Path", population-density-layer "PopulationDensity"}]
  (println "Running Subsistence Fisheries flow model.")
  (let [rows                (get-rows source-layer)
        cols                (get-cols source-layer)
        cache-layer         (make-matrix rows cols (fn [_] (ref ())))
        [source-points use-points path-points] (pmap (p filter-matrix-for-coords (p not= _0_))
                                                     [source-layer use-layer path-layer])]
    (println "Source points: " (count source-points))
    (println "Use points:    " (count use-points))
    (let [km2-per-cell   (* cell-width cell-height (Math/pow 10.0 -6.0))
          source-kg      (seq2map source-points (fn [id] [id (*_ km2-per-cell                                   ;; km^2
                                                                 (get-in source-layer id))]))                   ;; kg/km^2*year
          use-kg         (seq2map use-points    (fn [id] [id (*_ km2-per-cell                                   ;; km^2
                                                                 (_*_ (get-in use-layer id)                     ;; kg/person*year
                                                                      (get-in population-density-layer id)))])) ;; person/km^2
          path?          (set path-points)
          fishing-spot?  (set source-points)
          fishing-routes (find-shortest-paths-to-coast path? fishing-spot? rows cols use-points)
          fishermen      (make-fishermen use-kg fishing-routes cache-layer)]
      (send-forth-fishermen! fishermen source-kg rows cols)
      (println "Simulation complete. Returning the cache-layer.")
      (map-matrix (& seq deref) cache-layer))))
