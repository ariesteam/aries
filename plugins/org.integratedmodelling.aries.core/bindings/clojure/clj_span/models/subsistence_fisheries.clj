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
;;; 1. Setup source caches at all source points.
;;; 2. For each use point, find its nearest path point and the closest shore point.
;;; 3. Follow the path, using a closed list and the shore orientation as a shortest-distance heuristic.
;;; 4. Make a map of these shore paths for each use point.
;;; 5. Each fisherman will enter the water at this shore point and
;;;    begin to fish simultaneously, thereby depleting the local
;;;    source caches.
;;; 6. If a location does not contain enough fish to meet the
;;;    fisherman's demand, it proceeds to fish in each neighboring
;;;    water cell until either its demand has been met or all cells
;;;    within 5km of the initial shore point have been exhausted.
;;; 7. The simulation ends when either all fishermen have acquired
;;;    their use value in fish or exhausted their fishing region
;;;    (within 5km buffer of the initial shore point).

(ns clj-span.models.subsistence-fisheries
  (:use [clj-misc.utils      :only (p
                                    &
                                    seq2map
                                    seq2redundant-map
                                    count-distinct
                                    angular-distance
                                    with-message
                                    with-progress-bar-cool
                                    iterate-while-seq
                                    my-partition-all
                                    shortest-path-bfgs
                                    shortest-path-idgs)]
        [clj-misc.matrix-ops :only (subtract-ids
                                    filter-matrix-for-coords
                                    get-neighbors
                                    get-bearing
                                    find-nearest
                                    find-in-range
                                    find-line-between)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier with-typed-math-syms))

(def #^{:dynamic true} _0_)
(def #^{:dynamic true} *_)
(def #^{:dynamic true} _d)
(def #^{:dynamic true} _*_)
(def #^{:dynamic true} _+_)
(def #^{:dynamic true} _>)
(def #^{:dynamic true} rv-fn)

(defstruct fisherman :need :route :cache :fishing-area)

(defn go-fish!
  [fish-supply fish-left? possible-flow-layer actual-flow-layer km2-per-cell
   {:keys [need route cache fishing-area] :as fisherman}]
  (dosync
   (let [current-id       (peek route)
         local-supply-ref (fish-supply current-id)
         local-supply     (deref local-supply-ref)
         need-remaining   (if (_> local-supply 0.0) ;; Check for fish locally.
                            ;; There are fish here. Let's get some.
                            (let [fish-caught         (rv-fn 'min local-supply need)
                                  fish-remaining      (rv-fn '(fn [s n] (- s (min s n))) local-supply need)
                                  need-remaining      (rv-fn '(fn [s n] (- n (min s n))) local-supply need)
                                  fish-caught-per-km2 (_d fish-caught km2-per-cell)]
                              ;; Reduce the local-supply-ref by this amount.
                              (alter local-supply-ref (constantly fish-remaining))
                              ;; Update the possible and actual flow layers.
                              (doseq [id route]
                                (alter (get-in possible-flow-layer id) _+_ fish-caught-per-km2)
                                (alter (get-in actual-flow-layer   id) _+_ fish-caught-per-km2))
                              ;; Store a service-carrier in your cache.
                              (alter cache conj (struct-map service-carrier
                                                  :source-id       current-id
                                                  ;;:route           (rseq route) ;; Deprecated
                                                  :possible-weight fish-caught-per-km2
                                                  :actual-weight   fish-caught-per-km2))
                              need-remaining)
                            need)]
     ;; On to the next.
     (if (_> need-remaining 0.0)
       (if-let [fishing-area-remaining (seq (filter fish-left? fishing-area))]
         (assoc fisherman
           :need         need-remaining
           :route        (conj (pop route) (first fishing-area-remaining))
           :fishing-area (rest fishing-area-remaining)))))))

(defn send-forth-fishermen!
  [{:keys [fishermen fish-supply possible-flow-layer actual-flow-layer km2-per-cell] :as params}]
  (with-message "Let's go fishing...\n" "All done."
    (let [fish-left? #(if-let [supply-ref (fish-supply %)] (_> (deref supply-ref) 0.0))]
      (dorun (iterate-while-seq
              #(with-message "Fishermen: " count
                 (doall (pmap (p go-fish! fish-supply fish-left? possible-flow-layer actual-flow-layer km2-per-cell) %)))
              fishermen)))))

(def #^{:dynamic true} *fishing-range* 5000.0) ;; max distance in meters that a fisherman can sail from shore

;; FIXME: Use locations with no path to the coast will not create fishermen agents.
(defn make-fishermen
  [{:keys [fishing-spot? fish-demand fishing-routes cache-layer cell-width cell-height rows cols] :as params}]
  (assoc params
    :fishermen (with-message
                 "Creating fishermen agents...\n"
                 #(str
                   "Total fishermen agents:      " (count %) "\n"
                   "Distinct launch points:      " (count (distinct (map (& peek :route) %))) "\n"
                   "Distinct route lengths:      " (count-distinct (map (& count :route) %)) "\n"
                   "Distinct fishing area sizes: " (count-distinct (map (& count :fishing-area) %)))
                 (remove nil?
                         (with-progress-bar-cool
                           :keep
                           (count fish-demand)
                           (apply concat
                                  (pmap (fn [demand-part route-part]
                                          (doall
                                           (map (fn [demand route]
                                                  (if route
                                                    (struct-map fisherman
                                                      :need         demand
                                                      :route        route
                                                      :cache        (get-in cache-layer (first route))
                                                      :fishing-area (doall (find-in-range fishing-spot?
                                                                                          *fishing-range*
                                                                                          cell-width
                                                                                          cell-height
                                                                                          rows
                                                                                          cols
                                                                                          (peek route))))))
                                                demand-part route-part)))
                                        (my-partition-all 1000 fish-demand)
                                        (my-partition-all 1000 fishing-routes))))))))

(defn nearest-to-bearing
  [bearing id neighbors]
  (if (seq neighbors)
    (let [bearing-changes (seq2redundant-map neighbors
                                             #(let [bearing-to-neighbor (subtract-ids % id)]
                                                [(angular-distance bearing bearing-to-neighbor) [%]])
                                             concat)]
      (bearing-changes (apply min (keys bearing-changes))))))

(defn follow-path
  [path? rows cols id]
  (filter path? (get-neighbors rows cols id)))
(def follow-path (memoize follow-path))

(defn find-shortest-paths-to-coast-heuristic
  [{:keys [path? fishing-spot? rows cols use-points] :as params}]
  (assoc params
    :fishing-routes (let [follow-path-wrapper (p follow-path path? rows cols)]
                      (with-message "Finding paths to coast...\n" "\nAll done."
                        (with-progress-bar-cool
                          :keep
                          (count use-points)
                          (map
                           #(let [path-root    (find-nearest path?         rows cols %)
                                  fishing-spot (find-nearest fishing-spot? rows cols %)]
                              (if (= path-root fishing-spot)
                                (vec (find-line-between % path-root))
                                (if-let [path-ids (seq (shortest-path-bfgs path-root
                                                                           follow-path-wrapper
                                                                           fishing-spot?
                                                                           (p nearest-to-bearing (get-bearing path-root fishing-spot))))]
                                  (vec (concat (find-line-between % path-root) (rest path-ids))))))
                           use-points))))))

(defn find-shortest-paths-to-coast-idgs
  [{:keys [path? fishing-spot? rows cols use-points] :as params}]
  (assoc params
    :fishing-routes (let [follow-path-wrapper (p follow-path path? rows cols)]
                      (with-message "Finding paths to coast...\n" "\nAll done."
                        (with-progress-bar-cool
                          :keep
                          (count use-points)
                          (map
                           #(let [path-root (find-nearest path? rows cols %)]
                              (if-let [path-ids (seq (shortest-path-idgs path-root
                                                                         follow-path-wrapper
                                                                         fishing-spot?))]
                                (vec (concat (find-line-between % path-root) (rest path-ids)))))
                           use-points))))))

(defn find-shortest-paths-to-coast
  "Fuck it. I'm just drawing a line and going to bed."
  [{:keys [path? fishing-spot? rows cols use-points] :as params}]
  (assoc params
    :fishing-routes (with-message "Finding paths to coast...\n" "\nAll done."
                      (with-progress-bar-cool
                        :keep
                        (count use-points)
                        (apply concat
                               (pmap
                                (fn [part]
                                  (doall
                                   (map #(let [path-root    (find-nearest path?         rows cols %)
                                               fishing-spot (find-nearest fishing-spot? rows cols %)]
                                           (vec (concat (find-line-between % path-root)
                                                        (rest (find-line-between path-root fishing-spot)))))
                                        part)))
                                (my-partition-all 1000 use-points)))))))

(defn calculate-fish-supply-and-demand
  [{:keys [source-layer source-points use-layer use-points flow-layers km2-per-cell] :as params}]
  (let [population-density-layer (flow-layers "PopulationDensity")]
    (assoc params
      :fish-supply (seq2map source-points
                            (fn [id] [id (ref (*_ km2-per-cell ;; km^2
                                                  (get-in source-layer id)))])) ;; kg/km^2*year
      :fish-demand (map (fn [id] (*_ km2-per-cell             ;; km^2
                                     (_*_ (get-in use-layer id) ;; kg/person*year
                                          (get-in population-density-layer id)))) ;; person/km^2
                        use-points))))

(defn create-feature-tests
  "Stores sets in params map of all fishing-spot and path ids under :fishing-spot? and :path? respectively."
  [{:keys [source-points flow-layers] :as params}]
  (assoc params
    :fishing-spot? (set source-points)
    :path?         (set (filter-matrix-for-coords #(not= _0_ %) (flow-layers "Path")))))

(defn compute-km2-per-cell
  "Stores cell-width * cell-height * 10^-6 under (params :km2-per-cell)."
  [{:keys [cell-width cell-height] :as params}]
  (assoc params
    :km2-per-cell (* cell-width cell-height (Math/pow 10.0 -6.0))))

;; FIXME: Because Theoretical Use is in kg/person*year and
;;        Possible/Actual Use are in kg/km^2*year, Inaccessible Use
;;        will not make sense.
(defmethod distribute-flow! "SubsistenceFishAccessibility"
  [{:keys [source-layer use-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points use-points
           value-type cell-width cell-height rows cols]
    :as params}]
  (with-typed-math-syms value-type [_0_ *_ _d _*_ _+_ _> rv-fn]
    (-> params
        compute-km2-per-cell
        create-feature-tests
        calculate-fish-supply-and-demand
        find-shortest-paths-to-coast
        make-fishermen
        send-forth-fishermen!)))
