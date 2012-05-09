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
;;; This namespace defines the surface-water-topologic model.
;;;

(ns clj-span.models.surface-water-topologic
  (:use [clj-misc.utils         :only (seq2map mapmap p &)]
        [clj-misc.matrix-ops    :only (get-rows get-cols make-matrix map-matrix find-bounding-box
                                       filter-matrix-for-coords get-neighbors on-bounds?)]
        [clj-misc.point-algebra :only (nearest-point-where)]))

(refer 'clj-span.core :only '(distribute-flow! service-carrier))

(def ^:dynamic _0_)
(def ^:dynamic _+_)
(def ^:dynamic rv-fn)
(def ^:dynamic _min_)
(def ^:dynamic _max_)

(defn- step-upstream
  [closed-set in-stream? elevation-layer rows cols id]
  (if-let [in-stream-neighbors (seq (filter #(and (in-stream? %) (not (closed-set %)))
                                            (get-neighbors rows cols id)))]
    (let [neighbor-elevs      (map (p get-in elevation-layer) in-stream-neighbors)
          local-elev          (get-in elevation-layer id)
          max-elev            (reduce _max_ (cons local-elev neighbor-elevs))]
      (filter #(= max-elev (get-in elevation-layer %)) in-stream-neighbors))))

(defn- claim-locations-and-step-upstream!
  [in-stream-carriers closed-set claimant-list downstream-users
   captured-carriers elevation-layer rows cols [seeker search-list]]
  [seeker
   (apply concat
          (for [location search-list]
            (if (seq @(in-stream-carriers location))
              ;; Carriers are stored at this location.
              (if-let [claimant (@claimant-list location)]
                ;; Some other seeker claimed this location first
                (if (= location (first (@captured-carriers claimant)))
                  ;; This is the head of the claimant's
                  ;; captured-carriers list.  This seeker shares
                  ;; all their carriers.
                  (do
                    (dosync (alter downstream-users (p merge-with concat) {location [seeker]}))
                    nil)
                  ;; This location occurs partway through the
                  ;; claimant's captured-carriers list.  This
                  ;; seeker shares the subsequence of the
                  ;; claimant's carriers starting with this
                  ;; location.
                  (do
                    (dosync (alter captured-carriers assoc location nil)
                            (alter downstream-users (p merge-with concat) {location [seeker claimant]}))
                    nil))
                ;; This location has not yet been claimed. Let's
                ;; grab it, then continue upstream.
                (do
                  (dosync (alter closed-set conj location)
                          (alter claimant-list assoc location seeker)
                          (alter captured-carriers update-in [seeker] conj location))
                  (step-upstream closed-set in-stream-carriers elevation-layer rows cols location)))
              ;; No carriers are stored at this location. Mark it as
              ;; seen and continue upstream.
              (do
                (dosync (alter closed-set conj location))
                (step-upstream closed-set in-stream-carriers elevation-layer rows cols location)))))])

(defn- handle-sink-effects!
  "Computes the amount sunk by each sink encountered along an
   out-of-stream flow path. Reduces the sink-caps for each sink which
   captures some of the service medium. Returns remaining
   actual-weight and the local sink effects."
  [current-id actual-weight sink-caps]
  (dosync
   (let [sink-cap-ref      (sink-caps current-id)
         sink-cap          (deref sink-cap-ref)
         new-sink-effects  (rv-fn 'min                        actual-weight sink-cap)
         new-actual-weight (rv-fn '(fn [a s] (- a (min a s))) actual-weight sink-cap)
         new-sink-cap      (rv-fn '(fn [a s] (- s (min a s))) actual-weight sink-cap)]
     (alter sink-cap-ref (constantly new-sink-cap))
     [new-actual-weight new-sink-effects])))

(defn- search-upstream
  [elevation-layer rows cols in-stream-carriers stream-roots use-caps unsaturated-use?]
  (let [seekers           (keys stream-roots)
        closed-set        (ref #{})
        claimant-list     (ref {})
        downstream-users  (ref {})
        captured-carriers (ref (zipmap seekers (repeat [])))]
    (println "Searching upstream from stream-roots along" (count seekers) "paths...")
    (doseq [_ (take-while seq (iterate
                               (fn [seekers-and-search-lists]
                                 (filter (& seq second)
                                         (pmap
                                          (p claim-locations-and-step-upstream!
                                             in-stream-carriers
                                             closed-set
                                             claimant-list
                                             downstream-users
                                             captured-carriers
                                             elevation-layer
                                             rows
                                             cols)
                                          seekers-and-search-lists)))
                               (map #(vector % (list %)) seekers)))]
      (print "*") (flush))
    (println "\nSorting out inter-user dependencies...")
    ;; All entries in captured-carriers whose vals are nil are
    ;; placeholders for intersection locations between two seekers'
    ;; paths.  Extract all carriers upstream from this location from
    ;; its original claimant's carrier list and assign them to the
    ;; intersection location in captured-carriers.
    (doseq [id (map first (remove val @captured-carriers))]
      (let [claimant          (@claimant-list id)
            claimant-carriers (@captured-carriers claimant)]
        (dosync (alter captured-carriers assoc
                       id       (drop-while (p not= id) claimant-carriers)
                       claimant (take-while (p not= id) claimant-carriers)))))
    ;; Need to create a map of use-points (vals of stream-roots) to carriers.

    ;; Step 1: I need six pieces of information to make this work:
    ;;         1) {seekers -> acquired-actual-carriers}
    ;;         2) {seekers -> acquired-possible-carriers}
    ;;         3) {seekers -> remaining-actual-carriers}
    ;;         4) {seekers -> remaining-possible-carriers}
    ;;         5) {seekers -> actual-use-caps}
    ;;         6) {seekers -> possible-use-caps}
    ;;
    ;; Step 2: Wait! Upstream seekers should always get the first
    ;;         crack at water resources.  So, do this...
    ;;         
    ;;         For every seeker location:
    ;;           Lookup its users in stream-roots.
    ;;           Lookup its carriers in captured-carriers.
    ;;           Run a function with these carriers as the source
    ;;             inputs and the users as the beneficiaries.
    ;;           The function should deplete the actual-use-caps and
    ;;             possible-use-caps values in the maps created in
    ;;
    ;;             Step 1.  It should also reduce the possible and
    ;;             actual carrier weights in remaining-actual-carriers
    ;;             and remaining-possible-carriers and attach
    ;;             use-effects values to these carriers representing
    ;;             the amount lost.  Finally, it should create new
    ;;             carriers with the acquired actual and possible
    ;;             weights and the full routes to their users.
    ;;
    ;;           The order that this has to happen is:
    ;;
    ;;           1) All locations in downstream-users which are not
    ;;              seekers (i.e. not in stream-roots).  After
    ;;              depleting the carriers, attach any which are not
    ;;              _0_ to the ends of the captured-carriers sequences
    ;;              for both downstream-users and remove the
    ;;              captured-carriers entry for this shared location.
    ;;              Finally, remove this entry from downstream-users.
    ;;
    ;;           2) Now run all seekers.  Any who have greater than
    ;;              _0_ actual-use-cap or possible-use-cap can absorb
    ;;              everything they can get from their
    ;;              captured-carriers.
    ;;
    ;;           3) Finally, look at downstream users, and let them 
    ;;
    ;;         with no downstream seekers and let it absorbs all it
    ;;         wants from its privately claimed carriers.  Create an
    ;;         actual-use-caps and possible-use-caps map and update it
    ;;         during this option to contain the remaining actual and
    ;;         possible use capacity of each seeker.  Also update
    ;;         captured-carriers to reflect the new values of
    ;;
    ;;       Damnit, start over.
    ;;
    ;;       Downstream-users needs to be built as a proper directed
    ;;       graph.  Then, we just do this:
    ;;
    ;;       Update all seekers whose downstream-users value is nil
    ;;       (they have no downstream users, so they get everything in
    ;;       their captured-carriers list).  Next, start with a seeker
    ;;       with a non-nil value in downstream-users and check if it
    ;;       is a seeker.  If so, let it deplete its captured-carriers
    ;;       list.  Then proceed down the graph to its children,
    ;;       passing a sequence to each, which contains the list of
    ;;       ids back to the root.  Let each of these deplete their
    ;;       subsequences using sharing.  Then let each capture all of
    ;;       its own...
    ;;
    ;;       ======================================================
    ;;       Here's a note I found from my ACES scribblings:
    ;;
    ;;       ...

    (comment
    (let [actual-use-caps    use-caps
          possible-use-caps  (mapmap identity #(ref @%) use-caps)
          carriers-by-seeker (zipmap seekers (repeat (ref {})))
          foo-map            (zipmap seekers (repeat (ref {})))
          (doseq [id (remove downstream-users seekers)]
            (let [unshared-carriers     (@captured-carriers id)
                  total-actual-weight   (reduce _+_ _0_ (map :actual-weight   unshared-carriers))
                  total-possible-weight (reduce _+_ _0_ (map :possible-weight unshared-carriers))
                  [new-actual-weight   actual-use-effects]   (handle-sink-effects! id
                                                                                   total-actual-weight
                                                                                   actual-use-caps)
                  [new-possible-weight possible-use-effects] (handle-sink-effects! id
                                                                                   total-possible-weight
                                                                                   possible-use-caps)]
              

                                 "Resume here. I need to assign
                                  captured-carriers for these seekers
                                  to a cache for them
                                  somewhere (should make another map
                                  of refs per seeker id).  I should
                                  also store the remaining
                                  actual-weight and possible-weight
                                  values for each of these carriers in
                                  another map.  Then I need to get the
                                  algorithm that returns carriers per
                                  user for a combination of
                                  use-capacities and
                                  source-weights (in carbon-model or
                                  flood-model).  For each such user in
                                  these groupings, add the new
                                  carriers to their carrier caches.
                                  This will ultimately give me a map
                                  of seeker-ids to carrier-caches
                                  containing both their unshared
                                  carriers and the fractional carriers
                                  which they captured from the shared
                                  carrier sets.  This map should be
                                  returned as the final result and
                                  packed into the cache-layer."))]))
    (println "\nAll done.")
    (println
        "\nseekers"              (count seekers)
        "\nclosed-set"           (count @closed-set)
        "\nopen-set"             (count (clojure.set/difference (set (keys in-stream-carriers)) @closed-set))
        "\nclaimant-list"        claimant-list
        "\ndownstream-users"     downstream-users
        "\ncaptured-carriers"    captured-carriers)
    {}))

(defn find-nearest-stream-point-lazily
  [in-stream? rows cols id]
  (if (in-stream? id)
    id
    (first
     (drop-while nil?
                 (map #(first (filter in-stream? %))
                      (iterate (p find-bounding-box rows cols)
                               (find-bounding-box rows cols (list id))))))))

(defn find-nearest-stream-point
  [in-stream? rows cols id]
  (if (in-stream? id)
    id
    (loop [bounding-box (find-bounding-box rows cols (list id))]
      (if (seq bounding-box)
        (if-let [stream-point (first (filter in-stream? bounding-box))]
          stream-point
          (recur (find-bounding-box rows cols bounding-box)))))))

(defn find-nearest-stream-points
  [in-stream? rows cols use-points]
  (println "Finding nearest stream points to all users...")
;;  (let [stream-intakes (pmap (p nearest-point-where in-stream? [[0 rows][0 cols]]) use-points)]
  (let [stream-intakes (pmap (p find-nearest-stream-point in-stream? rows cols) use-points)]
    (doseq [_ (take-nth 100 stream-intakes)]
      (print "*") (flush))
    (println "\nAll done.")
    (dissoc (apply merge-with concat (map array-map stream-intakes (map list use-points))) nil)))

(defn- step-downhill
  "Returns the steepest downhill neighboring cell from id within the
   bounds [[0 rows] [0 cols]].  If id is lower than all of its
   neighbors, returns nil.  If more than one neighbor shares the
   steepest downhill slope, returns the first one found.  If
   exit-on-bounds? is true and the current id is located on the bounds
   [[0 rows][0 cols]], returns nil."
  [elevation-layer rows cols id]
  (if (not (on-bounds? rows cols id))
    (let [neighbors      (get-neighbors rows cols id)
          neighbor-elevs (map (p get-in elevation-layer) neighbors)
          local-elev     (get-in elevation-layer id)
          min-elev       (reduce _min_ (cons local-elev neighbor-elevs))]
      (first (filter #(= min-elev (get-in elevation-layer %)) neighbors)))))
(def step-downhill (memoize step-downhill))

(defn- head-streamward!
  "Computes the state of the surface-water-carrier after it takes
   another step downhill.  If it encounters a sink location, it drops
   some water according to the remaining sink capacity at this
   location."
  [in-stream-carriers sink-caps elevation-layer rows cols
   {:keys [route actual-weight sink-effects] :as surface-water-carrier}]
  (let [current-id (peek route)]
    (if-let [local-carrier-seq (in-stream-carriers current-id)]
      ;; We've reached a stream. Store the carrier in the local-carrier-seq.
      (do
        (dosync (alter local-carrier-seq conj surface-water-carrier))
        nil)
      ;; Continue until we get stuck in a low point or fall off the map.
      (if-let [new-id (step-downhill elevation-layer rows cols current-id)]
        ;; Compute the local sink-effects and the remaining
        ;; actual-weight.
        (if (or (= actual-weight _0_)
                (nil? (sink-caps current-id)))
          (assoc surface-water-carrier
            :route         (conj route new-id)
            :actual-weight actual-weight
            :sink-effects  sink-effects)
          (let [[new-actual-weight new-sink-effects] (handle-sink-effects! current-id
                                                                           actual-weight
                                                                           sink-caps)]
            (assoc surface-water-carrier
              :route         (conj route new-id)
              :actual-weight new-actual-weight
              :sink-effects  (assoc sink-effects current-id new-sink-effects))))))))

(defn- shift-source-to-stream
  "Constructs a sequence of surface-water-carrier objects (one per
   source point) and then iteratively propagates them downhill until
   they reach a stream location, get stuck in a low elevation point,
   or fall off the map bounds.  All the carriers are moved together in
   timesteps (more or less).  When the simulation is complete, a map
   of in-stream-ids to refs of vectors of carriers in each location is
   returned."
  [source-points source-layer sink-caps stream-points elevation-layer rows cols]
  (println "Moving the surface-water-carriers downhill to streams...")
  (let [in-stream-carriers (zipmap stream-points (repeatedly #(ref [])))]
    (doseq [_ (take-while seq (iterate
                               (fn [surface-water-carriers]
                                 (remove nil?
                                         (pmap (p head-streamward!
                                                  in-stream-carriers
                                                  sink-caps
                                                  elevation-layer
                                                  rows
                                                  cols)
                                               surface-water-carriers)))
                               (map
                                #(let [source-weight (get-in source-layer %)]
                                   (struct-map service-carrier
                                     :source-id       %
                                     :route           [%]
                                     :possible-weight source-weight
                                     :actual-weight   source-weight
                                     :sink-effects    {}))
                                source-points)))]
      (print "*") (flush))
    (println "\nAll done.")
    in-stream-carriers))

(defmethod distribute-flow! "SurfaceWaterMovement"
  [{:keys [source-layer sink-layer use-layer flow-layers
           cache-layer possible-flow-layer actual-flow-layer
           source-points sink-points use-points
           cell-width cell-height rows cols value-type]}]
  (let [{stream-layer "River",
         elevation-layer "Altitude"} flow-layers
        prob-ns (case value-type
                  :numbers  'clj-misc.numbers
                  :varprop  'clj-misc.varprop
                  :randvars 'clj-misc.randvars)]
    (binding [_0_   (var-get (ns-resolve prob-ns '_0_))
              _+_   (var-get (ns-resolve prob-ns '_+_))
              rv-fn (var-get (ns-resolve prob-ns 'rv-fn))
              _min_ (var-get (ns-resolve prob-ns '_min_))
              _max_ (var-get (ns-resolve prob-ns '_max_))]
      (let [stream-points (filter-matrix-for-coords (p not= _0_) stream-layer)]
        (println "Stream points:" (count stream-points))
        (let [sink-caps          (seq2map sink-points (fn [id] [id (ref (get-in sink-layer id))]))
              use-caps           (seq2map use-points  (fn [id] [id (ref (get-in use-layer  id))]))
              unsaturated-use?   (fn [id] (not= _0_ (deref (use-caps id))))
              in-stream-carriers (shift-source-to-stream source-points
                                                         source-layer
                                                         sink-caps
                                                         stream-points
                                                         elevation-layer
                                                         rows
                                                         cols)
              stream-roots       (find-nearest-stream-points in-stream-carriers rows cols use-points)
              carrier-caches     (search-upstream elevation-layer rows cols in-stream-carriers stream-roots use-caps unsaturated-use?)]
          ;; Update the cache-layer.
          (dosync
           (doseq [[id cache] carrier-caches]
             (alter (get-in cache-layer id) (constantly cache)))))))))
