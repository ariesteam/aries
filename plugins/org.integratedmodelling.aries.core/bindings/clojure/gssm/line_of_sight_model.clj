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

(ns gssm.line-of-sight-model
  (:refer-clojure)
  (:use [misc.utils     :only (euclidean-distance)]
	[gssm.model-api :only (distribute-flow!
			       service-carrier
			       distribute-load-over-processors)]))

(defn find-viewpath
  [provider beneficiary]
  (let [[pi pj] (:id provider)
	[bi bj] (:id beneficiary)
	m (if (not= pj bj) (/ (- bi pi) (- bj pj)))
	b (if m (- pi (* m pj)))
	f (fn [x] (+ (* m x) b))]
    (cond (nil? m) (map (fn [i] [i pj])
			(if (< pi bi)
			  (range pi (inc bi))
			  (range pi (dec bi) -1)))

	  (== m 0) (map (fn [j] [pi j])
			(if (< pj bj)
			  (range pj (inc bj))
			  (range pj (dec bj) -1)))

	  :otherwise (let [get-i-range
			   (cond (and (< pi bi) (< pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j pj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j bj) 0.0 0.5))))]
					   (range left-i (inc right-i))))
					       
				 (and (< pi bi) (> pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j bj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j pj) 0.0 0.5))))]
					   (range right-i (inc left-i))))
					       
				 (and (> pi bi) (< pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j pj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j bj) 0.0 0.5))))]
					   (range left-i  (dec right-i) -1)))
					       
				 (and (> pi bi) (> pj bj))
				 (fn [j] (let [left-i  (Math/round (f (- j (if (== j bj) 0.0 0.5))))
					       right-i (Math/round (f (+ j (if (== j pj) 0.0 0.5))))]
					   (range right-i (dec left-i)  -1))))
			   j-range (if (< pj bj)
				     (range pj (inc bj))
				     (range pj (dec bj) -1))]
		       (for [j j-range i (get-i-range j)] [i j])))))

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
    (find-viewpath p b)))

(defn get-valid-elevation
  [location]
  (let [elev ((:flow-features location) "Altitude")]
    (if (> elev 55530.0) 0.0 elev)))

(defn distribute-raycast!
  [provider beneficiary decay-rate location-map]
;;  (println "Casting from" (:id provider))
  (let [source-val (force (:source provider))]
    (if (= provider beneficiary)
;;      (do (println "Provider = Beneficiary")
      (swap! (:carrier-cache beneficiary) conj
	     (struct service-carrier source-val [provider]))
      (let [source-elev (get-valid-elevation provider)
	    rise  (- (get-valid-elevation beneficiary) source-elev)
	    run   (euclidean-distance (:id provider) (:id beneficiary))
	    m     (/ rise run)
	    f     (fn [x] (+ (* m x) source-elev))
	    path  (find-viewpath provider beneficiary)
	    steps (dec (count path))
	    dx    (/ run steps)]
;;	(println "Path steps:" steps)
	(loop [step 0
	       traversed-locs []
	       untraversed-ids path]
	  (when (seq untraversed-ids)
	    (let [current-loc (location-map (first untraversed-ids))]
	      (if (or (> (force (:sink current-loc)) 0.14)
		      (== step steps))
;;		(do (println "Hit a Sink/Use Location")
		(swap! (:carrier-cache current-loc) conj
		       (struct service-carrier
			       (* source-val (Math/pow decay-rate (* step dx)))
			       (conj traversed-locs current-loc))))
	      (let [current-elev (get-valid-elevation current-loc)
		    view-elev    (f (* step dx))]
;;		(println "Current elev:" current-elev)
;;		(println "View elev:   " view-elev)
		(when (<= current-elev view-elev)
;;		  (println "Recurring (STEP" (inc step) ")")
		  (recur (inc step)
			 (conj traversed-locs current-loc)
			 (rest untraversed-ids)))))))))))

(defn distance-within-range?
  [provider beneficiary decay-rate trans-threshold]
  (let [source-val       (force (:source provider))
	path-length      (euclidean-distance (:id provider) (:id beneficiary))
	asset-propagated (* source-val (Math/pow decay-rate path-length))]
    (> asset-propagated trans-threshold)))

(defmethod distribute-flow! "LineOfSight"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (println "Global LineOfSight Model begins...")
  (let [locations     (vals location-map)
        providers     (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries (filter #(> (force (:use %)) 0.0) locations)]
    (println "Num Providers:" (count providers))
    (println "Num Beneficiaries:" (count beneficiaries))
    (doseq [p (take 270 providers)]
	(doseq [b beneficiaries]
	    (if (distance-within-range? p b decay-rate trans-threshold)
;;	      (do (println "PATH IN RANGE")
	      (distribute-raycast! p b decay-rate location-map))))))
;;	      (println "PATH OUT OF RANGE"))))))

(comment

(defmethod distribute-flow! "LineOfSight_Foo"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (let [locations     (vals location-map)
        providers     (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries (filter #(> (force (:use %)) 0.0) locations)]
    (distribute-load-over-processors
     (fn [_ viewpath] (distribute-raycast! viewpath decay-rate))
     (filter #(let [path-length (dec (count %))]
                (and (distance-within-range? % path-length decay-rate trans-threshold)
                     (no-elevation-interference? % path-length)))
             (map #(map location-map %)
                  (find-viewpaths providers beneficiaries))))))

(defmethod distribute-flow! "LineOfSight_Bar"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (println "LineOfSight Model begins...")
  (let [locations      (vals location-map)
        providers      (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries  (filter #(> (force (:use %)) 0.0) locations)
	viewpaths      (map #(map location-map %) (find-viewpaths providers beneficiaries))
	path-info      (map #(let [path-length (dec (count %))]
;;;			       (println "Assessing next path " (:id (first %))
;;;					"->" (:id (last %)) "(length " path-length ")")
			       (let [range? (distance-within-range? % path-length
								    decay-rate trans-threshold)
				     elev?  (if range?
					      (no-elevation-interference? % path-length))]
;;;					      (println "-- OUT OF RANGE"))]
;;;				 (println "Building hash-map...")
				 (let [hmap (hash-map
					     :path   %
					     :steps  path-length
					     :range? range?
					     :elev?  elev?)]
;;;				   (println "Built hash-map")
				   hmap)))
			    (take 3000 viewpaths))]
    (println "Total Viewpaths Computed: " (count viewpaths))
    (let [good-path?     #(and (:range? %) (:elev? %))
	  good-path-info (filter good-path? path-info)
	  bad-path-info  (filter (complement good-path?) path-info)]
      (println "Total Viewpaths Accepted: " (count good-path-info))
      (println "Total Viewpaths Rejected: " (count bad-path-info))
      (println "Propagating carriers...")
      (println "Results: " (map (fn [p] (distribute-raycast! p decay-rate)) (map :path good-path-info)))
      (println "Done!"))))
;;      (distribute-load-over-processors
;;       (fn [_ viewpath] (distribute-raycast! viewpath decay-rate))
;;       (map :path good-path-info))
;;      (println "Distribute-Load Returned."))))

(defmethod distribute-flow! "LineOfSight_Baz"
  [_ {:keys [decay-rate trans-threshold]} location-map _ _]
  (println "LineOfSight Model begins...")
  (let [locations      (vals location-map)
        providers      (filter #(> (force (:source %)) trans-threshold) locations)
        beneficiaries  (filter #(> (force (:use %)) 0.0) locations)
	viewpath-ids   (find-viewpaths providers beneficiaries)
	viewpaths      (map #(map location-map %) viewpath-ids)]
;;    (println "Num Locations:     " (count locations))
;;    (println "Num Providers:     " (count providers))
;;    (println "Num Beneficiaries: " (count beneficiaries))
;;    (println "Provider I Min:    " (apply min (map (comp first :id) providers)))
;;    (println "Provider I Max:    " (apply max (map (comp first :id) providers)))
;;    (println "Provider J Min:    " (apply min (map (comp second :id) providers)))
;;    (println "Provider J Max:    " (apply max (map (comp second :id) providers)))
;;    (println "Beneficiary I Min: " (apply min (map (comp first :id) beneficiaries)))
;;    (println "Beneficiary I Max: " (apply max (map (comp first :id) beneficiaries)))
;;    (println "Beneficiary J Min: " (apply min (map (comp second :id) beneficiaries)))
;;    (println "Beneficiary J Max: " (apply max (map (comp second :id) beneficiaries)))
    (println "Num Viewpath-ids:  " (count viewpath-ids))
    (println "Num Viewpaths:     " (count viewpaths))
;;    (println "Some Providers:    " (take 11 (map :id providers)))
;;    (println "Some Beneficiaries:" (take 26 (map :id beneficiaries)))
;;    (newline)
;;    (println "Id-seq  2691: " (nth viewpath-ids 2690))
;;    (newline)
;;    (println "Loc-seq 2691: " (map :id (nth viewpaths 2690)))))
))

)