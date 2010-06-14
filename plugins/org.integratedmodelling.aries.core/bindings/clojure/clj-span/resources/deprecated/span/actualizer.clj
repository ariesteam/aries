;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.actualizer
  (:use [clojure.set :as set :only (difference)]
	[span.analyzer :only (rerun-actual-route sink-loc? use-loc?)]
	[span.params   :only (*sink-type* *use-type* *benefit-type*)]))

(defn- upstream-dependent-carriers
  [carrier dependency?]
  (some #(and (dependency? %) @(:carrier-cache %))
	(rest (rseq (:route carrier)))))

(defn- order-carriers-by-dependence
  "This should be a topological sort."
  [carriers]
  (println "Here we go!")
  (if (and (#{:relative nil} *sink-type*)
	   (or (= *use-type* :relative)
	       (= *benefit-type* :non-rival)))
    carriers ; carriers do not need to be sorted as they are all independent
    (let [dependency? (if (= *sink-type* :absolute)
			(if (= *use-type* :absolute)
			  (if (= *benefit-type* :rival)
			    #(or (sink-loc? %) (use-loc? %))
			    sink-loc?)
			  sink-loc?)
			(if (= *use-type* :absolute)
			  (if (= *benefit-type* :rival)
			    use-loc?
			    (constantly false))
			  (constantly false)))]
      (loop [unordered   (set carriers)
	     ordered     []
	     open-list   (list (first carriers))
	     closed-set #{}]
	(if (empty? open-list)
	  (if (empty? unordered)
	    ordered
	    (recur unordered
		   ordered
		   (list (first unordered))
		   closed-set))
	  (let [c          (first open-list)
		successors (filter (set/difference unordered closed-set) (upstream-dependent-carriers c dependency?))]
	    (if (nil? successors)
	      (recur (disj unordered c)
		     (conj ordered c)
		     (rest open-list)
		     (disj closed-set c))
	      (recur unordered
		     ordered
		     (concat successors open-list)
		     (conj closed-set c)))))))))

(defn cache-all-actual-routes!
  [locations flow-concept-name]
  (println "Computing actual routes from possible routes...")
;;  (let [carriers (doall (mapcat (comp deref :carrier-cache) (filter #(or (sink-loc? %) (use-loc? %)) locations)))]
  (println "Number of locations:" (count locations))
  (let [sink-use-locs (filter #(or (sink-loc? %) (use-loc? %)) locations)
	carriers      (mapcat (comp deref :carrier-cache) sink-use-locs)]
    (println "Carriers collected:"  (count carriers))
    (Thread/sleep 1000)
    (println "Ordering carriers by dependence...")
    (let [sorted-carriers (order-carriers-by-dependence carriers)]
      (println "Rerunning routes by dependence order...")
      (doseq [c sorted-carriers]
	(rerun-actual-route c flow-concept-name))
      (println "All runs completed."))))
