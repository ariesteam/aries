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
;;; This namespace defines functions for rerunning the carrier routes
;;; captured from the first pass of the SPAN flow simulation.  The
;;; routes are processed this time with sink and use effects taken
;;; into account.  All the newly computed carrier route information is
;;; cached via memoization for use by the analyzer functions.

(ns clj-span.route-caching
  (:use [clj-span.analyzer :only (rerun-actual-route sink-loc? use-loc?)]
	[clj-span.params   :only (*sink-type* *use-type* *benefit-type*)]))

(defn- upstream-dependent-carriers
  [carrier dependency?]
  (some #(and (dependency? %) @(:carrier-cache %))
	(rest (rseq (:route carrier)))))

(defn- order-carriers-by-dependence
  "This should be a topological sort."
  [carriers]
  (if (and (= *sink-type* :relative)
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
	     open-list   (list (first carriers))]
	(if (empty? open-list)
	  (if (empty? unordered)
	    ordered
	    (recur unordered
		   ordered
		   (list (first unordered))))
	  (let [c          (first open-list)
		successors (filter unordered (upstream-dependent-carriers c dependency?))]
	    (if (nil? successors)
	      (recur (disj unordered c)
		     (conj ordered c)
		     (rest open-list))
	      (recur unordered
		     ordered
		     (concat successors open-list)))))))))

(defn cache-all-actual-routes!
  [locations flow-model]
  (println "Computing actual routes from possible routes...")
  (let [carriers (doall (mapcat (comp deref :carrier-cache) (filter #(or (sink-loc? %) (use-loc? %)) locations)))]
    (println "Ordering carriers by dependence...")
    (let [sorted-carriers (order-carriers-by-dependence carriers)]
      (println "Rerunning routes by dependence order...")
      (doseq [c sorted-carriers]
	(rerun-actual-route c flow-model))
      (println "All runs completed."))))
