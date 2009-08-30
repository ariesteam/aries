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

(ns gssm.actualizer
  (:refer-clojure)
  (:use [gssm.analyzer :only (rerun-actual-route sink-loc? use-loc?)]
	[gssm.params   :only (*sink-type* *use-type* *benefit-type*)]))

(defn- upstream-dependent-carriers
  [carrier dependency?]
  ;;(println "Finding upstream carriers for:" (:id (peek (:route carrier))))
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
      ;;(println "Building a set of unordered carriers...")
      (loop [unordered   (set carriers)
	     ordered     []
	     open-list   (list (first carriers))]
	;;(println "Unordered set size:" (count unordered))
	;;(println "Ordered list size: " (count ordered))
	;;(println "Open list size:    " (count open-list))
	(if (empty? open-list)
	  (do
	    ;;(println "OPEN LIST is empty. Tree completed.")
	    (if (empty? unordered)
	      (do
		;;(println "All trees completed.")
		ordered)
	      (do
		;;(println "Beginning new tree.")
		(recur unordered
		       ordered
		       (list (first unordered))))))
	  (do
	    ;;(println "Beginning tree descent.")
	    ;;(println "Continue? ")(read)
	    (let [c          (first open-list)
		  successors (filter unordered (upstream-dependent-carriers c dependency?))]
	      ;;(println "Num successors:" (count successors))
	      ;;(println "Continue? ")(read)
	      (if (nil? successors)
		(recur (disj unordered c)
		       (conj ordered c)
		       (rest open-list))
		(recur unordered
		       ordered
		       (concat successors open-list))))))))))

(defn cache-all-actual-routes!
  [locations]
  (println "Computing actual routes from possible routes...")
  (let [carriers (apply concat (map (comp deref :carrier-cache) (filter #(or (sink-loc? %) (use-loc? %)) locations)))]
    (println "Total carriers:" (count carriers))
    (println "Distinct carriers:" (count (distinct carriers)))
    (println "Ordering carriers by dependence...")
    (let [sorted-carriers (order-carriers-by-dependence carriers)]
      (println "Rerunning routes by dependence order...")
      (doseq [c sorted-carriers]
	(rerun-actual-route c)))))
