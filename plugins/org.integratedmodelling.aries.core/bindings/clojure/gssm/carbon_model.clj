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

(ns gssm.carbon-model
  (:refer-clojure)
  (:use [misc.utils     :only (seq2map maphash)]
	[gssm.model-api :only (distribute-flow! service-carrier)]
	[gssm.analyzer  :only (source-loc? use-loc?)]))

(defmethod distribute-flow! "Carbon"
  [_ _ location-map _ _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative :use
   values."
  (let [locations         (vals location-map)
	source-locations  (filter source-loc? locations)
	use-locations     (filter use-loc? locations)
	total-production  (reduce + (map (comp force :source) source-locations))
	total-consumption (reduce + (map (comp force :use)    use-locations))]
    (when (> total-consumption 0.0)
      (let [production-consumption-ratio (/ total-production total-consumption)
	    producer-percentages (seq2map source-locations
					  #(vector % (/ (force (:source %)) total-production)))
	    consumer-units       (seq2map use-locations
					  #(vector % (* (force (:use %)) production-consumption-ratio)))]
	(doseq [p (keys producer-percentages)]
	    (doseq [c (keys consumer-units)]
		(swap! (:carrier-cache c) conj
		       (struct service-carrier
			       (* (producer-percentages p) (consumer-units c))
			       [p c]))))))))

(defmethod distribute-flow! "Carbon-Uneven"
  [_ _ location-map _ _]
  (let [locations        (vals location-map)
	source-locations (filter source-loc? locations)
	use-locations    (filter use-loc? locations)]
    (loop [producer-map (seq2map source-locations #(vector % (force (:source %))))
	   consumer-map (seq2map use-locations #(vector % (force (:use %))))]
      (when (seq producer-map)
	(if (seq consumer-map)
	  (let [num-producers     (count producer-map)
		num-consumers     (count consumer-map)
		min-extra-credits (apply min (vals producer-map))
		min-unmet-demand  (apply min (vals consumer-map))
		[credits-allocable max-allocable-increment]
		(if (< (* num-producers min-extra-credits)
		       (* num-consumers min-unmet-demand))
		  [min-extra-credits (/ min-extra-credits num-consumers)]
		  [min-unmet-demand  (/ min-unmet-demand  num-producers)])]
	    (doseq [p (keys producer-map)]
		(doseq [c (keys consumer-map)]
		    (swap! (:carrier-cache c) conj
			   (struct service-carrier
				   max-allocable-increment
				   [p c]))))
	    (recur (filter (fn [[k v]] (> v 0.0))
			   (maphash identity #(- % credits-allocable)) producer-map)
		   (filter (fn [[k v]] (> v 0.0))
			   (maphash identity #(- % credits-allocable)) consumer-map)))
	  (let [consumers     use-locations
		num-consumers (count consumers)]
	    (doseq [p (keys producer-map)]
		(let [max-allocable-increment (/ (producer-map p) num-consumers)]
		  (doseq [c consumers]
		      (swap! (:carrier-cache c) conj
			     (struct service-carrier
				     max-allocable-increment
				     [p c])))))))))))
