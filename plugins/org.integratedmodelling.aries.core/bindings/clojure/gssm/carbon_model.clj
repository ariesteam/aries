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
  (:use [misc.utils     :only (seq2map)]
	[misc.stats     :only (rv-zero? rv-add rv-mult rv-div scalar-rv-mult rv-scalar-div scalar-rv-div)]
	[gssm.model-api :only (distribute-flow! service-carrier distribute-load-over-processors)]
	[gssm.analyzer  :only (source-loc? use-loc?)]))

(defmethod distribute-flow! "Carbon"
  [_ location-map _ _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative :use
   values."
  (let [locations                 (vals location-map)
	{s :source u :use}        (select-keys (first locations) [:source :use])
	[add-sources div-sources] (if (map? s) [rv-add rv-div]   [+ /])
	[add-uses zero-use?]      (if (map? u) [rv-add rv-zero?] [+ zero?])
	[div-source-by-use mult-use-by-ratio mult-percent-by-use]
	(if (map? s)
	  (if (map? u) [rv-div        rv-mult rv-mult]        [rv-scalar-div scalar-rv-mult rv-mult])
	  (if (map? u) [scalar-rv-div rv-mult scalar-rv-mult] [/             *              *]))
	source-locations          (filter source-loc? locations)
	use-locations             (filter use-loc?    locations)
	total-production          (reduce add-sources (map :source source-locations))
	total-consumption         (reduce add-uses    (map :use    use-locations))]
    (when-not (zero-use? total-consumption)
      (let [production-consumption-ratio (div-source-by-use total-production total-consumption)
	    producer-percentages         (seq2map source-locations
						  #(vector % (div-sources (:source %) total-production)))
	    consumer-units               (seq2map use-locations
						  #(vector % (mult-use-by-ratio (:use %) production-consumption-ratio)))
	    action-fn                    (fn [_ c]
					   ;;(reset! (:carrier-cache c) ; FIXME upgrade clojure!
					   (swap! (:carrier-cache c) (constantly
								      (for [p source-locations]
									(struct service-carrier
										(mult-percent-by-use
										 (producer-percentages p)
										 (consumer-units c))
										[p c])))))]
	(distribute-load-over-processors action-fn use-locations)))))
