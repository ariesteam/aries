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

(ns span.carbon-model
  (:refer-clojure)
  (:use [misc.utils     :only (seq2map)]
	[misc.stats     :only (rv-add rv-divide rv-multiply)]
	[span.model-api :only (distribute-flow! service-carrier distribute-load-over-processors)]
	[span.analyzer  :only (source-loc? use-loc?)]))

(defmethod distribute-flow! "Carbon"
  [_ location-map _ _]
  "The amount of carbon sequestration produced is distributed among
   the consumers (carbon emitters) according to their relative :use
   values."
  (let [locations         (vals location-map)
	source-locations  (filter source-loc? locations)
	use-locations     (filter use-loc?    locations)
	total-consumption (reduce rv-add (map :use use-locations))
	percent-consumed  (seq2map use-locations #(vector % (rv-divide (:use %) total-consumption)))]
    (distribute-load-over-processors
     (fn [_ c]
       ;;(reset! (:carrier-cache c) ; FIXME upgrade clojure!
       (swap! (:carrier-cache c) (constantly
				  (for [p source-locations]
				    (struct service-carrier
					    (rv-multiply (:source p) (percent-consumed c))
					    [p c])))))
     use-locations)))
