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

(ns span.flow-model
  (:use [misc.utils            :only (maphash)]
	[span.location-builder :only (make-location-map)]
	[span.params           :only (set-global-params! *downscaling-factor*)]
	[span.model-api        :only (distribute-flow!)]
	[span.actualizer       :only (cache-all-actual-routes!)])
  (:require span.water-model
	    span.carbon-model
	    span.proximity-model
	    span.line-of-sight-model))
(refer 'geospace :only '(get-spatial-extent))

(defn- load-params!
  []
  (with-open [foo (java.io.PushbackReader. (java.io.FileReader. "/tmp/span-flow-params.txt"))]
    (binding [*in* foo]
      (set-global-params! (read)))))

(defn- store-data
  [r c fcn lm]
  (with-open [foo (java.io.FileWriter. "/tmp/span-location-data.txt")]
    (binding [*out*        foo
	      *print-meta* true]
      (prn r c fcn lm))))

(defn- load-data
  []
  (with-open [foo (java.io.PushbackReader. (java.io.FileReader. "/tmp/span-location-data.txt"))]
    (binding [*in* foo]
      [(read) (read) (read) (read)])))

(defn- add-atoms-to
  [location-map]
  (maphash identity #(assoc % :carrier-cache (atom ())) location-map))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which collect information about the routes traveled and
   the service weight transmitted along the route.  When the
   simulation completes, a sequence of the locations in the network is
   returned."
  [observation source-conc sink-conc use-conc flow-conc flow-conc-name rows cols]
  (let [scaled-rows (int (/ rows *downscaling-factor*))
	scaled-cols (int (/ cols *downscaling-factor*))
	extent      (get-spatial-extent observation)
	lm (make-location-map observation
			      source-conc
			      sink-conc
			      use-conc
			      flow-conc
			      rows
			      cols
			      scaled-rows
			      scaled-cols)
	location-map (add-atoms-to lm)
	locations    (vals location-map)]
    ;;(store-data rows cols flow-conc-name lm)
    ;;(throw (Exception. "Exiting early."))
    (distribute-flow! flow-conc-name location-map scaled-rows scaled-cols)
    (cache-all-actual-routes! locations flow-conc-name)
    locations))

(defn ssf
  []
  (load-params!)
  (let [[rows cols flow-conc-name lm] (load-data)
	location-map (add-atoms-to lm)
	locations    (vals location-map)]
    (distribute-flow! flow-conc-name location-map rows cols)
    (cache-all-actual-routes! locations flow-conc-name)
    (println "Locs:         " (count locations))
    (println "Filled Caches:" (count (filter #(not (empty? @(:carrier-cache %))) locations)))))
