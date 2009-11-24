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

(ns gssm.flow-model
  (:refer-clojure)
  (:use [gssm.location-builder :only (make-location-map)]
	[gssm.model-api        :only (distribute-flow!)]
	[gssm.actualizer       :only (cache-all-actual-routes!)])
  (:require gssm.water-model
	    gssm.carbon-model
	    gssm.proximity-model
	    gssm.line-of-sight-model))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which collect information about the routes traveled and
   the service weight transmitted along the route.  When the
   simulation completes, a sequence of the locations in the network is
   returned."
  [source-concept source-obs
   sink-concept   sink-obs
   use-concept    use-obs
   flow-concept   flow-obs
   rows           cols]
  (let [location-map (make-location-map source-concept source-obs
					sink-concept   sink-obs
					use-concept    use-obs
					flow-obs rows cols)
	locations    (vals location-map)]
    (distribute-flow! (.getLocalName flow-concept) location-map rows cols)
    (cache-all-actual-routes! locations)
    locations))
