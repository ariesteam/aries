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
	[gssm.analyzer         :only (cache-all-actual-routes!)])
  (:require gssm.water-model
	    gssm.carbon-model
	    gssm.proximity-model
	    gssm.line-of-sight-model))

(defn simulate-service-flows
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which all update properties of the locations.  When the
   simulation completes, a vector containing the location network and
   its dimensions is returned."
  [source-concept source-observation
   sink-concept   sink-observation
   use-concept    use-observation
   flow-concept   flow-observation
   flow-params]
  (assert (geospace/grid-extent? source-observation))
  (let [rows (geospace/grid-rows    source-observation)
	cols (geospace/grid-columns source-observation)
	location-map (make-location-map source-concept source-observation
					sink-concept   sink-observation
					use-concept    use-observation
					flow-concept   flow-observation
					rows           cols)]
    (distribute-flow! flow-concept
		      flow-params
		      location-map
		      rows
		      cols)
    (cache-all-actual-routes! (vals location-map) flow-params)
    [location-map rows cols]))
