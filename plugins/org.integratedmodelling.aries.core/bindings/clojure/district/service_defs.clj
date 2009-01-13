;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-DISTRICT.
;;;
;;; CLJ-DISTRICT is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; CLJ-DISTRICT is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-DISTRICT.  If not, see
;;; <http://www.gnu.org/licenses/>.

(ns district.service-defs
  (:refer-clojure))

(defmulti
  #^{:doc "Expected units of service carrier provision."}
  source-val (fn [benefit features] benefit))

(defmulti
  #^{:doc "Likelihood of destroying a unit of the service carrier without using it."}
  sink-prob  (fn [benefit features] benefit))

(defmulti
  #^{:doc "Likelihood of non-destructively using a unit of the service carrier."}
  usage-prob (fn [benefit features] benefit))

(defmulti
  #^{:doc "Likelihood of destructively using a unit of the service carrier."}
  consumption-prob (fn [benefit features] benefit))

(defmulti
  #^{:doc "Likelihood that a unit of the service carrier will travel from src to dest."}
  transition-prob  (fn [benefit src-features dest-features] benefit))


;;; Flood Prevention Functions

(defmethod source-val 'FloodPreventionBenefit [benefit features]
  "")

(defmethod sink-prob 'FloodPreventionBenefit [benefit features]
  "")

(defmethod usage-prob 'FloodPreventionBenefit [benefit features]
  "")

(defmethod consumption-prob 'FloodPreventionBenefit [benefit features]
  "")

(defmethod transition-prob 'FloodPreventionBenefit [benefit features]
  "")


;;; Climate Stability Functions

(defmethod source-val 'ClimateStabilityBenefit [benefit features]
  "")

(defmethod sink-prob 'ClimateStabilityBenefit [benefit features]
  "")

(defmethod usage-prob 'ClimateStabilityBenefit [benefit features]
  "")

(defmethod consumption-prob 'ClimateStabilityBenefit [benefit features]
  "")

(defmethod transition-prob 'ClimateStabilityBenefit [benefit features]
  "")
