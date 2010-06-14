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
;;; This namespace defines the service-carrier type and the
;;; multimethods which must be implemented by each flow model
;;; specification: distribute-flow!, decay, and undecay.

(ns clj-span.model-api)

(defstruct service-carrier
  :source-id      ; starting id of this flow path
  :route          ; byte array of directions from source-id to use-id or nil
  :possible-weight; amount of source-weight which reaches (and is used by) this use location disregarding sink-effects
  :actual-weight  ; amount of source-weight which reaches (and is used by) this use location including sink-effects
  :sink-effects)  ; map of sink-ids to sink-effects on this flow path (decayed as necessary)

(defmulti distribute-flow!
  "Service-specific flow distribution functions."
  (fn [flow-model location-map rows cols] flow-model))

(defmethod distribute-flow! :default
  [flow-model _ _ _]
  (throw (Exception. (str "distribute-flow! is undefined for flow type: " flow-model))))

(defmulti distribute-flow
  "Creates a network of interconnected locations, and starts a
   service-carrier propagating in every location whose source value is
   greater than 0.  These carriers propagate child carriers through
   the network which collect information about the routes traveled and
   the service weight transmitted along these routes.  When the
   simulation completes, a sequence of the locations in the network is
   returned."
  (fn [flow-model source-layer sink-layer use-layer flow-layers] flow-model))

(defmethod distribute-flow :default
  [flow-model _ _ _ _]
  (throw (Exception. (str "distribute-flow is undefined for flow type: " flow-model))))

(defmulti decay
  "Service-specific decay functions."
  (fn [flow-model weight distance] flow-model))

(defmethod decay :default [_ weight _] weight)

(defmulti undecay
  "Service-specific inverse decay functions."
  (fn [flow-model weight distance] flow-model))

(defmethod undecay :default [_ weight _] weight)
