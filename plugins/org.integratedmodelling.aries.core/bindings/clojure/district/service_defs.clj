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

(defmethod source-val :default [_ features] 100.0)

(defmethod source-val :water [_ features] (features :rainfall))

(defstruct flows :sink :use :consume :out)

(defmulti
  #^{:doc "Returns a flows map specifying the distribution of a
           service carrier's weight between being sunk, used,
           consumed, or propagated on to neighboring locations."}
  compute-flows (fn [benefit features neighbor-features] benefit))

(defmethod compute-flows :default [_ features neighbor-features]
  (let [num-neighbors (count neighbor-features)
	outval (/ 0.8 num-neighbors)]
    (struct-map flows
      :sink 0.1
      :use 0.1
      :consume 0.1
      :out (replicate num-neighbors outval))))

(defmethod compute-flows :water [_ features neighbor-features]
  (let [elevs (map :elevation neighbor-features)
	min-elev (let [e (min elevs)] (if (<= e (:elevation features)) e 0))
	num-paths (count (filter #(== min-elev %) elevs))
	path-weight (if (> num-paths 0) (/ 0.8 num-paths) 0.0)]
    (struct-map flows
      :sink 0.2
      :use 0.0
      :consume 0.0
      :out (map #(if (== min-elev %) path-weight 0.0) elevs))))
