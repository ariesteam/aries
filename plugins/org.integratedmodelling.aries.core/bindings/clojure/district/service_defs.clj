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

(defmethod source-val :default [benefit features]
  100.0)

(defstruct flows :sink :use :consume :out)

(defmulti
  #^{:doc "Returns a flows map specifying the distribution of a
           service carrier's weight between being sunk, used,
           consumed, or propagated on to neighboring locations."}
  compute-flows (fn [benefit features neighbor-features] benefit))

(defmethod compute-flows :default [benefit features neighbor-features]
  (let [num-neighbors (count neighbor-features)
	outval (/ 0.8 num-neighbors)]
    (struct-map flows
      :sink 0.1
      :use 0.1
      :consume 0.1
      :out (replicate num-neighbors outval))))

