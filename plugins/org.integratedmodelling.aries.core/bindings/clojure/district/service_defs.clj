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
  (:refer-clojure)
  (:use [district.discretization :only (undiscretization-table)]))

(defn expected-value
  [concept-name distribution]
  (reduce + (map (fn [[state prob]]
		   (let [transformer (undiscretization-table concept-name)
			 undiscretized-val (transformer state)]
		     (cond (Double/isNaN undiscretized-val)
			   (throw (Exception. (str "Undiscretized Value for" concept-name
						   "/" state "is NaN:" undiscretized-val)))
			   (Double/isNaN prob)
			   (throw (Exception. (str "Probability Value for" concept-name
						   "/" state "is NaN:" undiscretized-val))))
		     (* undiscretized-val prob)))
		 distribution)))

(defn source-val
  "Expected amount of service carrier provision."
  [benefit-source-name source-inference-engine source-features]
  (let [inference-results (aries/run-inference (aries/set-evidence source-inference-engine source-features))
	source-distribution (aries/get-marginals-table inference-results benefit-source-name)]
    (expected-value benefit-source-name source-distribution)))

(defmulti
  #^{:doc "Service-specific flow distribution function."}
  distribute-flow (fn [benefit-sink-name sink-features neighbor-sink-features flow-amount]
		    benefit-sink-name))

(defmethod distribute-flow :default [benefit-sink-name sink-features neighbor-sink-features flow-amount]
  (let [num-neighbors (count neighbor-sink-features)
	amt (/ flow-amount num-neighbors)]
    (replicate num-neighbors amt)))

(defmethod distribute-flow :default-old [benefit-sink-name _ _ _]
  (throw (Exception. (str "Service " benefit-sink-name " is unrecognized."))))

(defstruct flows :use :sink :consume :out)

(defn compute-flows
  "Returns a flows map specifying the distribution of a service
   carrier's weight between being used, sunk, consumed, or propagated
   on to neighboring locations."
  [benefit-sink-name sink-inference-engine sink-features neighbor-sink-features]
  (let [inference-results (aries/run-inference (aries/set-evidence sink-inference-engine sink-features))
	sink-distribution (aries/get-marginals-table inference-results benefit-sink-name)]
    (struct-map flows
      :use     (aries/get-marginals inference-results "NondestructiveUse" "NondestructiveUse")
      :sink    (get sink-distribution "Sink")
      :consume (get sink-distribution "DestructiveUse")
      :out     (distribute-flow benefit-sink-name sink-features neighbor-sink-features
				(get sink-distribution "NoUse")))))
