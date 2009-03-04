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

(defn expected-value
  [distribution]
  0.5)

(defn source-val
  "Expected amount of service carrier provision."
  [benefit-source features source-inference-engine]
  (let [inference-results (run-inference (set-evidence source-inference-engine features))
	source-distribution (get-marginals-table inference-results (.getLocalName benefit-source))]
    (expected-value source-distribution)))

(defmulti
  #^{:doc "Service-specific flow distribution function."}
  distribute-flow (fn [benefit-sink features neighbor-features flow-amount]
		    (.getLocalName benefit-sink)))

(defmethod distribute-flow :default [benefit-sink _ _ _]
  (throw (Exception. (str "Service " (.getLocalName benefit-sink) " is unrecognized."))))

(defstruct flows :use :sink :consume :out)

(defn compute-flows
  "Returns a flows map specifying the distribution of a service
   carrier's weight between being used, sunk, consumed, or propagated
   on to neighboring locations."
  [benefit-sink features neighbor-features sink-inference-engine]
  (let [inference-results (run-inference (set-evidence sink-inference-engine features))
	sink-distribution (get-marginals-table inference-results (.getLocalName benefit-sink))]
    (struct-map flows
      :use     (get-marginals inference-results "NondestructiveUse" "NondestructiveUse")
      :sink    (get sink-distribution "Sink")
      :consume (get sink-distribution "DestructiveUse")
      :out     (distribute-flow benefit-sink features neighbor-features
				(get sink-distribution "NoUse")))))
