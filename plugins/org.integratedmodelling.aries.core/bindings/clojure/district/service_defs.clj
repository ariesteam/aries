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
  [concept-name distribution source-features]
  (reduce + (map (fn [[state prob]]
		   (let [transformer (undiscretization-table concept-name)
			 undiscretized-val (transformer state)]
;		     (if (Double/isNaN prob)
;		       (throw (Exception. (str "Probability Value for " concept-name
;					       "/" state " is NaN: " prob
;					       ".\nTable says: " (seq distribution)
;					       "\nEvidence was: " source-features))))
		     (if (Double/isNaN prob) 0.0 (* undiscretized-val prob))))
		 distribution)))

(defn source-val
  "Expected amount of service carrier provision."
  [benefit-source-name source-inference-engine source-features]
  (let [inference-results (aries/run-inference (aries/set-evidence source-inference-engine source-features))
	source-distribution (aries/get-marginals-table inference-results benefit-source-name)]
    (expected-value benefit-source-name source-distribution source-features)))

(defstruct flows :use :sink :consume :out)

(defn compute-flows
  "Returns a flows map specifying the distribution of a service
   carrier's weight between being used, sunk, consumed, or propagated
   on to neighboring locations."
  [benefit-sink-name sink-inference-engine sink-features]
  (let [inference-results (aries/run-inference (aries/set-evidence sink-inference-engine sink-features))
	sink-distribution (aries/get-marginals-table inference-results benefit-sink-name)]
    (struct-map flows
      :use      (let [use (aries/get-marginals inference-results "NonDestructiveUse" "Yes")]
		  (if (Double/isNaN use) 0.0 use))
      :sink     (let [sink (get sink-distribution "Sink")]
		  (if (Double/isNaN sink) 0.0 sink))
      :consume  (let [consume (get sink-distribution "DestructiveUse")]
		  (if (Double/isNaN consume) 0.0 consume))
      :out      (let [out (get sink-distribution "NoUse")]
		  (if (Double/isNaN out) 0.0 out)))))
