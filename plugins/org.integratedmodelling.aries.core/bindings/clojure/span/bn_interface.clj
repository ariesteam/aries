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

(ns span.bn-interface
  (:refer-clojure)
  (:use [span.discretizer :only (undiscretize-value)]))
;;	[aries            :only (set-evidence
;;				 run-inference
;;				 get-marginals-table)]))

(defn expected-value
  [concept-name distribution features]
  (reduce + (map (fn [[state prob]]
		   (if (Double/isNaN prob)
		     (do
		       (println (str "ERROR: NaN encountered in " concept-name ":" state ".\n"
				     "       Evidence: " features "\n"
				     "       Results: " (seq distribution)))
		       0.0)
		     (* (undiscretize-value concept-name state) prob)))
		 distribution)))

(defn run-bayes-net
  "Returns the undiscretized expected marginal value of concept-name,
   given the input features."
  [concept-name inference-engine features]
  (let [inference-results (aries/run-inference (aries/set-evidence inference-engine features))
	distribution      (aries/get-marginals-table inference-results concept-name)]
    (expected-value concept-name distribution features)))
