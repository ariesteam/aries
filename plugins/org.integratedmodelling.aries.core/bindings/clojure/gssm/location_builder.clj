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

(ns gssm.location-builder
  (:refer-clojure)
  (:use [misc.utils        :only (maphash seq2map)]
	[misc.matrix-ops   :only (get-neighbors)]
	[gssm.discretizer  :only (discretize-value)]
	[gssm.bn-interface :only (run-bayes-net)]))

(defstruct location :id :neighbors :source :sink :use :carrier-cache :flow-features)

(defn extract-features-discretized
  "Returns a map of feature names to the discretized values at i j."
  [observation-states idx]
  (seq2map observation-states
	   (fn [[feature-name values]]
	     [feature-name (discretize-value feature-name (nth values idx))])))

(defn count-distinct-states
  [observation-states]
  (maphash identity
	   (fn [vals]
	     (let [distinct-vals (distinct vals)
		   num-distinct (count distinct-vals)]
	       (if (<= num-distinct 10)
		 (for [val distinct-vals]
		   [val (count (filter #(= % val) vals))])
		 (str num-distinct " distinct values..."))))
	   observation-states))

(defn make-location-map
  "Returns a map of ids to location objects, one per location in the
   observations."
  [source-concept source-observation
   sink-concept   sink-observation
   use-concept    use-observation
   flow-concept   flow-observation
   rows           cols]
  (let [source-concept-name     (.getLocalName source-concept)
	sink-concept-name       (.getLocalName sink-concept)
	use-concept-name        (.getLocalName use-concept)
	flow-concept-name       (.getLocalName flow-concept)
	source-inference-engine (aries/make-bn-inference source-concept)
	sink-inference-engine   (aries/make-bn-inference sink-concept)
	use-inference-engine    (aries/make-bn-inference use-concept)
	source-feature-map      (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states source-observation))
	sink-feature-map        (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states sink-observation))
	use-feature-map         (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states use-observation))
	flow-feature-map        (maphash (memfn getLocalName) identity
					 (corescience/map-dependent-states flow-observation))]
    (do
      (let [location-map
	    (seq2map (for [i (range rows) j (range cols)]
		       (let [feature-idx     (+ (* i cols) j)
			     source-features (extract-features-discretized source-feature-map feature-idx)
			     sink-features   (extract-features-discretized sink-feature-map   feature-idx)
			     use-features    (extract-features-discretized use-feature-map    feature-idx)
			     flow-features   (extract-features-discretized flow-feature-map   feature-idx)]
			 (struct-map location
			   :id            [i j]
			   :neighbors     (get-neighbors [i j] rows cols)
			   :source        (delay (run-bayes-net source-concept-name
								source-inference-engine
								source-features))
			   :sink          (delay (run-bayes-net sink-concept-name
								sink-inference-engine
								sink-features))
			   :use           (delay (run-bayes-net use-concept-name
								use-inference-engine
								use-features))
			   :carrier-cache (ref ())
			   :flow-features flow-features)))
		     (fn [loc] [(:id loc) loc]))]
	(println "Rows x Cols: " rows " x " cols)
	(println "Source-Concept-Name: "   source-concept-name)
	(println "Source-Feature-States: " (count-distinct-states source-feature-map))
	(println "Sink-Concept-Name: "     sink-concept-name)
	(println "Sink-Feature-States: "   (count-distinct-states sink-feature-map))
	(println "Use-Concept-Name: "      use-concept-name)
	(println "Use-Feature-States: "    (count-distinct-states use-feature-map))
	(println "Flow-Concept-Name: "     flow-concept-name)
	(println "Flow-Feature-States: "   (count-distinct-states flow-feature-map))
	location-map))))
