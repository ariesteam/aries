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

(ns span.location-builder
  (:refer-clojure)
  (:use [misc.utils        :only (maphash seq2map)]
	[misc.stats        :only (successive-sums)]
	[misc.matrix-ops   :only (get-neighbors)]))
(refer 'corescience :only '(map-dependent-states))
(refer 'modelling   :only '(probabilistic? get-dist-breakpoints get-possible-states get-probabilities get-data))

(defstruct location :id :neighbors :source :sink :use :flow-features :carrier-cache)

(defn unpack-datasource
  "Returns a seq of length n of the values in ds,
   represented as probability distributions."
  [ds n]
  (if (probabilistic? ds)
    (try ; ranged continuous distributions
     (let [bounds                (get-dist-breakpoints ds)
	   unbounded-from-below? (= Double/NEGATIVE_INFINITY (first bounds))
	   prob-dist             (apply create-struct (if unbounded-from-below? (rest bounds) bounds))]
       (if unbounded-from-below?
	 (map (fn [idx] (apply struct prob-dist (successive-sums (get-probabilities ds idx)))) (range n))
	 (map (fn [idx] (apply struct prob-dist (successive-sums 0 (get-probabilities ds idx)))) (range n))))
     (catch Exception e ; discrete distributions
       (let [states    (get-possible-states ds)
	     prob-dist (apply create-struct states)]
	 (map (fn [idx] (apply struct prob-dist (get-probabilities ds idx))) (range n)))))
    (map #(array-map % 1.0) (get-data ds)))) ; deterministic values

(defn- extract-values-by-concept
  "Returns a seq of the concept's values in the observation,
   which are doubles or probability distributions."
  [obs conc n]
  (unpack-datasource ((map-dependent-states obs) conc) n))

(defn- extract-all-values
  "Returns a map of concepts to vectors of doubles or probability
   distributions."
  [obs n]
  (maphash identity #(vec (unpack-datasource % n)) (map-dependent-states obs)))

(defn make-location-map
  "Returns a map of ids to location objects, one per location in the
   observation set."
  [source-conc source-obs sink-conc sink-obs use-conc use-obs flow-obs rows cols]
  (let [n             (* rows cols)
	flow-vals-map (extract-all-values flow-obs n)]
    (seq2map
     (map (fn [[i j] source sink use]
	    (struct-map location
	      :id            [i j]
	      :neighbors     (vec (get-neighbors [i j] rows cols))
	      :source        source
	      :sink          sink
	      :use           use
	      :flow-features (maphash identity #(% (+ (* i cols) j)) flow-vals-map)
	      :carrier-cache (atom ()))) ;; FIXME make carrier-cache into a [] to save memory
	  (for [i (range rows) j (range cols)] [i j])
	  (extract-values-by-concept source-obs source-conc n)
	  (extract-values-by-concept sink-obs   sink-conc   n)
	  (extract-values-by-concept use-obs    use-conc    n))
     (fn [loc] [(:id loc) loc]))))
