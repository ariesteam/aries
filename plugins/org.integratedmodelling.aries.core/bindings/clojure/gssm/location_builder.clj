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
	[misc.matrix-ops   :only (get-neighbors)]))
;;	[corescience       :only (map-dependent-states)]
;;      [modelling         :only (probabilistic? get-possible-states get-probabilities get-data)]))

(defstruct location :id :neighbors :source :sink :use :flow-features :carrier-cache)

(defn unpack-datasource
  "Returns a vector (of length rows * cols) of the values in ds,
   either doubles or probability distributions."
  [ds rows cols]
  (vec (if (modelling/probabilistic? ds)
	 (let [prob-dist (apply create-struct (modelling/get-possible-states ds))]
	   (for [idx (range (* rows cols))]
	     (apply struct prob-dist (modelling/get-probabilities ds idx))))
	 (modelling/get-data ds))))

(defn- extract-values-by-concept
  "Returns a vector of the concept's values in the observation,
   which are doubles or probability distributions."
  [obs conc rows cols]
  (unpack-datasource ((corescience/map-dependent-states obs) conc) rows cols))

(defn- extract-all-values
  "Returns a map of concepts to vectors of doubles or probability
   distributions."
  [obs rows cols]
  (maphash identity #(unpack-datasource % rows cols) (corescience/map-dependent-states obs)))

(defn make-location-map
  "Returns a map of ids to location objects, one per location in the
   observation set."
  [source-conc source-obs sink-conc sink-obs use-conc use-obs flow-obs rows cols]
  (let [source-vec   (extract-values-by-concept source-obs source-conc rows cols)
	sink-vec     (extract-values-by-concept sink-obs   sink-conc   rows cols)
	use-vec      (extract-values-by-concept use-obs    use-conc    rows cols)
	flow-vec-map (extract-all-values flow-obs rows cols)]
    (seq2map (for [i (range rows) j (range cols)]
	       (let [idx (+ (* i cols) j)]
		 (struct-map location
		   :id            [i j]
		   :neighbors     (get-neighbors [i j] rows cols)
		   :source        (source-vec idx)
		   :sink          (sink-vec   idx)
		   :use           (use-vec    idx)
		   :flow-features (maphash identity #(% idx) flow-vec-map)
		   :carrier-cache (atom ()))))
	     (fn [loc] [(:id loc) loc]))))
