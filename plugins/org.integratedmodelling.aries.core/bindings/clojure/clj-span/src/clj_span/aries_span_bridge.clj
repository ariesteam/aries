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
;;; This namespace defines the span-driver function which takes in a
;;; Thinklab observation, several SPAN-related concepts, and a map of
;;; flow-params, extracts the SPAN source, sink, use, and flow layers
;;; from the observation and passes everything on to
;;; clj-span.core/run-span.

(ns clj-span.aries-span-bridge
  (:use [clj-span.core           :only (run-span)]
        [clj-span.sediment-model :only (aggregate-flow-dirs)]
        [clj-misc.matrix-ops     :only (seq2matrix resample-matrix)]
        [clj-misc.utils          :only (mapmap remove-nil-val-entries constraints-1.0 p &)]
        [clj-misc.randvars       :only (cont-type disc-type successive-sums)]))

#_(refer 'thinklab :only '(conc))

#_(refer 'geospace :only '(grid-rows
                           grid-columns
                           grid-extent?
                           get-shape
                           get-spatial-extent))

#_(refer 'corescience :only '(find-state
                              find-observation
                              get-state-map
                              get-observable-class))

#_(refer 'modelling   :only '(probabilistic?
                              binary?
                              encodes-continuous-distribution?
                              get-dist-breakpoints
                              get-possible-states
                              get-probabilities
                              get-data
                              run-at-shape))

(declare conc
         grid-rows
         grid-columns
         grid-extent?
         get-shape
         get-spatial-extent
         find-state
         find-observation
         get-state-map
         get-observable-class
         probabilistic?
         binary?
         encodes-continuous-distribution?
         get-dist-breakpoints
         get-possible-states
         get-probabilities
         get-data
         run-at-shape)

(defn- unpack-datasource
  "Returns a seq of length n of the values in ds,
   represented as probability distributions.  All values and
   probabilities are represented as rationals."
  [ds rows cols]
  (let [n            (* rows cols)
        to-rationals (p map #(if (Double/isNaN %) 0 (rationalize %)))]
    (if (and (probabilistic? ds) (not (binary? ds)))
      (if (encodes-continuous-distribution? ds)
        ;; sampled continuous distributions (FIXME: How is missing information represented?)
        (let [bounds                (get-dist-breakpoints ds)
              unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
              unbounded-from-above? (== Double/POSITIVE_INFINITY (last bounds))]
          (let [prob-dist             (apply create-struct (to-rationals
                                                            (if unbounded-from-below?
                                                              (if unbounded-from-above?
                                                                (rest (butlast bounds))
                                                                (rest bounds))
                                                              (if unbounded-from-above?
                                                                (butlast bounds)
                                                                bounds))))
                get-cdf-vals          (if unbounded-from-below?
                                        (if unbounded-from-above?
                                          (& successive-sums to-rationals butlast (p get-probabilities ds))
                                          (& successive-sums to-rationals (p get-probabilities ds)))
                                        (if unbounded-from-above?
                                          (& (p successive-sums 0) to-rationals butlast (p get-probabilities ds))
                                          (& (p successive-sums 0) to-rationals (p get-probabilities ds))))]
            (for [idx (range n)]
              (with-meta (apply struct prob-dist (get-cdf-vals idx)) cont-type))))
        ;; discrete distributions (FIXME: How is missing information represented? Fns aren't setup for non-numeric values.)
        (let [prob-dist (apply create-struct (get-possible-states ds))]
          (for [idx (range n)]
            (with-meta (apply struct prob-dist (to-rationals (get-probabilities ds idx))) disc-type))))
      ;; binary distributions and deterministic values (FIXME: NaNs become 0s currently. Is this good?)
      (for [value (to-rationals (get-data ds))]
        (with-meta (array-map value 1) disc-type)))))

(defn- layer-from-observation
  "Builds a rows x cols matrix (vector of vectors) of the concept's
   state values in the observation."
  [observation concept rows cols]
  (when concept
    (seq2matrix rows cols (unpack-datasource (find-state observation concept) rows cols))))

(defn- layer-map-from-observation
  "Builds a map of {concept-names -> matrices}, where each concept's
   matrix is a rows x cols vector of vectors of the concept's state
   values in the observation."
  [observation concept rows cols]
  (when concept
    (mapmap (memfn getLocalName)
            #(seq2matrix rows cols (unpack-datasource % rows cols))
            (get-state-map (find-observation observation concept)))))

(defn- get-hydrosheds-layer
  [observation rows cols]
  (let [hydrosheds-observation  (run-at-shape "aries/flood/flow-direction"
                                              (get-shape (get-spatial-extent observation)))
        hydrosheds-native-rows  (grid-rows    hydrosheds-observation)
        hydrosheds-native-cols  (grid-columns hydrosheds-observation)
        hydrosheds-native-layer (layer-from-observation hydrosheds-observation
                                                        (conc 'geophysics:FlowDirection)
                                                        hydrosheds-native-rows
                                                        hydrosheds-native-cols)]
    (resample-matrix rows cols aggregate-flow-dirs hydrosheds-native-layer)))

(defn span-driver
  "Takes the source, sink, use, and flow concepts along with the
   flow-params map and an observation containing the concepts'
   dependent features, calculates the SPAN flows, and returns the
   results using one of the following result-types: :cli-menu
   :closure-map"
  [observation source-concept use-concept sink-concept flow-concept
   {:keys [source-threshold sink-threshold use-threshold trans-threshold
           rv-max-states downscaling-factor source-type sink-type use-type benefit-type result-type]
    :or {result-type :closure-map}}]
  ;; This version of SPAN only works for grid-based observations (i.e. raster maps).
  (constraints-1.0 {:pre [(grid-extent? observation)]})
  (let [rows         (grid-rows    observation)
        cols         (grid-columns observation)
        flow-model   (.getLocalName (get-observable-class observation))
        source-layer (layer-from-observation observation source-concept rows cols)
        sink-layer   (layer-from-observation observation sink-concept   rows cols)
        use-layer    (layer-from-observation observation use-concept    rows cols)
        flow-layers  (let [layer-map (layer-map-from-observation observation flow-concept rows cols)]
                       (if (= flow-model "Sediment")
                         (assoc layer-map "Hydrosheds" (get-hydrosheds-layer observation rows cols))
                         layer-map))]
    (run-span (remove-nil-val-entries
               {:source-layer       source-layer
                :source-threshold   source-threshold
                :sink-layer         sink-layer
                :sink-threshold     sink-threshold
                :use-layer          use-layer
                :use-threshold      use-threshold
                :flow-layers        flow-layers
                :trans-threshold    trans-threshold
                :rv-max-states      rv-max-states
                :downscaling-factor downscaling-factor
                :source-type        source-type
                :sink-type          sink-type
                :use-type           use-type
                :benefit-type       benefit-type
                :flow-model         flow-model
                :result-type        result-type}))))
