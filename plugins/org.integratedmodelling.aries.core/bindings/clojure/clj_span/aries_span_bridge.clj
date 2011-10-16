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
;;; This namespace defines the span-driver function which takes a
;;; Thinklab observation or model-spec [model-name location
;;; resolution], several SPAN-related concepts, and a map of
;;; flow-params, extracts the SPAN source, sink, use, and flow layers
;;; from the observation and passes everything on to
;;; clj-span.core/run-span.

(ns clj-span.aries-span-bridge
  (:use [clj-span.core           :only (run-span)]
        [clj-misc.matrix-ops     :only (seq2matrix map-matrix)]
        [clj-misc.utils          :only (mapmap remove-nil-val-entries p & constraints-1.0 with-message successive-sums)]
        [clj-misc.randvars       :only (cont-type disc-type)]
        [clj-misc.varprop        :only (fuzzy-number fuzzy-number-from-states fuzzy-number-from-ranges _0_)])
  (:import (java.io File FileWriter FileReader PushbackReader)
           (org.integratedmodelling.corescience.implementations.datasources MemObjectContextualizedDatasource
                                                                            MemDoubleContextualizedDatasource)
           (org.integratedmodelling.corescience.literals IndexedCategoricalDistribution)))

;;(comment
  (refer 'geospace :only '(grid-rows
                           grid-columns
                           grid-extent?
                           cell-dimensions))

  (refer 'corescience :only '(find-state
                              collect-states
                              get-observable-class))

  (refer 'modelling   :only '(probabilistic?
                              binary?
                              encodes-continuous-distribution?
                              get-dist-breakpoints
                              get-possible-states
                              get-probabilities
                              get-data
                              run-at-location))
;;  )

(comment
(declare grid-rows
         grid-columns
         grid-extent?
         cell-dimensions
         find-state
         collect-states
         get-observable-class
         probabilistic?
         binary?
         encodes-continuous-distribution?
         get-dist-breakpoints
         get-possible-states
         get-probabilities
         get-data
         run-at-location)
)

(defn save-span-layers
  [filename source-layer sink-layer use-layer flow-layers cell-width cell-height value-type]
  (let [dummy-map    {:theoretical-source  (constantly {})
                      :inaccessible-source (constantly {})
                      :possible-source     (constantly {})
                      :blocked-source      (constantly {})
                      :actual-source       (constantly {})
                      :theoretical-sink    (constantly {})
                      :inaccessible-sink   (constantly {})
                      :actual-sink         (constantly {})
                      :theoretical-use     (constantly {})
                      :inaccessible-use    (constantly {})
                      :possible-use        (constantly {})
                      :blocked-use         (constantly {})
                      :actual-use          (constantly {})
                      :possible-flow       (constantly {})
                      :blocked-flow        (constantly {})
                      :actual-flow         (constantly {})}
        to-printable (if (contains? #{:randvars :varprop} value-type)
                       #(with-meta (into {} %) (meta %))
                       identity)]
    (with-open [outstream (FileWriter. filename)]
      (binding [*out*       outstream
                *print-dup* true]
        (doseq [layer [source-layer sink-layer use-layer]]
          (prn (if layer (map-matrix to-printable layer))))
        (prn (mapmap identity #(map-matrix to-printable %) flow-layers))
        (prn cell-width)
        (prn cell-height)))
    dummy-map))

(defn read-span-layers
  [filename]
  (constraints-1.0 {:pre [(.canRead (File. filename))]})
  (with-open [instream (PushbackReader. (FileReader. filename))]
    (binding [*in* instream]
      (let [source-layer (read)
            sink-layer   (read)
            use-layer    (read)
            flow-layers  (read)
            cell-width   (read)
            cell-height  (read)]
        [source-layer sink-layer use-layer flow-layers cell-width cell-height]))))

(defn NaNs-to-zero
  [doubles]
  (map #(if (Double/isNaN %) 0.0 %) doubles))

(defn get-midpoints
  [bounds]
  (map (fn [next prev] (/ (+ next prev) 2)) (rest bounds) bounds))

(defmulti unpack-datasource
  ;;"Returns a seq of length n of the values in ds, where their
  ;; representations are determined by value-type."
  (fn [value-type ds n] value-type))

(defmethod unpack-datasource :default
  [value-type _ _]
  (throw (Exception. (str "unpack-datasource is undefined for value-type: " value-type))))

(defmethod unpack-datasource :randvars
  [_ ds n]
  (println "Inside unpack-datasource!\nChecking datasource type..." ds)
  (if (and (probabilistic? ds) (not (binary? ds)))
    (do (print "It's probabilistic...")
        (flush)
        (if (encodes-continuous-distribution? ds)
          ;; sampled continuous distributions
          ;; FIXME: How is missing information represented?
          ;; FIXME: Evil hack warning! Continuous RV arithmetic is
          ;; broken so I'm going to make these all discrete
          ;; distributions which use the range midpoints as their
          ;; states.
          (do
            (println "and continuous.")
            (let [bounds                (get-dist-breakpoints ds)
                  unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
                  unbounded-from-above? (== Double/POSITIVE_INFINITY (last bounds))]
              (if (or unbounded-from-below? unbounded-from-above?)
                (throw (Exception. "All undiscretized bounds must be closed above and below.")))
              (let [prob-dist (apply create-struct (NaNs-to-zero (get-midpoints bounds)))]
                (for [idx (range n)]
                  (with-meta
                    (if-let [probs (get-probabilities ds idx)]
                      (apply struct prob-dist probs)
                      (array-map 0.0 1.0))
                    disc-type)))))
          ;; discrete distributions (FIXME: How is missing information represented? Fns aren't setup for non-numeric values.)
          (do
            (println "and discrete.")
            (let [prob-dist (apply create-struct (get-possible-states ds))]
              (for [idx (range n)]
                (with-meta
                  (if-let [probs (get-probabilities ds idx)]
                    (apply struct prob-dist probs)
                    (array-map 0.0 1.0))
                  disc-type))))))
    ;; binary distributions and deterministic values (FIXME: NaNs become 0s currently. Is this good?)
    (do (println "It's deterministic.")
        (for [value (NaNs-to-zero (get-data ds))]
          (with-meta (array-map value 1.0) disc-type)))))

(defmethod unpack-datasource :varprop
  [_ ds _]
  (println "Inside unpack-datasource!\nChecking datasource type..." ds)
  (if (nil? ds) (throw (Exception. (str "null datasource"))))
  (cond (and (.isProbabilistic ds) (.isContinuous ds))
        (do (println "It's probabilistic and continuous.")
            (let [dists                 (.getRawData ds)
                  example-dist          (first (remove nil? dists))
                  bounds                (seq (.getRanges example-dist))
                  example-probs         (seq (.getData   example-dist))
                  unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
                  unbounded-from-above? (== Double/POSITIVE_INFINITY (last bounds))]
              (if (or unbounded-from-below? unbounded-from-above?)
                (throw (Exception. "All undiscretized bounds must be closed above and below.")))
              (println "Breakpoints:  " bounds)
              (println "Example Probs:" example-probs)
              (for [#^IndexedCategoricalDistribution dist dists]
                (if dist
                  (fuzzy-number-from-ranges bounds (.getData dist))
                  _0_))))

        ;; other checks available: isCategorical isBoolean isNumeric
        :otherwise
        (do (println "It's deterministic.")
            (for [value (NaNs-to-zero (get-data ds))]
              (fuzzy-number value 0.0)))))

(defn- unpack-datasource-orig
  "Returns a seq of length n of the values in ds,
   represented as probability distributions {rationals -> doubles}.
   NaN state values are converted to 0s."
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
                                          (& successive-sums butlast (p get-probabilities ds))
                                          (& successive-sums (p get-probabilities ds)))
                                        (if unbounded-from-above?
                                          (& (p successive-sums 0.0) butlast (p get-probabilities ds))
                                          (& (p successive-sums 0.0) (p get-probabilities ds))))]
            (for [idx (range n)]
              (with-meta (apply struct prob-dist (get-cdf-vals idx)) cont-type))))
        ;; discrete distributions (FIXME: How is missing information represented? Fns aren't setup for non-numeric values.)
        (let [prob-dist (apply create-struct (get-possible-states ds))]
          (for [idx (range n)]
            (with-meta (apply struct prob-dist (get-probabilities ds idx)) disc-type))))
      ;; binary distributions and deterministic values (FIXME: NaNs become 0s currently. Is this good?)
      (for [value (to-rationals (get-data ds))]
        (with-meta (array-map value 1.0) disc-type)))))

(defn- layer-from-observation
  "Builds a rows x cols matrix (vector of vectors) of the concept's
   state values in the observation."
  [observation concept rows cols value-type]
  (when concept
    (println "Extracting" (.getLocalName concept) "layer.")
    (seq2matrix rows cols (unpack-datasource value-type (find-state observation concept) (* rows cols)))))

(defn- layer-map-from-observation
  "Builds a map of {concept-names -> matrices}, where each concept's
   matrix is a rows x cols vector of vectors of the concept's state
   values in the observation."
  [observation concepts rows cols value-type]
  (when (seq concepts)
    (println "Extracting flow layers:" (map (memfn getLocalName) concepts))
    (into {}
          (map (fn [c] [(.getLocalName c)
                        (seq2matrix rows cols (unpack-datasource value-type (find-state observation c) (* rows cols)))])
               concepts))))

(defn span-driver
  "Takes the source, sink, use, and flow concepts along with the
   flow-params map and an observation containing the concepts'
   dependent features (or model-spec [model-name location resolution]
   which produces this observation), calculates the SPAN flows, and
   returns the results using one of the following result-types:
   (:cli-menu :closure-map). If the :save-file parameter is set in the
   flow-params map, the SPAN model will not be run, and instead the
   source, sink, use, and flow layers will be extracted from the
   observation and written to :save-file."
  [observation-or-model-spec source-concept sink-concept use-concept flow-concepts
   {:keys [source-threshold sink-threshold use-threshold trans-threshold
           rv-max-states downscaling-factor source-type sink-type use-type benefit-type
           result-type value-type animation? save-file]
    :or {result-type :closure-map
         value-type  :varprop}}]
  (let [observation (if (vector? observation-or-model-spec)
                      (with-message "Running model to get observation..." "done."
                        (apply run-at-location observation-or-model-spec))
                      observation-or-model-spec)]
    ;; This version of SPAN only works for grid-based observations (i.e. raster maps).
    (assert (grid-extent? observation))
    (println "Unpacking observation into data-layers.")
    ;; FIXME fv this is to address an issue before it shows up - to be removed
    (collect-states observation)
    (println (. observation getStates))
    (let [rows            (grid-rows       observation)
          cols            (grid-columns    observation)
          [cell-w cell-h] (cell-dimensions observation) ;; in meters
          flow-model      (.getLocalName (get-observable-class observation))
          source-layer    (layer-from-observation     observation source-concept rows cols value-type)
          sink-layer      (layer-from-observation     observation sink-concept   rows cols value-type)
          use-layer       (layer-from-observation     observation use-concept    rows cols value-type)
          flow-layers     (layer-map-from-observation observation flow-concepts  rows cols value-type)]
      (println "Flow Parameters:")
      (println "flow-model         =" flow-model)
      (println "downscaling-factor =" downscaling-factor)
      (println "rv-max-states      =" rv-max-states)
      (println "source-threshold   =" source-threshold)
      (println "sink-threshold     =" sink-threshold)
      (println "use-threshold      =" use-threshold)
      (println "trans-threshold    =" trans-threshold)
      (println "cell-width         =" cell-w "meters")
      (println "cell-height        =" cell-h "meters")
      (println "source-type        =" source-type)
      (println "sink-type          =" sink-type)
      (println "use-type           =" use-type)
      (println "benefit-type       =" benefit-type)
      (println "result-type        =" result-type)
      (println "value-type         =" value-type)
      (println "animation?         =" animation?)
      (println "save-file          =" save-file)
      (println "(Pausing 10 seconds)")
      (Thread/sleep 10000)
      (if (string? save-file)
        (do (println "Writing extracted SPAN layers to" save-file "and exiting early.")
            (save-span-layers save-file source-layer sink-layer use-layer flow-layers cell-w cell-h value-type))
        (run-span (remove-nil-val-entries
                   {:source-layer       source-layer
                    :source-threshold   source-threshold
                    :sink-layer         sink-layer
                    :sink-threshold     sink-threshold
                    :use-layer          use-layer
                    :use-threshold      use-threshold
                    :flow-layers        flow-layers
                    :trans-threshold    trans-threshold
                    :cell-width         cell-w
                    :cell-height        cell-h
                    :rv-max-states      rv-max-states
                    :downscaling-factor downscaling-factor
                    :source-type        source-type
                    :sink-type          sink-type
                    :use-type           use-type
                    :benefit-type       benefit-type
                    :flow-model         flow-model
                    :animation?         animation?
                    :result-type        result-type}))))))
