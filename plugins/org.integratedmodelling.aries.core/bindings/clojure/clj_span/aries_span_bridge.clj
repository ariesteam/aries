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
        [clj-misc.matrix-ops     :only (seq2matrix)]
        [clj-misc.utils          :only (remove-nil-val-entries constraints-1.0 with-message)])
  (:require (clj-misc [numbers :as nb] [varprop :as vp] [randvars :as rv]))
  (:import (java.io File FileWriter FileReader PushbackReader)
           (org.integratedmodelling.corescience.literals IndexedCategoricalDistribution)))

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
                            get-possible-states
                            get-probabilities
                            get-data
                            run-at-location))

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
         get-possible-states
         get-probabilities
         get-data
         run-at-location)
)

(defn save-span-layers
  [filename source-layer sink-layer use-layer flow-layers cell-width cell-height]
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
                      :actual-flow         (constantly {})}]
    (with-open [outstream (FileWriter. filename)]
      (binding [*out*       outstream
                *print-dup* true]
        (doseq [data [source-layer sink-layer use-layer flow-layers cell-width cell-height]]
          (prn data))))
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

;; NOTE: If I ever need to deal with a discrete distribution rather
;;       than continuous ranges, I can get the distribution's states
;;       with (get-possible-states ds).
(defmulti unpack-datasource
  ;; "Returns a seq of the values in ds, where their representations are
  ;;  determined by value-type. NaNs and nils are converted to 0s."
  (fn [value-type ds] (if (.isProbabilistic ds) :probabilistic :deterministic)))

(defmethod unpack-datasource :probabilistic
  [value-type ds]
  (println "Unpacking Bayesian datasource" ds)
  (let [dists                 (.getRawData ds)
        example-dist          (first (remove nil? dists))
        bounds                (seq (.getRanges example-dist))
        example-probs         (seq (.getData   example-dist))
        unbounded-from-below? (== Double/NEGATIVE_INFINITY (first bounds))
        unbounded-from-above? (== Double/POSITIVE_INFINITY (last  bounds))
        unpack-fn             (cond
                               (= value-type :randvars) #(if % (rv/create-from-ranges bounds (.getData %)) rv/_0_)
                               (= value-type :varprop)  #(if % (vp/create-from-ranges bounds (.getData %)) vp/_0_)
                               (= value-type :numbers)  #(if % (nb/create-from-ranges bounds (.getData %)) nb/_0_))]
    (println "Breakpoints:  " bounds)
    (println "Example Probs:" example-probs)
    (if (or unbounded-from-below? unbounded-from-above?)
      (throw (Exception. "All undiscretized bounds must be closed above and below.")))
    (for [#^IndexedCategoricalDistribution dist dists]
      (unpack-fn dist))))

(defmethod unpack-datasource :deterministic
  [value-type ds]
  (println "Unpacking deterministic datasource" ds)
  (let [unpack-fn (cond
                   (= value-type :randvars) #(rv/make-randvar :discrete 1 [%])
                   (= value-type :varprop)  #(vp/fuzzy-number % 0.0)
                   (= value-type :numbers)  identity)]
    (for [value (NaNs-to-zero (get-data ds))]
      (unpack-fn value))))

(defmethod unpack-datasource :default
  [value-type ds]
  (throw (Exception. (str "unpack-datasource is undefined for datasource type " (class ds)))))

(defn- layer-from-observation
  "Builds a rows x cols matrix (vector of vectors) of the concept's
   state values in the observation."
  [observation concept rows cols value-type]
  (when concept
    (println "Extracting" (.getLocalName concept) "layer.")
    (seq2matrix rows cols (unpack-datasource value-type (find-state observation concept)))))

(defn- layer-map-from-observation
  "Builds a map of {concept-names -> matrices}, where each concept's
   matrix is a rows x cols vector of vectors of the concept's state
   values in the observation."
  [observation concepts rows cols value-type]
  (when (seq concepts)
    (println "Extracting flow layers:" (map (memfn getLocalName) concepts))
    (into {}
          (map (fn [c] [(.getLocalName c)
                        (seq2matrix rows cols (unpack-datasource value-type (find-state observation c)))])
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
   observation, converted to :value-type, and written to :save-file."
  [observation-or-model-spec source-concept sink-concept use-concept flow-concepts
   {:keys [source-threshold sink-threshold use-threshold trans-threshold
           rv-max-states downscaling-factor source-type sink-type use-type benefit-type
           value-type animation? result-type save-file]
    :or {rv-max-states      10
         downscaling-factor 1
         value-type         :varprop
         result-type        :closure-map}}]
  (let [observation (if (vector? observation-or-model-spec)
                      (with-message "Running model to get observation..." "done."
                        (apply run-at-location observation-or-model-spec))
                      observation-or-model-spec)]
    ;; This version of SPAN only works for grid-based observations (i.e. raster maps).
    (assert (grid-extent? observation))
    (println "Unpacking observation into data-layers.")
    ;; FIXME fv this is to address an issue before it shows up - to be removed
    (collect-states observation)
    (let [rows            (grid-rows       observation)
          cols            (grid-columns    observation)
          [cell-w cell-h] (cell-dimensions observation) ;; in meters
          flow-model      (.getLocalName (get-observable-class observation))
          source-layer    (layer-from-observation     observation source-concept rows cols value-type)
          sink-layer      (layer-from-observation     observation sink-concept   rows cols value-type)
          use-layer       (layer-from-observation     observation use-concept    rows cols value-type)
          flow-layers     (layer-map-from-observation observation flow-concepts  rows cols value-type)]
      (println "Flow Parameters:")
      (println "source-threshold   =" source-threshold)
      (println "sink-threshold     =" sink-threshold)
      (println "use-threshold      =" use-threshold)
      (println "trans-threshold    =" trans-threshold)
      (println "cell-width         =" cell-w "meters")
      (println "cell-height        =" cell-h "meters")
      (println "rv-max-states      =" rv-max-states)
      (println "downscaling-factor =" downscaling-factor)
      (println "source-type        =" source-type)
      (println "sink-type          =" sink-type)
      (println "use-type           =" use-type)
      (println "benefit-type       =" benefit-type)
      (println "value-type         =" value-type)
      (println "flow-model         =" flow-model)
      (println "animation?         =" animation?)
      (println "result-type        =" result-type)
      (println "(Pausing 10 seconds)")
      (Thread/sleep 10000)
      (if (string? save-file)
        (do (println "Writing extracted SPAN layers to" save-file "and exiting early.")
            (save-span-layers save-file source-layer sink-layer use-layer flow-layers cell-w cell-h))
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
                    :value-type         value-type
                    :flow-model         flow-model
                    :animation?         animation?
                    :result-type        result-type}))))))
