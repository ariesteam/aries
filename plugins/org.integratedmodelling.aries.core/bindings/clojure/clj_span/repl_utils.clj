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
;;; This namespace defines functions for testing the various SPAN
;;; simulations at the REPL.

(ns clj-span.repl-utils
  (:use (clj-span core commandline aries-span-bridge analyzer gui)
        [clj-span.worldgen :only [read-layer-from-file]]
        (clj-misc utils matrix-ops stats)
        clojure.pprint)
  (:require (clj-misc [numbers :as nb] [varprop :as vp] [randvars :as rv])))

(defn load-layers
  [filename]
  (let [[s k u f cell-w cell-h] (read-span-layers filename)]
    (def source-layer s)
    (def sink-layer   k)
    (def use-layer    u)
    (def flow-layers  f)
    (def cell-width   cell-w)
    (def cell-height  cell-h)
    (def rows (get-rows s))
    (def cols (get-cols s))))

(defn load-layer
  [layer-name filename]
  (let [data (read-layer-from-file filename)]
    (case layer-name
      :source (def source-layer data)
      :sink   (def sink-layer   data)
      :use    (def use-layer    data)
      :flow   (def flow-layers  data))))

(defn extract-results
  [value-type result-map]
  (let [_0_ (case value-type
              :numbers  nb/_0_
              :varprop  vp/_0_
              :randvars rv/_0_)]
    (def tsrc  (let [rmap ((:theoretical-source  result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def isrc  (let [rmap ((:inaccessible-source result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def psrc  (let [rmap ((:possible-source     result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def bsrc  (let [rmap ((:blocked-source      result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def asrc  (let [rmap ((:actual-source       result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def tsnk  (let [rmap ((:theoretical-sink    result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def isnk  (let [rmap ((:inaccessible-sink   result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def asnk  (let [rmap ((:actual-sink         result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def tuse  (let [rmap ((:theoretical-use     result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def iuse  (let [rmap ((:inaccessible-use    result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def puse  (let [rmap ((:possible-use        result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def buse  (let [rmap ((:blocked-use         result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def ause  (let [rmap ((:actual-use          result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def pflow (let [rmap ((:possible-flow       result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def bflow (let [rmap ((:blocked-flow        result-map))] (make-matrix rows cols #(get rmap % _0_))))
    (def aflow (let [rmap ((:actual-flow         result-map))] (make-matrix rows cols #(get rmap % _0_))))))

(defn test-run-sediment
  [value-type]
  (run-span {:flow-model         "SedimentTransport"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      0.0
             :trans-threshold    1.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :infinite
             :benefit-type       :rival ;; or :non-rival for turbidity
             :value-type         value-type
             :downscaling-factor 4
             :rv-max-states      10
             :animation?         true
             :result-type        :closure-map}))

(defn test-run-flood
  [value-type]
  (run-span {:flow-model         "FloodWaterMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   50.0
             :sink-threshold     3000.0
             :use-threshold      0.0
             :trans-threshold    5.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         value-type
             :downscaling-factor 3
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-storm
  [value-type]
  (run-span {:flow-model         "CoastalStormMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         value-type
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         true
             :result-type        :closure-map}))

(defn test-run-fishing
  [value-type]
  (run-span {:flow-model         "SubsistenceFishAccessibility"
             :source-layer       source-layer
             :sink-layer         nil
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     nil
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          nil
             :use-type           :finite
             :benefit-type       :rival
             :value-type         value-type
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-water
  [value-type]
  (run-span {:flow-model         "SurfaceWaterMovement"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      1.0
             :trans-threshold    1.0
             :source-type        :finite
             :sink-type          :finite
             :use-type           :finite
             :benefit-type       :rival
             :value-type         value-type
             :downscaling-factor 8
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-carbon
  [value-type]
  (run-span {:flow-model         "CO2Removed"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   0.0
             :sink-threshold     0.0
             :use-threshold      0.0
             :trans-threshold    0.1
             :source-type        :finite
             :sink-type          :finite
             :use-type           :finite
             :benefit-type       :rival
             :value-type         value-type
             :downscaling-factor 20
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-view
  [value-type]
  (run-span {:flow-model         "LineOfSight"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   25.0
             :sink-threshold     25.0
             :use-threshold      0.2
             :trans-threshold    1.0
             :source-type        :infinite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         value-type
             :downscaling-factor 2
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))

(defn test-run-proximity
  [value-type]
  (run-span {:flow-model         "Proximity"
             :source-layer       source-layer
             :sink-layer         sink-layer
             :use-layer          use-layer
             :flow-layers        flow-layers
             :cell-width         cell-width
             :cell-height        cell-height
             :source-threshold   40.0
             :sink-threshold     0.0
             :use-threshold      0.2
             :trans-threshold    1.0
             :source-type        :infinite
             :sink-type          :infinite
             :use-type           :infinite
             :benefit-type       :non-rival
             :value-type         value-type
             :downscaling-factor 1
             :rv-max-states      10
             :animation?         false
             :result-type        :closure-map}))
