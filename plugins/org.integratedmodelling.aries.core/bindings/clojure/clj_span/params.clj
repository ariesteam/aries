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
;;; This namespace defines the global symbols available to all SPAN
;;; functions.  They should be set using set-global-params! before any
;;; data-preprocessing, flow model simulations, or result analyses.

(ns clj-span.params
  (:use [clj-misc.randvars :only (*rv-max-states* reset-rv-max-states!)]))

(declare *trans-threshold* *source-type* *sink-type* *use-type* *benefit-type*)

(defn set-global-params!
  [{:keys [rv-max-states trans-threshold source-type sink-type use-type benefit-type]}]
  (reset-rv-max-states! rv-max-states)
  (doseq [[sym value] {'*trans-threshold* trans-threshold,
                       '*source-type*     source-type,
                       '*sink-type*       sink-type,
                       '*use-type*        use-type,
                       '*benefit-type*    benefit-type}]
    (intern (find-ns 'clj-span.params) sym value)))

(defn print-global-params
  []
  (println "*rv-max-states*"   *rv-max-states*)
  (println "*trans-threshold*" *trans-threshold*)
  (println "*source-type*"     *source-type*)
  (println "*sink-type*"       *sink-type*)
  (println "*use-type*"        *use-type*)
  (println "*benefit-type*"    *benefit-type*))
