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
;;; This namespace defines a number of functions for generating
;;; source, sink, use, and flow layers (randomly or from GIS maps),
;;; writing them to files, and reading them back.

(ns clj-span.worldgen
  (:use [clj-misc.matrix-ops :only (make-matrix)]
        [clj-misc.utils      :only (constraints-1.0 p)]
        [clj-misc.randvars   :only (cont-type disc-type)]
        [clojure.contrib.duck-streams :only (spit file-str with-in-reader read-lines)]))

(defn read-layer-from-file
  [filename]
  (with-in-reader (file-str filename) (read)))

(defn write-layer-to-file
  [filename layer]
  (binding [*print-dup* true]
    (spit (file-str filename) layer)))

;; Deprecated - reimplement with clj-misc.randvars/make-randvar
(defn make-random-layer
  [rows cols type]
  (constraints-1.0 {:pre [(#{:discrete :continuous} type)]})
  (let [meta (if (= type :discrete) disc-type cont-type)]
    (make-matrix rows cols (fn [_] (with-meta {(rationalize (rand-int 100)) 1} meta)))))

;; Deprecated - reimplement with clj-misc.randvars/make-randvar
(defn make-random-layer-map
  [rows cols name-to-type-map]
  (constraints-1.0 {:pre [(every? #(or (fn? %) (#{:discrete :continuous :hydrosheds} %)) (vals name-to-type-map))]})
  (into {}
        (for [[name type] name-to-type-map]
          [name (make-matrix rows cols
                             (cond (fn? type)           type
                                   (= type :hydrosheds) (fn [_] (with-meta
                                                                  {([-1 0 1 2 4 8 16 32 64 128] (rand-int 10)) 1}
                                                                  disc-type))
                                   :otherwise           (let [meta (if (= type :discrete)
                                                                     disc-type
                                                                     cont-type)]
                                                          (fn [_] (with-meta
                                                                    {(rationalize (rand-int 100)) 1}
                                                                    meta)))))])))

(defn make-layer-from-ascii-grid
  [filename]
  (let [lines (read-lines filename)
        rows  (Integer/parseInt (second (re-find #"^NROWS\s+(\d+)" (first  lines))))
        cols  (Integer/parseInt (second (re-find #"^NCOLS\s+(\d+)" (second lines))))
        data  (drop-while (p re-find #"^[^\d]") lines)]
    (println "Stub...process the data...")))

(defn make-ascii-grid-from-layer
  [layer]
  (let [buf (StringBuffer.)]
    (.append buf "finish me...")
    (.toString buf)))

(comment
 "north:                   4299000.00
  south:                   4247000.00
  east:                     528000.00
  west:                     500000.00
  rows:                         10   
  cols:                         15   
  null:                      -9999   

  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15")
