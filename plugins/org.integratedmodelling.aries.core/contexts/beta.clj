;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.

(ns core.contexts.beta
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext usa-bbox
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-124.7625 24.5210, -66.9326  24.5210, -66.9326 49.3845, -124.7625 49.3845, -124.7625 24.5210))"))

(defcontext europe-bbox
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-21.2660 27.6363, -21.2660 61.0088, 39.8693 61.0088, 39.8693 27.6363, -21.2660 27.6363))"))

(defcontext agri
  ""
  (grid
   256
   "EPSG:4326 POLYGON((15.544968 40.455699, 15.919189 40.190414, 16.11557 40.336077, 15.770874 40.605612, 15.544968 40.455699))"))