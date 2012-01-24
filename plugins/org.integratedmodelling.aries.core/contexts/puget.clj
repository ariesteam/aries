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

(ns core.contexts.puget
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext chehalis
  "Chehalis watershed @ 256 linear. Just the square bounding box."
  (grid
   256
   "EPSG:4326 POLYGON ((-124.27 46.763, -124.27 47.55, -122.42 47.55, -122.42 46.763, -124.27 46.763))"))

(defcontext chehalis512
  "Chehalis watershed @ 512 linear. Just the square bounding box."
  (grid
   512
   "EPSG:4326 POLYGON ((-124.27 46.763, -124.27 47.55, -122.42 47.55, -122.42 46.763, -124.27 46.763))"))

(defcontext chehalis1024
  "Chehalis watershed @ 1024 linear. Just the square bounding box."
  (grid
   1024
   "EPSG:4326 POLYGON ((-124.27 46.763, -124.27 47.55, -122.42 47.55, -122.42 46.763, -124.27 46.763))"))

(defcontext wria9_256
  "Green-Duwamish watershed @ 256 linear. Just the square bounding box."
  (grid
   256
   "EPSG:4326 POLYGON ((-122.47 47.63, -122.47 47.08, -121.27 47.08, -121.27 47.63, -122.47 47.63))"))

(defcontext wria9_512
  "Green-Duwamish watershed @ 512 linear. Just the square bounding box."
  (grid
   512
   "EPSG:4326 POLYGON ((-122.47 47.63, -122.47 47.08, -121.27 47.08, -121.27 47.63, -122.47 47.63))"))

(defcontext wria9_1024
  "Green-Duwamish watershed @ 1024 linear. Just the square bounding box."
  (grid
   1024
   "EPSG:4326 POLYGON ((-122.47 47.63, -122.47 47.08, -121.27 47.08, -121.27 47.63, -122.47 47.63))"))

(defcontext wria9_2048
  "Green-Duwamish watershed @ 2048 linear. Just the square bounding box."
  (grid
   2048
   "EPSG:4326 POLYGON ((-122.47 47.63, -122.47 47.08, -121.27 47.08, -121.27 47.63, -122.47 47.63))"))

(defcontext wria9_views512
  "Green-Duwamish watershed @ 512 linear. Just the square bounding box."
  (grid
   512
   "EPSG:4326 POLYGON ((-122.6 47.63, -122.6 46.75, -121.27 46.75, -121.27 47.63, -122.6 47.63))"))

(defcontext wria9_views1024
  "Green-Duwamish watershed @ 1024 linear. Just the square bounding box."
  (grid
   1024
   "EPSG:4326 POLYGON ((-122.6 47.63, -122.6 46.75, -121.27 46.75, -121.27 47.63, -122.6 47.63))"))

(defcontext viewshed256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-122.420446 47.464349, -121.759593 47.464349, -121.759593 46.85382, -122.420446 46.85382, -122.420446 47.464349))"))

(defcontext viewshed512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-122.420446 47.464349, -121.759593 47.464349, -121.759593 46.85382, -122.420446 46.85382, -122.420446 47.464349))"))

(defcontext viewshed1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-122.420446 47.464349, -121.759593 47.464349, -121.759593 46.85382, -122.420446 46.85382, -122.420446 47.464349))"))

(defcontext western_wa256
  ""
  (grid
   256
   "EPSG:4326 POLYGON ((-124.88 46.3, -124.88 49.0, -120.6 49.0, -120.6 46.3, -124.88 46.3))"))

(defcontext western_wa512
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-124.88 46.3, -124.88 49.0, -120.6 49.0, -120.6 46.3, -124.88 46.3))"))

(defcontext western_wa
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-124.88 46.3, -124.88 49.0, -120.6 49.0, -120.6 46.3, -124.88 46.3))"))

(defcontext western_wa2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-124.88 46.3, -124.88 49.0, -120.6 49.0, -120.6 46.3, -124.88 46.3))"))

(defcontext puget400
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-124.8 46.58, -124.8 49.0, -120.63 49.0, -120.63 46.58, -124.8 46.58))"))

(defcontext puget512
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-124.8 46.58, -124.8 49.0, -120.63 49.0, -120.63 46.58, -124.8 46.58))"))

(defcontext puget1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON ((-124.8 46.58, -124.8 49.0, -120.63 49.0, -120.63 46.58, -124.8 46.58))"))

(defcontext puget2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-124.8 46.58, -124.8 49.0, -120.63 49.0, -120.63 46.58, -124.8 46.58))"))

(defcontext puget4096
  ""
  (grid
   4096
   "EPSG:4326 POLYGON ((-124.8 46.58, -124.8 49.0, -120.63 49.0, -120.63 46.58, -124.8 46.58))"))

;; Contexts below are for flood and sediment in Puget Sound.  Problem:
;; if we run the models in several smaller contexts rather than just
;; the entire extent, they will have different spatial resolutions -
;; this is really bad if we want to stitch them back together!
(defcontext olympic_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-124.83 47.65, -124.83 48.43, -122.58 48.43, -122.58 47.65, -124.83 47.65))"))

(defcontext kitsap_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-123.55 47.0, -123.55 48.0, -122.33 48.0, -122.33 47.0, -123.55 47.0))"))

(defcontext southsound_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-123.07 46.57, -123.07 47.78, -121.35 47.78, -121.35 46.57, -123.07 46.57))"))

(defcontext eastcentralsound_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-122.45 47.01, -122.45 48.2, -121.05 48.2, -121.05 47.01, -122.45 47.01))"))

(defcontext northsound_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-123.11 47.95, -123.11 49, -120.64 49, -120.64 47.95, -123.11 47.95))"))

(defcontext islands_hydro2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-123.25 47.85, -123.25 48.81, -122.31 48.81, -122.31 47.85, -123.25 47.85))"))

(defcontext wa_or_carbon
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-124.88 42.0, -124.88 49.0, -120.6 49.0, -120.6 42.0, -124.88 42.0))"))