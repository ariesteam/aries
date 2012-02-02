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

(ns core.contexts.vermont
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext raven_ridge
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext raven_ridge_large
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-74.439038 45.188716, -71.877970 45.188716, -71.877970 43.346402, -74.439038 43.346402,-74.439038 45.188716))"))

(defcontext lye_brook
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.097615 43.167086, -72.938627 43.167086, -72.938627 43.051168, -73.097615 43.051168, -73.097615 43.167086))"))

(defcontext vt256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext vt512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext vtcoverage
  ""
  (grid
   256
   "EPSG:4326 POLYGON ((-73.151 44.27 ,-73.144  44.27 ,-73.144  44.286 , -73.151 44.286 ,-73.151  44.27 ))"))

(defcontext vt_simple
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-71.46528176222033 45.0136832226448, -71.63336591213641 44.750048537748114, -71.57686650739787 44.50236705743422, -72.03287227690927 44.32052732132643, -72.0315363920979 44.07937899156674, -72.38050646075203 43.57382871186157, -72.55722886459505 42.853682769843914, -72.45842479251264 42.72711895351649, -73.27619328348301 42.745989373360246, -73.24148772974226 43.53280954970073, -73.43133354459462 43.58792612069299, -73.34293424084181 45.01072500108264, -71.46528176222033 45.0136832226448))"))
