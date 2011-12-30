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

(defcontext arizona512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-114.821 37, -109.05 37, -109.05 31.333, -114.821 31.333, -114.821 37))"))

(defcontext arizona2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-114.821 37, -109.05 37, -109.05 31.333, -114.821 31.333, -114.821 37))"))

(defcontext san_joaquin512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-120.75 36.5, -118.75 36.5, -118.75 34.75, -119.5 34.75, -120.75 36.5))"))

(defcontext san_joaquin1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-120.75 36.5, -118.75 36.5, -118.75 34.75, -119.5 34.75, -120.75 36.5))"))

(defcontext san_pedro256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 30.869, -111.012 30.869, -111.012 33.281))"))

(defcontext san_pedro512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 30.869, -111.012 30.869, -111.012 33.281))"))

(defcontext san_pedro1024
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-110.98 33.07, -109.86 33.07, -109.86 30.869, -110.98 30.869, -110.98 33.07))"))

(defcontext san_pedro_sprnca2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext san_pedro_bsr256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-110.401 32.037, -110.193 32.037, -110.193 31.855, -110.401 31.855, -110.401 32.037))"))

(defcontext san_pedro_us256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us400
  ""
  (grid
   400
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_sprnca256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext san_pedro_sprnca512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext la_antigua1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))

(defcontext la_antigua800
  ""
  (grid
   800
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))

(defcontext la_antigua512
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))

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

(defcontext DR256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-71.7 19.9, -70.5 19.9, -70.5 18.85, -71.7 18.85, -71.7 19.9))"))

(defcontext DR512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-71.7 19.9, -70.5 19.9, -70.5 18.85, -71.7 18.85, -71.7 19.9))"))

                                        ;(defcontext chehalis
                                        ;  ""
                                        ;  (grid
                                        ;    256
                                        ;    "EPSG:4326 POLYGON((-124.27 47.55, -124.27 46.33, -122.42 46.33, -122.42 47.55, -124.27 47.55))"))

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

(defcontext wa_or_carbon
  ""
  (grid
   2048
   "EPSG:4326 POLYGON ((-124.88 42.0, -124.88 49.0, -120.6 49.0, -120.6 42.0, -124.88 42.0))"))

(defcontext mg256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((41.352539056744014 -27.644606378394307, 52.778320305152796 -27.644606378394307, 52.778320305152796 -10.488, 41.352539056744014 -10.488, 41.352539056744014 -27.644606378394307))"))

(defcontext mg512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((41.352539056744014 -27.644606378394307, 52.778320305152796 -27.644606378394307, 52.778320305152796 -10.488, 41.352539056744014 -10.488, 41.352539056744014 -27.644606378394307))"))

(defcontext mg_coastal128
  ""
  (grid
   128
   "EPSG:4326 POLYGON ((47.0 -16.4, 50.9 -16.4, 50.9 -20.2, 47.0 -20.2, 47.0 -16.4))"))

(defcontext mg_coastal256
  ""
  (grid
   256
   "EPSG:4326 POLYGON ((47.0 -16.4, 50.9 -16.4, 50.9 -20.2, 47.0 -20.2, 47.0 -16.4))"))  

(defcontext mg_coastal512
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((47.0 -16.4, 50.9 -16.4, 50.9 -20.2, 47.0 -20.2, 47.0 -16.4))"))

(defcontext mg_coastal1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON ((47.0 -16.4, 50.9 -16.4, 50.9 -20.2, 47.0 -20.2, 47.0 -16.4))"))

(defcontext mg_sed
  ""
  (grid
   1024
   "EPSG:4326 POLYGON ((52.778320305152796 -10, 52.778320305152796 -27.644606378394307, 41.352539056744014 -27.644606378394307, 41.352539056744014 -15.934637411351126, 52.778320305152796 -10))"))  

(defcontext lye_brook
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.097615 43.167086, -72.938627 43.167086, -72.938627 43.051168, -73.097615 43.051168, -73.097615 43.167086))")) 

(defcontext usa_bbox
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-124.7625 24.5210, -66.9326  24.5210, -66.9326 49.3845, -124.7625 49.3845, -124.7625 24.5210))"))

(defcontext europe_bbox
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-21.2660 27.6363, -21.2660 61.0088, 39.8693 61.0088, 39.8693 27.6363, -21.2660 27.6363))"))

(defcontext agri
  ""
  (grid
   256
   "EPSG:4326 POLYGON((15.544968 40.455699, 15.919189 40.190414, 16.11557 40.336077, 15.770874 40.605612, 15.544968 40.455699))"))

(defcontext west_coyote_hills256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-117.976828 33.907017, -117.976828  33.89560825, -117.940320  33.89560825, -117.940320 33.907017, -117.976828 33.907017))"))

(defcontext san_gabriel256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-118.2 34.03787, -117.724 34.03787, -117.724 33.739, -118.2 33.739, -118.2 34.03787))"))

(defcontext san_gabriel512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-118.2 34.03787, -117.724 34.03787, -117.724 33.739, -118.2 33.739, -118.2 34.03787))"))

(defcontext grr_tanzania
  "Great Ruaha River bounding box"
  (grid
   256
   "EPSG:4326 POLYGON((33.796 -7.086, 35.946 -7.086, 35.946 -9.41, 33.796 -9.41, 33.796 -7.086))"))

(defcontext grr_tanzania_coarse
  "Great Ruaha River bounding box"
  (grid
   100
   "EPSG:4326 POLYGON((33.796 -7.086, 35.946 -7.086, 35.946 -9.41, 33.796 -9.41, 33.796 -7.086))"))

(defcontext grr_tanzania_old
  "Great Ruaha River bounding box"
  (grid
   256
   "EPSG:4326 POLYGON((33.8 -7.087872, 35.934146 -7.087872, 35.934146 -9.407777, 33.8 -9.407777, 33.8 -7.087872))"))

(defcontext co_grand_county400
  "Grand County, CO, for viewshed & sediment analysis"
  (grid
   400
   "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))"))

(defcontext co_grand_county512
  "Grand County, CO, for viewshed & sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))"))

(defcontext co_blue
  "Colorado Blue River watershed, including Dillon and Green Mountain Reservoirs, for sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.475 40.05, -105.76 40.05, -105.76 39.35, -106.475 39.35, -106.475 40.05))"))

(defcontext upper_south_platte
  "Upper South Platte watershed, including several reservoirs, for sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.215 39.77, -104.8 39.77, -104.8 38.73, -106.215 38.73, -106.215 39.77))"))

(defcontext co_grand_boulder_larimer
  "Grand, Boulder, and Larimer counties, CO, for carbon analysis"
  (grid
   1024
   "EPSG:4326 POLYGON((-106.66 41.01, -104.93 41.01, -104.93 39.67, -106.66 39.67, -106.66 41.01))"))

(defcontext co_south_platte
  "Colorado Water Division 1 (South Platte River watershed), for water analysis"
  (grid
   1024
   "EPSG:4326 POLYGON((-106.25 41.01, -102.04 41.01, -102.04 38.63, -106.25 38.63, -106.25 41.01))"))

(defcontext co_south_platte_lowres
  "Colorado Water Division 1 (South Platte River watershed), for water analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.25 41.01, -102.04 41.01, -102.04 38.63, -106.25 38.63, -106.25 41.01))"))