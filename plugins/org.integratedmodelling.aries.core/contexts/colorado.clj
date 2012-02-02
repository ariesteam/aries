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

(ns core.contexts.colorado
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model transform])
  (:refer geospace :only [grid shape]))

;; --------------------------------------------------------------------------
;; Using a variable for the resolution, so you can change it here and it gets
;; changed in all contexts. 
;; ---------------------------------------------------------------------------
(def resolution 1024)

;; -----------------------------------------------------------------------------
;; FV pre-defining the inline WKT-specified shapes as variables to keep the context
;; definition easier to edit and understand. 
;; This is optional and cosmetic. It allows to just use the vars
;; below instead of the (shape ...) forms and in the (grid ...) statements.
;; 
;; FV using inline specs even if the gazetteer works fine to avoid naming conflict
;; due to Ken using different templates when initializing the gazetteer, and to eliminate a variable in case of 
;; bugs.
;; -----------------------------------------------------------------------------

(def grand_county
  (shape "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))")) ; Run this at 512

(def grand_boulder_larimer
  (shape "EPSG:4326 POLYGON((-106.66 41.01, -104.93 41.01, -104.93 39.67, -106.66 39.67, -106.66 41.01))")) ; Run this at 1024

(def blue
  (shape "EPSG:4326 POLYGON((-106.475 40.05, -105.76 40.05, -105.76 39.35, -106.475 39.35, -106.475 40.05))")) ; Intended to be run at 512

(def upper_south_platte
  (shape "EPSG:4326 POLYGON((-106.215 39.77, -104.8 39.77, -104.8 38.73, -106.215 38.73, -106.215 39.77))")) ; Intended to be run at 512

(defcontext no-mountain-pine-beetle-carbon
  "Conditions with no pine beetle damage"
  (grid resolution grand_boulder_larimer)
;; Changes to carbon model
  (transform 'colorado:MountainPineBeetleDamageClass 'colorado:NoDamage))

(defcontext no-mountain-pine-beetle-aesthetic-view
  "Conditions with no pine beetle damage"
  (grid resolution grand_county)
;; Changes to aesthetic view model
  (transform 'colorado:GrayBeetleKill 'colorado:GrayKillAbsent)  
  (transform 'colorado:GreenGrayBeetleKill 'colorado:GreenGrayKillAbsent))

(defcontext no-mountain-pine-beetle-sediment
  "Conditions with no pine beetle damage"
  (grid resolution grand_county) ; Can also sub blue or upper_south_platte
;; Changes to sediment model
  (transform 'colorado:MountainPineBeetleDamageClass 'colorado:NoDamage))

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