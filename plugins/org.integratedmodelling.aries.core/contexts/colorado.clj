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