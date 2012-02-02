;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; Water supply model for La Antigua, Veracruz, Mexico
;;;
;;; Valid Contexts: core.contexts.la_antigua/la_antigua*
;;;
;;;-------------------------------------------------------------------

(ns core.models.water-la-antigua
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization
                           namespace-ontology ranking numeric-coding
                           binary-coding identification
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking bayesian count])
  (:refer aries :only [span]))

(namespace-ontology waterSupplyService
  (representation:GenericObservable
   (TempSurfaceWaterData)))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;; Runoff would be preferable to precipitation, but its at a very
;; coarse spatial resolution (i.e., <3 full pixels for the La Antigua
;; watershed.  Use precip for now, with the goal of incorporating a
;; better runoff model (plus sink models that actually capture
;; infiltration & ET).
(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;; Incorporate runoff data in the future once we've done a better job with the hydro modeling.
;; (defmodel runoff soilRetentionService:AnnualRunoff
;;  (classification (measurement soilRetentionService:Runoff "mm/year")
;;    [2400 :>]   soilRetentionService:VeryHighAnnualRunoff
;;    [1200 2400] soilRetentionService:HighAnnualRunoff
;;    [600 1200]  soilRetentionService:ModerateAnnualRunoff
;;    [200 600]   soilRetentionService:LowAnnualRunoff
;;    [0 200]     soilRetentionService:VeryLowAnnualRunoff))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

;; Ad hoc sink model adapted from the ad hoc flood sink model.
;; Includes infiltration & evapotranspiration processes.  Deterministic
;; models could likely be used.

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00B0")
    [    0  1.15]         Level
    [ 1.15  4.57]         GentlyUndulating
    [ 4.57 16.70]         RollingToHilly
    [16.70 90 :inclusive] SteeplyDissectedToMountainous))

(defmodel soil-group HydrologicSoilsGroup
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 SoilGroupA
    2 SoilGroupB
    3 SoilGroupC
    4 SoilGroupD))

(defmodel imperviousness PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousSurface)
    [80 100 :inclusive] VeryHighImperviousCover
    [50 80]             HighImperviousCover
    [20 50]             ModeratelyHighImperviousCover
    [10 20]             ModeratelyLowImperviousCover
    [ 5 10]             LowImperviousCover
    [ 0  5]             VeryLowImperviousCover))

;; these classes do not show up in the classification statement, but are present in the data layer
;; Cuerpo de agua, Manglar, humedad
(defmodel vegetation-type veracruz:WaterSupplyVegetationType
  "Just a reclass of the Veracruz land use layer"
  (classification (categorization veracruz-lulc:VeracruzLULCCategory)
    "Bosque mesofilo de montana"                                                                         veracruz:CloudForest
    #{"Pastizal cultivado" "Pastizal inducido" "Zona Urbana" "riego" "temporal"}                         veracruz:DevelopedCultivated
    #{"Bosque cultivado" "Bosque de encino" "Bosque de oyamel" "Bosque de pino" "Bosque de pino-encino"} veracruz:DryForest
    #{"Matorral desertico rosetofilo" "Pradera de alta montana" "Vegetacion de dunas costeras"}          veracruz:GrasslandShrubland
    #{"Selva baja caducifolia" "Selva mediana subcaducifolia"}                                           veracruz:Rainforest))

;; 254 & 255 in the global layer are equal to zero, hence the strange
;; discretization here.  Also 80 represents 80-100% tree canopy cover,
;; and there are no values from 1-9.
(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    80                                                             VeryHighCanopyCover
    #{60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79} HighCanopyCover
    #{40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59} ModerateCanopyCover
    #{20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39} LowCanopyCover
    #{0 10 11 12 13 14 15 16 17 18 19 254 255}                     VeryLowCanopyCover))

;; see e-mail from Octavio regarding the fact that there are no major dams to consider in the La Antigua watershed
;;(defmodel dam-presence Dams
;;  (classification (binary-coding NonRivalWaterUseCode)
;;      1         DamPresent
;;     :otherwise DamAbsent))

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration, dams, etc.
(defmodel evapotranspiration EvapotranspirationClass
  (probabilistic-measurement EvapotranspirationClass "mm" 
    [180 260] VeryHighEvapotranspiration
    [100 180] HighEvapotranspiration
    [ 50 100] ModerateEvapotranspiration
    [  0  50] LowEvapotranspiration
    [  0   0] VeryLowEvapotranspiration))

(defmodel et-sink Evapotranspiration
  (bayesian Evapotranspiration
    :import  "aries.core::SurfaceWaterSinkLA.xdsl"
    :context [vegetation-type percent-canopy-cover]
    :keep    [EvapotranspirationClass]
    :result  evapotranspiration))

(defmodel soil-infiltration SoilInfiltrationClass
  (probabilistic-measurement SoilInfiltrationClass "mm" 
    [180 260] VeryHighInfiltration
    [100 180] HighInfiltration
    [ 50 100] ModerateInfiltration
    [  0  50] LowInfiltration
    [  0   0] VeryLowInfiltration))

(defmodel soil-sink SoilInfiltration
  (bayesian SoilInfiltration
    :import  "aries.core::SurfaceWaterSinkLA.xdsl"
    :context [soil-group slope imperviousness]
    :keep    [SoilInfiltrationClass]
    :result  soil-infiltration))

(defmodel surface-water-sink SurfaceWaterSink
  (measurement SurfaceWaterSink "mm"
    :context [soil-sink et-sink] 
    :state   #(let [si (:soil-infiltration-class  %)
                    et (:evapotranspiration-class %)]
                (+ 
                 (if (nil? si) 0.0 (.getMean si))
                 (if (nil? et) 0.0 (.getMean et))))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; The models for the La Antigua watershed are for SURFACE WATER ONLY
;; Following the February 2011 workshop we will determine if the inclusion 
;; of a groundwater model is possible

;; INDUSTRIAL
;; This is all we have to model industrial use right now: presence/absence of an industrial
;; user and whether they use ground or surface water. 
;; Current extraction is unknown.  Values below are 100% guesses - need information from local plants.
;; 2 & 3 are zeros because they represent groundwater use, which we're not yet modeling.
;; (defmodel industrial-users IndustrialWaterUse
;;  (measurement IndustrialWaterUse "mm" ;;This is an annual value.
;;    :context ((numeric-coding IndustrialWaterUseCode :as industrial-water-use))
;;    :state #(cond (== (:industrial-water-use %) 0) 50   ;;Paper factory, using surface water
;;                  (== (:industrial-water-use %) 1) 100  ;;Bottled water plant, using surface water
;;                  (== (:industrial-water-use %) 2)  0   ;;Nestle plant, using groundwater
;;                  (== (:industrial-water-use %) 3)  0   ;;Coca-Cola plant, using groundwater
;;                  :otherwise                        0)))

;; Revamped industrial water use model
;; Relies on the data provided by Octavio
(defmodel industrial-surface-water-use IndustrialSurfaceWaterUse
  (measurement IndustrialSurfaceWaterUse "mm")) 

;; Nonrival surface water use: Hydropower plus rafting
;; This is all we have to model rafting and hydropower use right now: presence/absence of a user
;; user. It's a little strange to lump hydro and rafting together, but we'll
;; do it for now.
(defmodel non-rival-surface-water-use NonRivalWaterUse
  (binary-coding NonRivalWaterUse
    :context [(binary-coding NonRivalWaterUseCode)]
    :state #(cond (== (:non-rival-water-user-code %) 0) 1  ;;Rafting use
                  (== (:non-rival-water-user-code %) 1) 1  ;;Hydropower use
                  :otherwise                        0)))

;; RESIDENTIAL
;; The ranking model should really be a count with spatial ctx (data are persons/30 arc-second pixel)
;; The first example is for a probability distribution function (discrete values of the probability states)
;; The second example, used, is for a cumulative distribution function (ranges)
;; Neither of these are enabled yet, so we're just using a deterministic function for now.
;; Residential surface water use: currently only looking at surface water use (80% of the total, per Rowan's BN. 
;; Top node discretization for water use is from Alberta: worth asking about better local sources.
;; www1.agric.gov.ab.ca/$department/deptdocs.nsf/all/agdex1349
;; (defmodel residential-surface-water-use ResidentialSurfaceWaterUse
;;  (measurement ResidentialSurfaceWaterUse "mm" ;;This is an annual value
;;    :context [(count policytarget:PopulationDensity "/km^2")]
;;    :state   #(* 0.8 0.082855 (:population-density %))))
;;    :state   #(rv-scalar-multiply {10 25/100, 20 50/100, 30 25/100} (* 0.8 (:population-density %))) 
;;    :state   #(rv-scalar-multiply {70.81 0, 78.84 25/100, 86.87 75/100, 94.9 1} (* 0.8 (:population-density %)))))

;; Revamped residential water use model
;; Relies on the data provided by Octavio
(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm"))

;; AQUACULTURE
;; This model is aquaculture use of surface water and is based entirely on the
;; water rights spreadsheets provided by Octavio
(defmodel aquacultural-surface-water-use AquaculturalSurfaceWaterUse
  (measurement AquaculturalSurfaceWaterUse "mm")) 

;; AGRICULTURE
;; Agricultural surface water use. Step 1: Estimate total livestock water needs.
;; Livestock use below is from different sources for pigs and other livestock: its unlikely
;; that pigs should use more water per capita than cattle (Rowan to check on this)
;; (defmodel livestock-total-water-use LivestockWaterUse
;;  (measurement LivestockWaterUse "mm"  ;;This is an annual value
;;    :context [(count CattlePopulation "/km^2") (count SheepPopulation  "/km^2")
;;              (count PigsPopulation   "/km^2") (count GoatsPopulation  "/km^2")]
;;    :state   #(+ (* (:sheep-population  %) 0.002745) ;;this is in m^3/1000 animals, the conversion ultimately giving mm
;;                 (* (:goats-population  %) 0.002745)
;;                 (* (:cattle-population %) 0.011032)
;;                 (* (:pigs-population   %) 0.013310))))

(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUse
  (measurement AgriculturalSurfaceWaterUse "mm")) 

;;(defmodel livestock-total-water-use-discretized LivestockTotalWaterUseClass
;;  (classification livestock-total-water-use
;;    [1.15 :>]  HighLivestockTotalWaterUse
;;    [0.5 1.15] ModerateLivestockTotalWaterUse
;;    [:<  0.5]  LowLivestockTotalWaterUse))

;;Agricultural surface water use. Step 2: Consider proximity to surface water.
;;(defmodel surface-water-proximity ProximityToSurfaceWaterClass
;;  (classification (measurement ProximityToSurfaceWater "m")
;;    [:<  250] HighSurfaceWaterProximity
;;    [250 500] ModerateSurfaceWaterProximity
;;    [500  :>] LowSurfaceWaterProximity))

;; Agricultural surface water use. Step 3: Estimate crop irrigation water needs.
;; Need better irrigation water estimates OR should we only rely on the water rights data???
;; for the workshop we will present the results using only the water rights data. This can obviously
;; be changed based on workshop inputs
;; (defmodel irrigation-water-use IrrigationWaterUseClass
;;  (measurement IrrigationWaterUse "mm"  ;;This is an annual value
;;     :context [(categorization veracruz-lulc:VeracruzLULCCategory)]
;;     :state   #(if (= (:veracruz-l-u-l-c-category %) "riego")
;;                  2000
;;                  0)))

;; Classification of irrigationWaterUse into 6 classes.
;; (defmodel irrigation-water-use-discretized IrrigationWaterUseClass
;;  (classification irrigation-water-use
;;    [2400   :>] VeryHighIrrigationUse
;;    [2150 2400] HighIrrigationUse
;;    [1850 2150] ModerateIrrigationUse
;;    [1600 1850] LowIrrigationUse
;;    [:<   1600] VeryLowIrrigationUse
;;    0           NoIrrigationUse))

;;Undiscretization of agricultural surface water use
;;(defmodel use-undiscretizer AgriculturalSurfaceWaterUseClass
;;  (probabilistic-measurement AgriculturalSurfaceWaterUseClass "mm" 
;;    [2000 3000] HighAgriculturalSurfaceWaterUse
;;    [1000 2000] ModerateAgriculturalSurfaceWaterUse
;;    [   0 1000] LowAgriculturalSurfaceWaterUse)) 

;;(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUseClass
;;  (bayesian AgriculturalSurfaceWaterUseClass  
;;    :import   "aries.core::SurfaceWaterUseLAAgriculture.xdsl"
;;    :context  [surface-water-proximity livestock-total-water-use-discretized irrigation-water-use-discretized]
;;    :context  [surface-water-proximity livestock-total-water-use-discretized]
;;    :keep     [AgriculturalSurfaceWaterUseClass]
;;    :result   use-undiscretizer)) 

;; Below would be a logical and simple way to do things.  However these features are not yet enabled.

;; Total surface water use. Step 5: Add the four rival user groups
(defmodel total-surface-water-use TotalSurfaceWaterUse
  (measurement TotalSurfaceWaterUse "mm"  ;;This is an annual value
    :context [agricultural-surface-water-use aquacultural-surface-water-use residential-surface-water-use industrial-surface-water-use]
    :state   #(let [a (:agricultural-surface-water-use %)
                    q (:aquacultural-surface-water-use %)
                    r (:residential-surface-water-use  %)
                    i (:industrial-surface-water-use   %)]
                (+ (or a 0)
                   (or q 0)
                   (or r 0)
                   (or i 0)))))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel data WaterSupply 
  (identification WaterSupply 
    :context [precipitation-annual surface-water-sink
              non-rival-surface-water-use total-surface-water-use
              altitude streams]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; flow model for surface water
(defmodel surface-flow FreshwaterSupply
  (span SurfaceWaterMovement
        AnnualPrecipitation
        TotalSurfaceWaterUse
        SurfaceWaterSink
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   nil
        :sink-threshold     nil
        :use-threshold      nil
        :trans-threshold    10.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/water_la_antigua_data.clj")
        :context             [precipitation-annual surface-water-sink
                              total-surface-water-use altitude streams]
        :keep                [TheoreticalSource
                              TheoreticalSink
                              TheoreticalUse
                              PossibleFlow
                              PossibleSource
                              PossibleUse
                              ActualFlow
                              ActualSource
                              ActualSink
                              ActualUse
                              InaccessibleSource
                              InaccessibleSink
                              InaccessibleUse
                              BlockedFlow
                              BlockedSource
                              BlockedUse]))
