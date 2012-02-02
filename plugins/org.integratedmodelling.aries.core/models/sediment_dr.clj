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
;;; Sediment regulation model for Dominican Republic
;;;
;;; Valid Contexts: core.contexts.dominican/DR*
;;;
;;;-------------------------------------------------------------------

(ns core.models.sediment-dr
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only [defscenario defmodel measurement
                           classification namespace-ontology
                           categorization ranking numeric-coding
                           probabilistic-measurement
                           probabilistic-classification
                           probabilistic-ranking binary-coding
                           identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology soilRetentionService)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel soil-group HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 SoilGroupA
    2 SoilGroupB
    3 SoilGroupC
    4 SoilGroupD))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [    0  1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

(defmodel soil-texture SoilTextureClass
  (classification (categorization habitat:SoilTexture)
    "Coarse" CoarseSoilTexture
    "Medium" MediumSoilTexture
    "Fine"	 FineSoilTexture)) 

;;Soil erodibility factor (k) from USLE/RUSLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
  (classification (ranking habitat:SoilErodibility)
    [:<     0.02]	VeryLowSoilErodibility
    [0.02   0.0275]	LowSoilErodibility
    [0.0275 0.0325]	ModerateSoilErodibility
    [0.0325 0.0375]	HighSoilErodibility
    [0.0375     :>]	VeryHighSoilErodibility))

;; Annual precipitation for Mg & DR
(defmodel precipitation-annual AnnualPrecipitationClass
  "FIXME this is annual precipitation."
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [2200   :>]	VeryHighAnnualPrecipitation
    [1800 2200]	HighAnnualPrecipitation
    [1200 1800] ModerateAnnualPrecipitation
    [ 600 1200]	LowAnnualPrecipitation
	[:<    600]	VeryLowAnnualPrecipitation))

;; Tropical storm probability, use only in DR & Mg
(defmodel storm-probability TropicalStormProbabilityClass
  (classification (ranking habitat:TropicalStormProbability)
    [5 :>]	   HighTropicalStormProbability
    [1 5]	   ModerateTropicalStormProbability
    :otherwise NoTropicalStormProbability)) 

;; Annual runoff, whereas snowmelt, precipitation, and temperature are
;; monnthly, so this is problematic.  Could divide yearly runoff by 12
;; but obviously its not evenly distributed throughout the year.  Or
;; could strongly consider just running it on an annual time step, as
;; that's what the data support.
(defmodel runoff AnnualRunoffClass
  (classification (measurement habitat:AnnualRunoff "mm")
    [2400   :>]	VeryHighAnnualRunoff
    [1200 2400]	HighAnnualRunoff
    [ 600 1200]	ModerateAnnualRunoff
    [ 200  600]	LowAnnualRunoff
    [   0  200]	VeryLowAnnualRunoff))

;; Vegetation type
;; FV the models in here are DR-specific so I see no
;; point in using general specs. It does create problems because the
;; BNs are not prepared to get the GLC classes, which do come up when
;; the DR data have holes. Commenting out the non-DR contingencies -
;; we should put them back when models are general, but then only
;; after making the BNs aware of all possible values.
(defmodel vegetation-type dominican:SedimentVegetationType
  "Just a reclass of the NLCD land use layer"
                                        ;	(classification (numeric-coding nlcd:NLCDNumeric)
                                        ;		#{41 42 43 71 90 95} ForestGrasslandWetland
                                        ;		#{52 81}			 ShrublandPasture
                                        ;		#{21 22 23 24 31 82} CropsBarrenDeveloped)
                                        ;  (classification (numeric-coding mglulc:MGLULCNumeric)
                                        ;	 #{1 2 4 5 6 10 14}							ForestWetland
                                        ;	 #{3 7 23}									DegradedForest
                                        ;		#{8 9 20 21 22 24 25 26 28 29 30 31 32 33} Savanna
                                        ;	 #{11 12 13 16 17}							CroplandDeveloped)
  (classification (numeric-coding domlulc:DOMLULCNumeric)
	#{1 2 4 6 8 9 11 18 35} dominican:ForestAndShrubland
	#{22 24 62}				dominican:WaterWetlandsMangroves
    #{41 45 53}			    dominican:ShadeCoffeeCocoa
	#{23 36 38 40 59}		dominican:IntensiveCroplandAndPasture
	#{42}					dominican:UrbanAndRoads)
                                        ;  (classification (numeric-coding glc:GLCNumeric)
                                        ;		#{1 2 3 4 5 6 7 8 9 15} ForestGrasslandWetland
                                        ;		#{10 11 12 13 14 17 18} ShrublandPasture
                                        ;	 #{16 19 22}			 CropsBarrenDeveloped)
  )

;; Discretization based on Quinton et al. (1997)
;; 254 & 255 in the global layer are equal to zero, hence the strange
;; discretization here.  Also 80 represents 80-100% tree canopy cover,
;; and there are no values from 1-9.
(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover :units "%")
    #{70 71 72 73 74 75 76 77 78 79 80} HighCanopyCover
    #{30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69} ModerateCanopyCover
    #{0 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 254 255} LowCanopyCover))

;;Sediment source value
(defmodel sediment-source-value-annual AnnualSedimentSourceClass
  ;; FV - sorry, my bad - theres a bug so the right way doesn't work
  ;; as a prototype obs. Will be fixed asap.  please leave as is for
  ;; now or the BN won't compile.
  (probabilistic-measurement AnnualSedimentSourceClass "t/ha"
                                        ;	(classification (measurement AnnualSedimentSource "t/ha")
    [40  100] HighAnnualSedimentSource
    [15   40] ModerateAnnualSedimentSource
    [0.01 15] LowAnnualSedimentSource 
    [0  0.01] NoAnnualSedimentSource))

;; Source bayesian model for Dominican Republic FV there is much
;; evidence setting for intermediate nodes here - those should be used
;; for training, when the PI eventually implements it. Commented those
;; below.
(defmodel source-dr AnnualSedimentSource
  (bayesian AnnualSedimentSource
	:import	  "aries.core::SedimentSourceDRAdHoc.xdsl"
	:context  [soil-group slope soil-texture (comment soil-erodibility)
              precipitation-annual storm-probability (comment runoff) 
              vegetation-type percent-canopy-cover]
	:required [SlopeClass]
	:keep	  [AnnualSedimentSourceClass]
	:result	  sediment-source-value-annual))

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel reservoirs ReservoirsClass 
  (classification (binary-coding geofeatures:Reservoir)
    1		   ReservoirPresent
    :otherwise ReservoirAbsent))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00B0")
	[2.86   :>]	HighStreamGradient
	[1.15 2.86]	ModerateStreamGradient
	[:<	  1.15]	LowStreamGradient))

(defmodel floodplain-canopy-cover FloodplainTreeCanopyCoverClass 
  (classification (numeric-coding habitat:PercentFloodplainTreeCanopyCover)
	[80 100 :inclusive] VeryHighFloodplainCanopyCover
	[60 80]				HighFloodplainCanopyCover
	[40 60]				ModerateFloodplainCanopyCover
    [20 40]				LowFloodplainCanopyCover
	[ 0 20]				VeryLowFloodplainCanopyCover))

(defmodel floodplains-code FloodplainsCode
  (binary-coding geofeatures:Floodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual AnnualSedimentSinkClass 
  ;; FV temporarily subst with dumb classification - see comment for sediment-source-value-annual
  (probabilistic-measurement AnnualSedimentSinkClass "t/ha"
    ;;	(classification (measurement AnnualSedimentSink "t/ha")
    [10  15] HighAnnualSedimentSink
    [ 5  10] ModerateAnnualSedimentSink
    [0.01 5] LowAnnualSedimentSink
    [0 0.01] NoAnnualSedimentSink))

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-dr AnnualSedimentSink
  (bayesian AnnualSedimentSink 
	:import	  "aries.core::SedimentSinkDR.xdsl"
	:context  [reservoirs stream-gradient floodplain-canopy-cover]
	:required [StreamGradientClass]
	:keep	  [AnnualSedimentSinkClass]
	:result	  sediment-sink-annual))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;(defmodel farmland Farmland
;;	"Just a reclass of the regionally appropriate LULC layer"
;;	(classification (binary-coding FarmlandCode)
;;	  1			 FarmlandPresent
;;	  0			 FarmlandAbsent))
;;Above statement (soilRetentionService:Farmland) is for coffee farmers in the DR; to use farmland 
;; from DR LULC data, comment out the above and turn on the statement below (domlulc:DOMLULCNumeric)
(defmodel farmland FarmlandCode
  (classification (numeric-coding domlulc:DOMLULCNumeric)
    #{23 36 38 40 41 45 53 59} FarmlandPresent
    :otherwise				   FarmlandAbsent))

;;Reservoirs use for DR: presence/absence only.
(defmodel hydroelectric-use-presence HydroelectricUsePresenceClass
  (classification (binary-coding geofeatures:Reservoir)
    0 HydroelectricUseAbsent
    1 HydroelectricUsePresent))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-deposition-use-dr DepositionProneFarmers 
  (binary-coding DepositionProneFarmers
    :context [(binary-coding FarmlandCode) (binary-coding geofeatures:Floodplain)]
    :state   #(if (and (= (:floodplain %) 1.0)
                       (= (:farmland-code %) 1.0))
                1
                0))) 

;; Models farmland in regions with erodible soils via basic spatial overlap.
;; FV FIXME I don't see any AnnualSedimentSource in the context?
;; Gary, is the context now correct to use the annual sediment source value properly?
(defmodel farmers-erosion-use-dr ErosionProneFarmers
  (ranking ErosionProneFarmers
    :context [(binary-coding FarmlandCode) source-dr]
    :state   #(if (= (:farmland-code %) 1.0)
                (cond (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                      1
                      (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                      2
                      :otherwise
                      0)
                0)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))			

(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
  (identification FarmlandSoilDeposition 
    :context [source-dr sediment-sink-dr farmers-deposition-use-dr
              altitude streams]))

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
  (identification ReservoirSoilDeposition 
    :context [source-dr sediment-sink-dr hydroelectric-use-presence
              altitude streams]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Sediment flow model for recipients of beneficial sedimentation.
;; We're assuming for now that sediment is detrimental to farmers, so
;; DON'T run this flow model for now, run the next two below (farmers
;; and hydro).  It should be able to be easily implemented in the
;; future, however.
(defmodel sediment-beneficial BeneficialSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
		DepositionProneFarmers
		AnnualSedimentSinkClass 
		nil
		(geophysics:Altitude FloodplainsCode geofeatures:River)
		:source-threshold	2.0 ; Note that threshold values are different in the Puget sediment SPAN models than in DR or Mg. This is because units are different, so keep these values (or similar ones)
		:sink-threshold		1.0
		:use-threshold		0.5
		:trans-threshold	0.25
		:source-type		:finite
		:sink-type			:finite
		:use-type			:finite
		:benefit-type		:rival
		:rv-max-states		10
		:downscaling-factor 2
		;;:save-file		  (str (System/getProperty "user.home") "/sediment_dr_data_beneficial.clj")
        :context [source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains-code streams]
		:keep    [TheoreticalSource
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
                  InaccessibleUse
                  BlockedFlow
                  BlockedSource
                  BlockedUse])) 

;;Sediment flow model for recipients of avoided detrimental sedimentation: farmers.	 This is one of two beneficiary models
;; currently designed to be run.
(defmodel sediment-detrimental-farmers DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
		DepositionProneFarmers
		AnnualSedimentSinkClass 
		nil
		(geophysics:Altitude FloodplainsCode geofeatures:River)
		:source-threshold	2.0
		:sink-threshold		1.0
		:use-threshold		0.5
		:trans-threshold	0.25
		:source-type		:finite
		:sink-type			:finite
		:use-type			:finite
		:benefit-type		:non-rival
		:rv-max-states		10
		:downscaling-factor 2
		;;:save-file		  (str (System/getProperty "user.home") "/sediment_dr_data_detrimental_farmers.clj")
        :context [source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains-code streams] ; Change the beneficiary group as needed
		:keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSink
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))

;; Sediment flow model for recipients of avoided detrimental
;; sedimentation: hydro reservoirs.  This is one of two beneficiary
;; models currently designed to be run.
(defmodel sediment-detrimental-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        AnnualSedimentSource
		HydroelectricUsePresenceClass
		AnnualSedimentSinkClass 
		nil
		(geophysics:Altitude FloodplainsCode geofeatures:River)
		:source-threshold	2.0
		:sink-threshold		1.0
		:use-threshold		0.5
		:trans-threshold	0.25
		:source-type		:finite
		:sink-type			:finite
		:use-type			:finite
		:benefit-type		:non-rival
		:rv-max-states		10
		:downscaling-factor 2
		;;:save-file		  (str (System/getProperty "user.home") "/sediment_dr_data_detrimental_reservoirs.clj")
		:context [source-dr hydroelectric-use-presence sediment-sink-dr altitude floodplains-code streams] ; Change the beneficiary group as needed
		:keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSink
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))

;; Sediment flow model for recipients of reduced turbidity. This SPAN
;; statement is not currently set up to run as we lack data on
;; beneficiary groups for reduced turbidity.  It should be able to be
;; easily implemented in the future, however.
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        AnnualSedimentSource
		WaterIntakeUse	; Change the beneficiary group as needed
		AnnualSedimentSinkClass 
		nil
		(geophysics:Altitude FloodplainsCode geofeatures:River)
		:source-threshold	2.0
		:sink-threshold		1.0
		:use-threshold		0.5
		:trans-threshold	0.25
		:source-type		:finite
		:sink-type			:finite
		:use-type			:finite
		:benefit-type		:non-rival
		:rv-max-states		10
		:downscaling-factor 2
		;;:save-file		  (str (System/getProperty "user.home") "/sediment_dr_data_turbidity.clj")
        :context [source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains-code streams] ; Change the beneficiary group as needed
		:keep    [TheoreticalSource
                  TheoreticalSink
                  TheoreticalUse
                  PossibleFlow
                  PossibleSource
                  PossibleUse
                  ActualFlow
                  ActualSource
                  ActualSink
                  ActualUse
                  InaccessibleSink
                  BlockedFlow
                  BlockedSource
                  BlockedUse]))
