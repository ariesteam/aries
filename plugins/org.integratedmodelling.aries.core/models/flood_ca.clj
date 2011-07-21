;;; Copyright 2011 The ARIES Consortium
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
;;; Flood regulation model for Southern California
;;;
;;; Valid Contexts: core.contexts.beta/{ca_mark_watershed,ca_mark}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.flood-ca
  (:refer-clojure :rename {count length})
  (:refer modelling :only (defagent defscenario defmodel measurement classification
							namespace-ontology categorization ranking numeric-coding binary-coding
							probabilistic-measurement probabilistic-classification
							identification bayesian count))
  (:refer aries :only (span)))

(namespace-ontology floodService
  (representation:GenericObservable
   (TempFloodData100)
   (TempFloodData500))
  (thinklab-core:BooleanRanking
   (LandOrSea
    (OnLand) (NotOnLand))))

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))   

(defmodel flow-direction geophysics:FlowDirection
  (ranking geophysics:FlowDirection)) 

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel precipitation Precipitation
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [:< 75]	  VeryLowPrecipitation
    [75 150]	  LowPrecipitation
    [150 300]	  ModeratePrecipitation
    [300 600]	  HighPrecipitation
    [600 :>]	  VeryHighPrecipitation))

(defmodel imperviousness PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousness)
    [80 100 :inclusive]	 VeryHighImperviousCover
    [50 80]				 HighImperviousCover
    [20 50]				 ModeratelyHighImperviousCover
    [10 20]				 ModeratelyLowImperviousCover
    [5 10]				 LowImperviousCover
    [0 5]					 VeryLowImperviousCover))

;;Error in Ferds code in handling this layer: removing from the context list and setting the prior
;; at a uniform level reflecting the data for the site of interest.
(defmodel rainfall-erosivity RainfallErosivityClass
  (classification (ranking soilRetentionService:RainfallRunoffErosivityIndex)
    [90 :>]  VeryHighRainfallErosivity
    [70 89]  HighRainfallErosivity
    [50 69]  ModerateRainfallErosivity
    [30 49]  LowRainfallErosivity
    [:< 29]  VeryLowRainfallErosivity)) 

;;Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel flood-source-training FloodSourceValue
  (classification (measurement habitat:AnnualRunoff "mm")
    [:< 200]	  VeryLowFloodSource
    [200 600]	  LowFloodSource
    [600 1200]  ModerateFloodSource
    [1200 2400] HighFloodSource
    [2400 :>]	  VeryHighFloodSource))

(defmodel flood-source-value FloodSourceValue
  (probabilistic-measurement FloodSourceValue "mm"
    [0 200]		 VeryLowFloodSource
    [200 600]		 LowFloodSource
    [600 1200]	 ModerateFloodSource
    [1200 2400]	 HighFloodSource
    [2400 11000]	 VeryHighFloodSource))

;; Flood source probability, ad hoc method
(defmodel source FloodSource
  (bayesian FloodSource 
    :import	  "aries.core::FloodSourceCaAdHoc.xdsl"
    :context  (precipitation imperviousness)
    :keep	  (FloodSourceValue)
    :observed (flood-source-value)))

;;;-------------------------------------------------------------------
;;; CN source model
;;;-------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
;;(defmodel source-cn FloodSource
;;	  (measurement habitat:AnnualRunoff "mm" 
;;			:context  (land-use :as landuse 
;;				   soil-group-puget :as soilgroup
;;				   (ranking habitat:PercentImperviousness) :as imperv
;;				   precipitation :as precipitation)
;;		:state #(let [ctable {(tl/conc 'floodService:Agriculture)		 [64 75 82 85]
;;							  (tl/conc 'floodService:Forest)			 [64 75 82 85]
;;							  (tl/conc 'floodService:GrassPasture)		 [64 75 82 85]
;;							  (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85]}]
;;				  )
;;))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel soil-group HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1		   SoilGroupA
    2		   SoilGroupB
    3		   SoilGroupC
    4		   SoilGroupD))

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:< 1.15]	   Level
    [1.15 4.57]  GentlyUndulating
    [4.57 16.70] RollingToHilly
    [16.70 :>]   SteeplyDissectedToMountainous))

;;Use NLCD here, the Vegetation Type SoCal layer provided by Mark doesn't have enough categories to cover the discretization below.
(defmodel vegetation-type southernCalifornia:VegetationTypeSoCalFlood
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}			 southernCalifornia:WetlandVegetation
    #{41 42 43 52 71}	 southernCalifornia:ForestGrasslandShrublandVegetation
    #{21 22 23 24 82}	 southernCalifornia:DevelopedCultivatedVegetation))

(defmodel percent-vegetation-cover PercentVegetationCoverClass
  (classification (ranking habitat:PercentVegetationCover)
    [80 100 :inclusive] VeryHighVegetationCover
    [60 80]			  HighVegetationCover
    [40 60]			  ModerateVegetationCover
    [20 40]			  LowVegetationCover
    [0 20]			  VeryLowVegetationCover))

;;Problems with coarse-grain pixels; removed this from the bayesian statement and set the prior
;; to its actual value from the data (LowActualEvapotranspiration) - a good temporary solution for
;; WCH but change if you ran it again for Southern California.
(defmodel evapotranspiration EvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
    [90 :>]	 VeryHighEvapotranspiration
    [60 90]	 HighEvapotranspiration
    [30 60]	 ModerateEvapotranspiration
    [12 30]	 LowEvapotranspiration
    [0 12]	 VeryLowEvapotranspiration)) 

(defmodel infiltration SoilInfiltrationClass
  (classification (measurement habitat:AnnualInfiltration "mm")
    [25 :>]	VeryHighSoilInfiltration
    [13 25]	HighSoilInfiltration
    [8 13]	ModerateSoilInfiltration
    [3 8]		LowSoilInfiltration
    [0 3]		VeryLowSoilInfiltration))

;;Doing this one like the detention basin model for Puget Sound.
(defmodel dam-storage DamStorage
  (measurement DamStorage "mm" 
	:context ((binary-coding DamStoragePresence :as dam-storage))
	:state #(cond (== (:dam-storage %) 0) 0
				  (== (:dam-storage %) 1) 10000)))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification	 (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage GreenInfrastructureStorage
  (probabilistic-measurement GreenInfrastructureStorage "mm" 
    [115 320]	   VeryHighGreenStorage
    [72 115]	   HighGreenStorage
    [40 72]	   ModerateGreenStorage
    [15 40]	   LowGreenStorage
    [0 15]	   VeryLowGreenStorage))

;; Flood sink probability
;; TODO missing data
(defmodel green-infrastructure-sink GreenInfrastructureSink
  (bayesian GreenInfrastructureSink 
    :import	  "aries.core::FloodSinkCa.xdsl"
    :keep	  (GreenInfrastructureStorage)
    :required (LandOrSea)
    :context  (soil-group slope imperviousness percent-vegetation-cover vegetation-type land-selector)
    :result	   green-infrastructure-storage))

(defmodel sink-annual FloodSink
  (measurement FloodSink "mm"
	:context (green-infrastructure-sink :as green-infrastructure dam-storage :as gray-infrastructure) 
	:state #(+ 
             (if (nil? (:green-infrastructure %)) 0.0 (.getMean (:green-infrastructure %)))
             (or		(:gray-infrastructure %)   0.0))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;;Uses for all beneficiary groups are defined for both the 100- and 500-year floodplain.  These are then incorporated into 
;; the identification and SPAN statements so beneficiary and flow maps can be run for both spatial extents of floodplains.
(defmodel floodplains-100 Floodplains100
  (classification (categorization geofeatures:Floodplain)
    #{"ANI" "X" "X500"} NotIn100YrFloodplain
    #{"A" "AO"}		  In100YrFloodplain))

(defmodel floodplains-500 Floodplains500
  (classification (categorization geofeatures:Floodplain)
    #{"ANI" "X"}		 NotIn500YrFloodplain
    #{"A" "AO" "X500"} In500YrFloodplain))

(defmodel public-asset PublicAsset
  "Public assets are defined as presence of highways, railways or both.	 Other classes of public infrastructure could
be added to this list if desired."
  (classification PublicAsset 
    :state   #(if (> (+ (:highway %) (:railway %)) 0) 
                (tl/conc 'floodService:PublicAssetPresent) 
                (tl/conc 'floodService:PublicAssetAbsent))
    :context ((ranking infrastructure:Highway)
              (ranking infrastructure:Railway))))

(defmodel farmland Farmland
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    82		 FarmlandPresent
    :otherwise FarmlandAbsent
										;	 :agent		"aries/flood/farm"
    :editable	 true))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-use-100 FloodFarmersUse100
  (binary-coding FloodFarmersUse100
    :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains100 %))
                     (= (tl/conc 'floodService:FarmlandPresent)	   (:farmland %)))
              1
              0)
    :context (farmland floodplains-100)))

(defmodel farmers-use-500 FloodFarmersUse500
  (binary-coding FloodFarmersUse500
    :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains500 %))
                     (= (tl/conc 'floodService:FarmlandPresent)	   (:farmland %)))
              1
              0)
    :context (farmland floodplains-500)))

;; Models public infrastructure in the floodplain via basic spatial overlap.
(defmodel public-use-100 FloodPublicAssetsUse100
  (binary-coding FloodPublicAssetsUse100
    :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                     (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
              1
              0)
    :context	 (public-asset floodplains-100)))

(defmodel public-use-500 FloodPublicAssetsUse500
  (binary-coding FloodPublicAssetsUse500
    :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
                     (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
              1
              0)
    :context	 (public-asset floodplains-500)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel flood-flow-data100 TempFloodData100
  (identification TempFloodData100
	:context (altitude streams floodplains-100)))

(defmodel flood-flow-data500 TempFloodData500
  (identification TempFloodData500
	:context (altitude streams floodplains-500)))

(defmodel data-farmers-100 AvoidedFarmDamage100
  (identification AvoidedFarmDamage100
    :context (source sink-annual farmers-use-100 flood-flow-data100)))

(defmodel data-farmers-500 AvoidedFarmDamage500
  (identification AvoidedFarmDamage500
    :context (source sink-annual farmers-use-500 flood-flow-data500)))

(defmodel data-public-100 AvoidedPublicAssetDamage100
  (identification AvoidedPublicAssetDamage100
    :context (source sink-annual public-use-100 flood-flow-data100)))

(defmodel data-public-500 AvoidedPublicAssetDamage500
  (identification AvoidedPublicAssetDamage500
    :context (source sink-annual public-use-500 flood-flow-data500)))

;;(defmodel data-private AvoidedDamageToPrivateAssets 
;;(identification AvoidedDamageToPrivateAssets 
;;				:context (source sink-annual private-use)))

;;(defmodel data-residents AvoidedDamageToResidents 
;;(identification AvoidedDamageToResidents 
;;				:context (source sink-annual residents-use)))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; flow model for farmers in the 100-year floodplain   
(defmodel flood-regulation-farmers-100 AvoidedDamageToFarms100
  (span FloodWaterMovement
		Precipitation
		FloodFarmersUse100
		AnnualFloodSink
		nil 
		(geophysics:Altitude geofeatures:River Floodplains100)
		:source-threshold	50.0	 ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		3000.0	 ;; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ;;Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 1  ;; MUST NOT trigger resampling! Fucking hydrosheds extent is prime!
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers100.clj")
		:keep (Runoff
			   PotentialRunoffMitigation
			   PotentiallyVulnerablePopulations
			   PotentiallyDamagingFloodFlow
			   PotentiallyDamagingRunoff
			   PotentialFloodDamageReceived
			   ActualFloodFlow
			   FloodDamagingRunoff
			   UtilizedRunoffMitigation
			   FloodDamageReceived
			   BenignRunoff
			   UnutilizedRunoffMitigation
			   AbsorbedFloodFlow
			   FloodMitigatedRunoff
			   FloodMitigationBenefitsAccrued)
		:context (source farmers-use-100 sink-annual flood-flow-data100)))

;; flow model for farmers in the 500-year floodplain  
(defmodel flood-regulation-farmers-500 AvoidedDamageToFarms500
  (span FloodWaterMovement 
		Precipitation
		FloodFarmersUse500
		AnnualFloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500)
		:source-threshold	50.0	 ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		3000.0	 ;; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ;;Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers500.clj")
		:keep (Runoff
			   PotentialRunoffMitigation
			   PotentiallyVulnerablePopulations
			   PotentiallyDamagingFloodFlow
			   PotentiallyDamagingRunoff
			   PotentialFloodDamageReceived
			   ActualFloodFlow
			   FloodDamagingRunoff
			   UtilizedRunoffMitigation
			   FloodDamageReceived
			   BenignRunoff
			   UnutilizedRunoffMitigation
			   AbsorbedFloodFlow
			   FloodMitigatedRunoff
			   FloodMitigationBenefitsAccrued)
		:context (source farmers-use-500 sink-annual flood-flow-data500)))

;; flow model for public-assets in the 100-year floodplain
(defmodel flood-regulation-public-assets-100 AvoidedDamageToPublicAssets100
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse100
		AnnualFloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains100)
		:source-threshold	50.0	 ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		3000.0	 ;; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ;;Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public100.clj")
		:keep (Runoff
			   PotentialRunoffMitigation
			   PotentiallyVulnerablePopulations
			   PotentiallyDamagingFloodFlow
			   PotentiallyDamagingRunoff
			   PotentialFloodDamageReceived
			   ActualFloodFlow
			   FloodDamagingRunoff
			   UtilizedRunoffMitigation
			   FloodDamageReceived
			   BenignRunoff
			   UnutilizedRunoffMitigation
			   AbsorbedFloodFlow
			   FloodMitigatedRunoff
			   FloodMitigationBenefitsAccrued)
		:context (source public-use-100 sink-annual flood-flow-data100)))

;; flow model for public-assets in the 500-year floodplain
(defmodel flood-regulation-public-assets-500 AvoidedDamageToPublicAssets500
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse500
		AnnualFloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500)
		:source-threshold	50.0	 ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		3000.0	 ;; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ;;Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public500.clj")
		:keep (Runoff
			   PotentialRunoffMitigation
			   PotentiallyVulnerablePopulations
			   PotentiallyDamagingFloodFlow
			   PotentiallyDamagingRunoff
			   PotentialFloodDamageReceived
			   ActualFloodFlow
			   FloodDamagingRunoff
			   UtilizedRunoffMitigation
			   FloodDamageReceived
			   BenignRunoff
			   UnutilizedRunoffMitigation
			   AbsorbedFloodFlow
			   FloodMitigatedRunoff
			   FloodMitigationBenefitsAccrued)
		:context (source public-use-500 sink-annual flood-flow-data500)))

;;Levees and floodplain width: used in the flow model
;;No data for levees in Orange County at this point but leaving the defmodel statement in for now.	   
(defmodel levees Levees
  "Presence of a levee in given context"
  (classification (binary-coding infrastructure:Levee)
    0 LeveesAbsent
    1 LeveesPresent
										;	 :agent "aries/flood/levee"
    ))
(defmodel floodplain-width FloodplainWidth
  (classification (measurement habitat:FloodplainWidth "m")
    [400 :>]	   HighFloodplainWidth
    [200 400]	   ModerateFloodplainWidth
    [:< 200]	   LowFloodplainWidth
    :otherwise   NoFloodplainWidth))
