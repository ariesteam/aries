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
;;; Flood regulation model for Southern California
;;;
;;; Valid Contexts: core.contexts.california/{san-gabriel,west-coyote-hills}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.flood-ca
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defagent defscenario defmodel measurement
                           classification namespace-ontology
                           categorization ranking numeric-coding
                           binary-coding probabilistic-measurement
                           probabilistic-classification identification
                           bayesian count])
  (:refer aries :only [span]))

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

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel precipitation Precipitation
  (classification (measurement habitat:AnnualPrecipitation "mm")
    [600  :>] VeryHighPrecipitation
    [300 600] HighPrecipitation
    [150 300] ModeratePrecipitation
    [ 75 150] LowPrecipitation
    [:<   75] VeryLowPrecipitation))

(defmodel imperviousness california:PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousSurface)
    [80 100 :inclusive]	california:VeryHighImperviousCover
    [50  80]			california:HighImperviousCover
    [20  50]			california:ModeratelyHighImperviousCover
    [10  20]			california:ModeratelyLowImperviousCover
    [ 5  10]			california:LowImperviousCover
    [ 0   5]	   		california:VeryLowImperviousCover))

;; Error in Ferd's code in handling this layer: removing from the
;; context list and setting the prior at a uniform level reflecting
;; the data for the site of interest.
(defmodel rainfall-erosivity RainfallErosivityClass
  (classification (ranking soilRetentionService:RainfallRunoffErosivityIndex)
    [90 :>] VeryHighRainfallErosivity
    [70 89] HighRainfallErosivity
    [50 69] ModerateRainfallErosivity
    [30 49] LowRainfallErosivity
    [:< 29] VeryLowRainfallErosivity))

;; Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel flood-source-training FloodSourceValue
  (classification (measurement habitat:AnnualRunoff "mm")
    [2400   :>]	VeryHighFloodSource
    [1200 2400] HighFloodSource
    [ 600 1200] ModerateFloodSource
    [ 200  600] LowFloodSource
    [:<    200]	VeryLowFloodSource))

(defmodel flood-source-value FloodSourceValue
  (probabilistic-measurement FloodSourceValue "mm"
    [2400 11000] VeryHighFloodSource
    [1200  2400] HighFloodSource
    [ 600  1200] ModerateFloodSource
    [ 200   600] LowFloodSource
    [   0   200] VeryLowFloodSource))

;; Flood source probability, ad hoc method
(defmodel source FloodSource
  (bayesian FloodSource 
    :import	 "aries.core::FloodSourceCaAdHoc.xdsl"
    :context [precipitation imperviousness]
    :keep	 [FloodSourceValue]
    :result  flood-source-value))

(defmodel source-annual Precipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;;;-------------------------------------------------------------------
;;; CN source model
;;;-------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
;;(defmodel source-cn FloodSource
;;	  (measurement habitat:AnnualRunoff "mm" 
;;			:context  [land-use soil-group precipitation (ranking habitat:PercentImperviousSurface)]
;;		    :state    #(let [ctable {(tl/conc 'floodService:Agriculture)		[64 75 82 85]
;;					         		 (tl/conc 'floodService:Forest)			    [64 75 82 85]
;;							         (tl/conc 'floodService:GrassPasture)		[64 75 82 85]
;;							         (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85]}]
;;				  )
;;))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel soil-group california:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 california:SoilGroupA
    2 california:SoilGroupB
    3 california:SoilGroupC
    4 california:SoilGroupD))

(defmodel slope california:SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [:<     1.15] california:Level
    [ 1.15  4.57] california:GentlyUndulating
    [ 4.57 16.70] california:RollingToHilly
    [16.70    :>] california:SteeplyDissectedToMountainous))

;;Use NLCD here, the Vegetation Type SoCal layer provided by Mark doesn't have enough categories to cover the discretization below.
(defmodel vegetation-type california:FloodVegetationType
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}		     california:Wetland
    #{41 42 43 52 71 81} california:ForestGrasslandShrubland
    #{21 22 23 24 31 82} california:DevelopedCultivated))

(defmodel percent-canopy-cover california:PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive] california:VeryHighCanopyCover
    [60  80]			california:HighCanopyCover
    [40  60]		    california:ModerateCanopyCover
    [20  40]			california:LowCanopyCover
    [ 0  20]			california:VeryLowCanopyCover))

;; Problems with coarse-grain pixels; removed this from the bayesian
;; statement and set the prior to its actual value from the data
;; (LowActualEvapotranspiration) - a good temporary solution for WCH
;; but change if you ran it again for Southern California.
(defmodel evapotranspiration EvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
    [45 60] VeryHighEvapotranspiration
    [30 45] HighEvapotranspiration
    [20 30] ModerateEvapotranspiration
    [10 20] LowEvapotranspiration
    [ 0 10] VeryLowEvapotranspiration))

(defmodel infiltration california:SoilInfiltrationClass
  (classification (measurement waterSupplyService:SoilInfiltrationClass "mm")
    [150 200] california:VeryHighSoilInfiltration
    [100 150] california:HighSoilInfiltration
    [ 25 100] california:ModerateSoilInfiltration
    [ 10  25] california:LowSoilInfiltration
    [  0  10] california:VeryLowSoilInfiltration))

;; Doing this one like the detention basin model for Puget Sound.
(defmodel dam-storage DamStorage
  (measurement DamStorage "mm" 
	:context [(binary-coding DamStoragePresence)]
	:state   #(cond (== (:dam-storage-presence %) 0) 0
                    (== (:dam-storage-presence %) 1) 10000)))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification	 (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage california:GreenInfrastructureStorage
  (probabilistic-measurement GreenInfrastructureStorage "mm" 
    [115 260] california:VeryHighGreenStorage
    [ 72 115] california:HighGreenStorage
    [ 40  72] california:ModerateGreenStorage
    [ 15  40] california:LowGreenStorage
    [  0  15] california:VeryLowGreenStorage))

;; Flood sink probability
(defmodel green-infrastructure-sink california:GreenInfrastructureSink
  (bayesian california:GreenInfrastructureSink 
    :import	  "aries.core::FloodSinkCa.xdsl"
    :context  [soil-group slope imperviousness percent-canopy-cover vegetation-type land-selector]
    :required [LandOrSea]
    :keep	  [GreenInfrastructureStorage]
    :result	  green-infrastructure-storage))

(defmodel sink-annual FloodSink
  (measurement FloodSink "mm"
	:context (green-infrastructure-sink dam-storage) 
	:state   #(+ 
               (if (nil? (:green-infrastructure-sink %)) 0.0 (.getMean (:green-infrastructure-sink %)))
               (or		 (:dam-storage %)   0.0))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

;; Uses for all beneficiary groups are defined for both the 100- and
;; 500-year floodplain.  These are then incorporated into the
;; identification and SPAN statements so beneficiary and flow maps can
;; be run for both spatial extents of floodplains.
(defmodel floodplains-100 Floodplains100
  (classification (categorization geofeatures:Floodplain)
    #{"ANI" "X" "X500"} NotIn100YrFloodplain
    #{"A" "AO"}		    In100YrFloodplain))

(defmodel floodplains-500 Floodplains500
  (classification (categorization geofeatures:Floodplain)
    #{"ANI" "X"}	   NotIn500YrFloodplain
    #{"A" "AO" "X500"} In500YrFloodplain))

(defmodel floodplains-100-code Floodplains100Code
  (binary-coding Floodplains100Code
    :context [(categorization geofeatures:Floodplain)]
    :state   #(if (= (:floodplain %) "A") 1 0)))

(defmodel floodplains-500-code Floodplains500Code
  (binary-coding Floodplains500Code
    :context [(categorization geofeatures:Floodplain)]
    :state   #(if (contains? #{"A" "X500"} (:floodplain %)) 1 0)))

(defmodel public-asset PublicAsset
  "Public assets are defined as presence of highways, railways or
both. Other classes of public infrastructure could be added to this
list if desired."
  (classification PublicAsset 
    :context [(binary-coding infrastructure:Highway)
              (binary-coding infrastructure:Railway)]
    :state   #(if (or (:highway %) (:railway %))
                (tl/conc 'floodService:PublicAssetPresent) 
                (tl/conc 'floodService:PublicAssetAbsent))))

(defmodel farmland Farmland
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    82		   FarmlandPresent
    :otherwise FarmlandAbsent
										;	 :agent		"aries/flood/farm"
    :editable	 true))

;; Uses NLCD where parcel data are unavailable. Assumes (incorrectly) that all developed land is housing.
(defmodel housing aestheticService:PresenceOfHousing
  (classification (numeric-coding nlcd:NLCDNumeric)
    [22 23 24] aestheticService:HousingPresent
    :otherwise aestheticService:HousingAbsent))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-use-100 FloodFarmersUse100
  (binary-coding FloodFarmersUse100
    :context [farmland floodplains-100]
    :state   #(if (and (= (tl/conc 'floodService:In100YrFloodplain) (:floodplains100 %))
                       (= (tl/conc 'floodService:FarmlandPresent)	(:farmland %)))
                1
                0)))

(defmodel farmers-use-500 FloodFarmersUse500
  (binary-coding FloodFarmersUse500
    :context [farmland floodplains-500]
    :state   #(if (and (= (tl/conc 'floodService:In500YrFloodplain) (:floodplains500 %))
                       (= (tl/conc 'floodService:FarmlandPresent)	(:farmland %)))
                1
                0)))

;; Models public infrastructure in the floodplain via basic spatial overlap.
(defmodel public-use-100 FloodPublicAssetsUse100
  (binary-coding FloodPublicAssetsUse100
    :context  [public-asset floodplains-100]
    :state    #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
                 1
                 0)))

(defmodel public-use-500 FloodPublicAssetsUse500
  (binary-coding FloodPublicAssetsUse500
    :context  [public-asset floodplains-500]
    :state    #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
                 1
                 0)))

;; Models housing in the floodplain via basic spatial overlap.
(defmodel residents-use-100 FloodResidentsUse100
  (binary-coding FloodResidentsUse100
    :context [housing floodplains-100]
    :state   #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                       (= (tl/conc 'aestheticService:HousingPresent) (:presence-of-housing %)))
                1
                0)))

(defmodel residents-use-500 FloodResidentsUse500
  (binary-coding FloodResidentsUse500
    :context [housing floodplains-500]
    :state   #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
                       (= (tl/conc 'aestheticService:HousingPresent) (:presence-of-housing %)))
                1
                0)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

;;Levees and floodplain width: used in the flow model
;;No data for levees in Orange County at this point but leaving the defmodel statement in for now.	   
(defmodel levees Levees
  "Presence of a levee in given context"
  (classification (binary-coding infrastructure:Levee)
    1 LeveesPresent
    0 LeveesAbsent
										;	 :agent "aries/flood/levee"
    ))

(defmodel floodplain-width FloodplainWidth
  (classification (measurement habitat:FloodplainWidth "m")
    [400  :>]  HighFloodplainWidth
    [200 400]  ModerateFloodplainWidth
    [:<  200]  LowFloodplainWidth
    :otherwise NoFloodplainWidth))

(defmodel flood-flow-data100 TempFloodData100
  (identification TempFloodData100
	:context [altitude streams floodplains-100]))

(defmodel flood-flow-data500 TempFloodData500
  (identification TempFloodData500
	:context [altitude streams floodplains-500]))

(defmodel data-farmers-100 AvoidedFarmDamage100
  (identification AvoidedFarmDamage100
    :context [source-annual sink-annual farmers-use-100 flood-flow-data100]))

(defmodel data-farmers-500 AvoidedFarmDamage500
  (identification AvoidedFarmDamage500
    :context [source-annual sink-annual farmers-use-500 flood-flow-data500]))

(defmodel data-public-100 AvoidedPublicAssetDamage100
  (identification AvoidedPublicAssetDamage100
    :context [source-annual sink-annual public-use-100 flood-flow-data100]))

(defmodel data-public-500 AvoidedPublicAssetDamage500
  (identification AvoidedPublicAssetDamage500
    :context [source-annual sink-annual public-use-500 flood-flow-data500]))

(defmodel data-residents-100 AvoidedDamageToResidents100 
  (identification AvoidedDamageToResidents100 
    :context [source-annual sink-annual residents-use-100 flood-flow-data100]))

(defmodel data-residents-500 AvoidedDamageToResidents500 
  (identification AvoidedDamageToResidents500 
    :context [source-annual sink-annual residents-use-500 flood-flow-data500]))

(defmodel data-farmers-100-sj AvoidedFarmDamage100
  (identification AvoidedFarmDamage100
    :context [source-annual green-infrastructure-sink farmers-use-100 flood-flow-data100]))

(defmodel data-farmers-500-sj AvoidedFarmDamage500
  (identification AvoidedFarmDamage500
    :context [source-annual green-infrastructure-sink farmers-use-500 flood-flow-data500]))

(defmodel data-public-100-sj AvoidedPublicAssetDamage100
  (identification AvoidedPublicAssetDamage100
    :context [source-annual green-infrastructure-sink public-use-100 flood-flow-data100]))

(defmodel data-public-500-sj AvoidedPublicAssetDamage500
  (identification AvoidedPublicAssetDamage500
    :context [source-annual green-infrastructure-sink public-use-500 flood-flow-data500]))

(defmodel data-residents-100-sj AvoidedDamageToResidents100 
  (identification AvoidedDamageToResidents100 
    :context [source-annual green-infrastructure-sink residents-use-100 flood-flow-data100]))

(defmodel data-residents-500-sj AvoidedDamageToResidents500 
  (identification AvoidedDamageToResidents500 
    :context [source-annual green-infrastructure-sink residents-use-500 flood-flow-data500]))

;;(defmodel data-private AvoidedDamageToPrivateAssets 
;; (identification AvoidedDamageToPrivateAssets 
;;	  :context [source sink-annual private-use]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;; Flow model for farmers in the 100-year floodplain   
(defmodel flood-regulation-farmers-100 AvoidedDamageToFarms100
  (span FloodWaterMovement
		Precipitation
		FloodFarmersUse100
		FloodSink
		nil 
		(geophysics:Altitude geofeatures:River Floodplains100Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		 0.0	 ; Set at zero since output values for this are a 0/1
		:trans-threshold	 5.0	 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 1
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers100.clj")
		:context [source-annual farmers-use-100 sink-annual altitude streams floodplains-100-code]
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

;; Flow model for farmers in the 500-year floodplain  
(defmodel flood-regulation-farmers-500 AvoidedDamageToFarms500
  (span FloodWaterMovement 
		Precipitation
		FloodFarmersUse500
		FloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers500.clj")
		:context [source-annual farmers-use-500 sink-annual altitude streams floodplains-500-code]
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

;; Flow model for public-assets in the 100-year floodplain
(defmodel flood-regulation-public-assets-100 AvoidedDamageToPublicAssets100
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse100
		FloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains100Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public100.clj")
		:context [source-annual public-use-100 sink-annual altitude streams floodplains-100-code]
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

;; Flow model for public-assets in the 500-year floodplain
(defmodel flood-regulation-public-assets-500 AvoidedDamageToPublicAssets500
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse500
		FloodSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public500.clj")
		:context [source-annual public-use-500 sink-annual altitude streams floodplains-500-code]
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

;; Flow model for residents in the 100-year floodplain
(defmodel flood-regulation-residents-100 AvoidedDamageToResidents100
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     30.0     ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 8        ; Originally set at 1; bumped it to 8 in order to run models at high enough resolution to produce a continuous streams layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_regulation_residents_100_puget_data.clj")
        :context            [source-annual residents-use-100 sink-annual altitude streams floodplains-100-code]
        :keep               [TheoreticalSource
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

;; Flow model for residents in the 500-year floodplain
(defmodel flood-regulation-residents-500 AvoidedDamageToResidents500
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     30.0     ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 3        ; Originally set at 1; bumped it to 3 in order to run models at high enough resolution to produce a continuous streams layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :context            [source-annual residents-use-500 sink-annual altitude streams floodplains-500-code]
        :keep               [TheoreticalSource
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

;; Models below are for San Joaquin: need to use GreenInfrastructureSink instead of FloodSink
;; Flow model for farmers in the 100-year floodplain   
(defmodel flood-regulation-farmers-100-sj AvoidedDamageToFarms100
  (span FloodWaterMovement
		Precipitation
		FloodFarmersUse100
		GreenInfrastructureSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains100Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 1  ; MUST NOT trigger resampling! Fucking hydrosheds extent is prime!
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers100.clj")
		:context [source-annual farmers-use-100 green-infrastructure-sink altitude streams floodplains-100-code]
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

;; Flow model for farmers in the 500-year floodplain  
(defmodel flood-regulation-farmers-500-sj AvoidedDamageToFarms500
  (span FloodWaterMovement 
		Precipitation
		FloodFarmersUse500
		GreenInfrastructureSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_farmers500.clj")
		:context [source-annual farmers-use-500 green-infrastructure-sink altitude streams floodplains-500-code]
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

;; Flow model for public-assets in the 100-year floodplain
(defmodel flood-regulation-public-assets-100-sj AvoidedDamageToPublicAssets100
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse100
		GreenInfrastructureSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains100Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public100.clj")
		:context [source-annual public-use-100 green-infrastructure-sink altitude streams floodplains-100-code]
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

;; Flow model for public-assets in the 500-year floodplain
(defmodel flood-regulation-public-assets-500-sj AvoidedDamageToPublicAssets500
  (span FloodWaterMovement
		Precipitation
		FloodPublicAssetsUse500
		GreenInfrastructureSink
		nil
		(geophysics:Altitude geofeatures:River Floodplains500Code)
		:source-threshold	50.0	 ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
		:sink-threshold		30.0	 ; Considering moderate, high, and very high flood sinks
		:use-threshold		0.0		 ; Set at zero since output values for this are a 0/1
		:trans-threshold	5.0		 ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
		:source-type		:finite
		:sink-type			:finite
		:use-type			:infinite
		:benefit-type		:non-rival
		:downscaling-factor 8
		:rv-max-states		10
		;;:save-file		  (str (System/getProperty "user.home") "/flood_ca_data_public500.clj")
		:context [source-annual public-use-500 green-infrastructure-sink altitude streams floodplains-500-code]
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

;; Flow model for residents in the 100-year floodplain
(defmodel flood-regulation-residents-100-sj AvoidedDamageToResidents100
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse100
        GreenInfrastructureSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     30.0     ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 8        ; Originally set at 1; bumped it to 8 in order to run models at high enough resolution to produce a continuous streams layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_regulation_residents_100_puget_data.clj")
        :context            [source-annual residents-use-100 green-infrastructure-sink altitude streams floodplains-100-code]
        :keep               [TheoreticalSource
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

;; Flow model for residents in the 500-year floodplain
(defmodel flood-regulation-residents-500-sj AvoidedDamageToResidents500
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse500
        GreenInfrastructureSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     30.0     ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 3        ; Originally set at 1; bumped it to 3 in order to run models at high enough resolution to produce a continuous streams layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :context            [source-annual residents-use-500 green-infrastructure-sink altitude streams floodplains-500-code]
        :keep               [TheoreticalSource
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