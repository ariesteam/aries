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
;;; Flood regulation model for Western Washington
;;;
;;; Valid Contexts: core.contexts.beta/{chehalis,wria9,viewshed,western_wa}*
;;;
;;;-------------------------------------------------------------------

(ns core.models.flood-puget
  (:refer-clojure :rename {count length}) 
  (:refer tl :only [is? conc])
  (:refer modelling :only [defagent defscenario defmodel measurement
                           classification model namespace-ontology
                           categorization ranking numeric-coding
                           probabilistic-measurement
                           probabilistic-classification binary-coding
                           identification bayesian count])
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

(defmodel flow-direction geophysics:FlowDirection
  (ranking geophysics:FlowDirection)) 

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel soil-group-puget HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
    1 SoilGroupA
    2 SoilGroupB
    3 SoilGroupC
    4 SoilGroupD))

;; This layer has problems for now (see .xml) but not currently used.
;;(defmodel precipitation-monthly Precipitation
;;  (measurement habitat:JanuaryPrecipitation "mm"))

(defmodel land-use LandUseLandCover
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    82             Agriculture
    #{11 90 95 12} WetlandsOpenWater
    21             DevelopedOpenSpace
    #{41 42 43}    Forest
    #{71 81 52}    GrassPasture
    #{22    31}    DevelopedLowIntensity
    23             DevelopedMediumIntensity
    24             DevelopedHighIntensity))

;; Surface temperature - again, should be monthly and matched by
;; temporal extents.  For mean temperature could use a slightly lower
;; discretization, i.e., < -4, -4 to 4, >4
(defmodel monthly-temperature MonthlyMaximumTemperature
  (classification (measurement geophysics:JanuaryMaximumGroundSurfaceTemperature "\u00b0C")
    [6 :>] HighMonthlyMaximumTemperature
    [0  6] ModerateMonthlyMaximumTemperature
    [:< 0] LowMonthlyMaximumTemperature))

(defmodel annual-temperature AnnualMaximumTemperature
  (classification (measurement geophysics:AnnualMaximumGroundSurfaceTemperature "\u00b0C")
    [6 :>] HighAnnualMaximumTemperature
    [0  6] ModerateAnnualMaximumTemperature
    [:< 0] LowAnnualMaximumTemperature)) 

;; Snow presence - only the puget-specific statement for now.  This is
;; not currently part of any model but could be incorporated in the
;; future.
(defmodel snow-presence SnowPresence
  (classification (categorization puget:SnowPrecipitationCategory)
    #{"LL" "HL"} LowlandAndHighland
    #{"RD" "SD"} RainDominatedAndSnowDominated
    "RS"         PeakRainOnSnow))

;; This discretization produces uniform snowmelt results everywhere -
;; consider altering if these data are actually used in another model
;; statement.
(defmodel snowmelt-annual AnnualSnowmelt
  (classification (measurement habitat:AnnualSnowmelt "mm")
    [700 :>]           HighAnnualSnowmelt
    [250 700]          ModerateAnnualSnowmelt
    [:exclusive 0 250] LowAnnualSnowmelt
    [0]                NoAnnualSnowmelt))

(defmodel snowmelt-monthly MonthlySnowmelt
  (measurement habitat:JanuarySnowmelt "mm"))

;;Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel runoff-training FloodSourceValue
  (classification (measurement habitat:AnnualRunoff "mm")
    [2400   :>] VeryHighFloodSource
    [1200 2400] HighFloodSource
    [ 600 1200] ModerateFloodSource
    [ 200  600] LowFloodSource
    [:<    200] VeryLowFloodSource))

;;Monthly source data is just the sum of precipitation and snowmelt.
;;(defmodel source-monthly FloodSourceMonthly
;;  (measurement FloodSourceMonthly "mm"
;;    :context [precipitation-monthly snowmelt-monthly]
;;    :state   #(+ (:precipitation-monthly %) (:snowmelt-monthly %)))) 

;; Annual source data is simply precipitation-annual (assume all snow
;; melts in each year, which is true everywhere but for glaciers.
;; Assume that glaciers are neither gaining nor losing mass, which is
;; not true but a simplifying assumption for now.
(defmodel source-annual Precipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;;;-------------------------------------------------------------------
;;; CN source model
;;;-------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
(defmodel source-cn FloodSource
  (measurement habitat:AnnualRunoff "mm" 
    :context [land-use soil-group-puget source-annual
             (ranking habitat:PercentImperviousSurface)]
    :state   #(let [
                    ctable 
                    {(tl/conc 'floodService:Agriculture) [64 75 82 85],
                     (tl/conc 'floodService:Forest) [64 75 82 85],
                     (tl/conc 'floodService:GrassPasture) [64 75 82 85],
                     (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85],
                     (tl/conc 'floodService:Agriculture) [64 75 82 85]}
                    ]
                )
    ))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel slope SlopeClass
  (classification (measurement geophysics:DegreeSlope "\u00b0")
    [   :<  1.15] Level
    [ 1.15  4.57] GentlyUndulating
    [ 4.57 16.70] RollingToHilly
    [16.70    :>] SteeplyDissectedToMountainous))

(defmodel vegetation-type puget:FloodVegetationType
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    #{90 95}          puget:Wetland
    #{41 42 43 52 71} puget:ForestGrasslandShrubland
    #{21 22 23 24 82} puget:DevelopedCultivated))

(defmodel vegetation-height VegetationHeight
  (classification (measurement habitat:VegetationHeight "ft")
    [120  :>] VeryHighVegetationHeight
    [ 80 120] HighVegetationHeight
    [ 50  80] ModerateVegetationHeight
    [ 20  50] LowVegetationHeight
    [ :<  20] VeryLowVegetationHeight))

(defmodel percent-canopy-cover PercentTreeCanopyCoverClass
  (classification (ranking habitat:PercentTreeCanopyCover)
    [80 100 :inclusive] VeryHighCanopyCover
    [60  80]            HighCanopyCover
    [40  60]            ModerateCanopyCover
    [20  40]            LowCanopyCover
    [ 0  20]            VeryLowCanopyCover))

(defmodel successional-stage SuccessionalStage
  (classification (ranking ecology:SuccessionalStage)
    #{5 6}                           OldGrowth
    4                                LateSuccession
    3                                MidSuccession
    2                                PoleSuccession
    1                                EarlySuccession
    #{21 22 23 24 25 26 27 28 40 41} NoSuccession))

(defmodel imperviousness PercentImperviousCoverClass
  (classification (ranking habitat:PercentImperviousSurface)
    [80 100 :inclusive] VeryHighImperviousCover
    [50  80]            HighImperviousCover
    [20  50]            ModeratelyHighImperviousCover
    [10  20]            ModeratelyLowImperviousCover
    [ 5  10]            LowImperviousCover
    [ 0   5]            VeryLowImperviousCover))

(defmodel evapotranspiration EvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
    [90 :>] VeryHighEvapotranspiration
    [60 90] HighEvapotranspiration
    [30 60] ModerateEvapotranspiration
    [12 30] LowEvapotranspiration
    [ 0 12] VeryLowEvapotranspiration))

(defmodel mean-days-precipitation-monthly MeanDaysPrecipitationPerMonth
  (classification (ranking habitat:JanuaryDaysOfPrecipitation)
    #{8 9}   VeryHighDaysPrecipitationPerMonth
    #{6 7}   HighDaysPrecipitationPerMonth
    #{4 5}   LowDaysPrecipitationPerMonth
    #{1 2 3} VeryLowDaysPrecipitationPerMonth))

(defmodel mean-days-precipitation-annual MeanDaysPrecipitationPerYear
  (classification (ranking habitat:AnnualDaysOfPrecipitation)
    #{8 9}   VeryHighDaysPrecipitationPerYear
    #{6 7}   HighDaysPrecipitationPerYear
    #{4 5}   LowDaysPrecipitationPerYear
    #{1 2 3} VeryLowDaysPrecipitationPerYear))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
  (classification  (measurement geophysics:Altitude "m")
    [:exclusive 0 :>] OnLand))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage GreenInfrastructureStorage
  (probabilistic-measurement GreenInfrastructureStorage "mm" 
    [115 320] VeryHighGreenStorage
    [ 72 115] HighGreenStorage
    [ 40  72] ModerateGreenStorage
    [ 15  40] LowGreenStorage
    [  0  15] VeryLowGreenStorage))

;; Assumes that detention basins average 3 m, i.e., 3000 mm, in depth,
;; i.e., storage capacity when empty.  Can alter this as appropriate.
(defmodel detention-basin-storage DetentionBasinStorage
  (measurement DetentionBasinStorage "mm" 
    :context [(binary-coding infrastructure:DetentionBasin)]
    :state   #(cond (== (:detention-basin %) 0) 0
                    (== (:detention-basin %) 1) 3000)))

(defmodel dam-storage DamStorage
  (measurement DamStorage "mm"))

(defmodel gray-infrastructure-sink GrayInfrastructureSink 
  (measurement GrayInfrastructureSink "mm"
    :context [dam-storage detention-basin-storage]
    :state   #(+ (or (:dam-storage %) 0.0) (or (:detention-basin %) 0.0))))

;; Flood sink probability, monthly (need a monthly flood sink undiscretizer here)
;;(defmodel sink-monthly MonthlyFloodSink
;;    (bayesian MonthlyFloodSink
;;      :import   "aries.core::FloodSinkPugetMonthly.xdsl"
;;      :keep     (
;;              MonthlyFloodSink 
;;              GreenInfrastructureStorage
;;              GrayInfrastructureStorage)
;;      :required (LandOrSea)
;;          :context  (
;;                  soil-group-puget vegetation-type slope monthly-temperature vegetation-height
;;                  successional-stage imperviousness dam-storage detention-basin-storage
;;                  percent-canopy-cover mean-days-precipitation-monthly land-selector)))

;; Flood sink probability, annual Comment veg height back in once the
;; layers been expanded to a meaningful extent OR Ferd's enabled
;; coexistence of small layers + priors for areas without evidence.
(defmodel green-infrastructure-sink GreenInfrastructureSink 
  (bayesian GreenInfrastructureSink
    :import  "aries.core::FloodSinkPugetAnnual.xdsl"
    :context  [soil-group-puget vegetation-type slope ; vegetation-height
               successional-stage imperviousness percent-canopy-cover 
               mean-days-precipitation-annual land-selector]
    :required [LandOrSea]
    :keep     [GreenInfrastructureStorage]
    :result   green-infrastructure-storage))

(defmodel sink-annual FloodSink
  (measurement FloodSink "mm"
    :context [green-infrastructure-sink gray-infrastructure-sink]
    :state   #(+ 
               (if (nil? (:green-infrastructure-sink %)) 0.0 (.getMean (:green-infrastructure-sink %)))
               (or       (:gray-infrastructure-sink %)   0.0))))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel floodplains-100 Floodplains100
  (classification (categorization geofeatures:Floodplain)
    "A"        In100YrFloodplain
    :otherwise NotIn100YrFloodplain))

(defmodel floodplains-500 Floodplains500
  (classification (categorization geofeatures:Floodplain)
    #{"A" "X500"} In500YrFloodplain
    :otherwise    NotIn500YrFloodplain))

(defmodel floodplains-100-code Floodplains100Code
  (binary-coding Floodplains100Code
    :context [(categorization geofeatures:Floodplain)]
    :state   #(if (= (:floodplain %) "A") 1 0)))

(defmodel floodplains-500-code Floodplains500Code
  (binary-coding Floodplains500Code
    :context [(categorization geofeatures:Floodplain)]
    :state   #(if (contains? #{"A" "X500"} (:floodplain %)) 1 0)))

;; KB: Don't seem to have any corresponding data here, but the
;; assumption that structures are in the floodplain wherever there is
;; private land is a bad one.  Let's avoid using this for now.
(defmodel structures Structures
  "Assume that any privately owned land in floodplain has vulnerable
  structures. TODO make more specific when we know more"
  (classification (ranking lulc:PrivatelyOwnedLand)
    0 StructuresAbsent
    1 StructuresPresent))

(defmodel housing aestheticService:PresenceOfHousing
  "Classifies land use from property data."
  (classification (ranking aestheticService:PresenceOfHousing)
    [1 :>]     aestheticService:HousingPresent  
    :otherwise aestheticService:HousingAbsent)
  ;; Uses NLCD where parcel data are unavailable. Assumes (incorrectly) that all developed land is housing.
  (classification (numeric-coding nlcd:NLCDNumeric)
    [22 23 24] aestheticService:HousingPresent
    :otherwise aestheticService:HousingAbsent))

(defmodel public-asset PublicAsset
  "Public assets are defined as presence of highways, railways or both. Other classes of public infrastructure could
be added to this list if desired."
  (classification PublicAsset
    :context [(binary-coding infrastructure:Highway)
              (binary-coding infrastructure:Railway)]
    :state   #(if (or (:highway %) (:railway %)) 
                (tl/conc 'floodService:PublicAssetPresent) 
                (tl/conc 'floodService:PublicAssetAbsent))))

(defmodel farmland Farmland
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding nlcd:NLCDNumeric)
    82         FarmlandPresent
    :otherwise FarmlandAbsent
                                        ;    :agent     "aries/flood/farm"
    :editable  true))

;; Models farmland in the floodplain via basic spatial overlap.
(defmodel farmers-use-100 FloodFarmersUse100
  (binary-coding FloodFarmersUse100
    :context [farmland floodplains-100]
    :state   #(if (and (= (tl/conc 'floodService:In100YrFloodplain) (:floodplains100 %))
                       (= (tl/conc 'floodService:FarmlandPresent)   (:farmland %)))
                1
                0)))

(defmodel farmers-use-500 FloodFarmersUse500
  (binary-coding FloodFarmersUse500
    :context [farmland floodplains-500]
    :state   #(if (and (= (tl/conc 'floodService:In500YrFloodplain) (:floodplains500 %))
                       (= (tl/conc 'floodService:FarmlandPresent)   (:farmland %)))
                1
                0)))

;; Models public infrastructure in the floodplain via basic spatial overlap.
(defmodel public-use-100 FloodPublicAssetsUse100
  (binary-coding FloodPublicAssetsUse100
    :context [public-asset floodplains-100]
    :state   #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                       (= (tl/conc 'floodService:PublicAssetPresent) (:public-asset %)))
                1
                0)))

(defmodel public-use-500 FloodPublicAssetsUse500
  (binary-coding FloodPublicAssetsUse500
    :context [public-asset floodplains-500]
    :state   #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
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

;; Models other private structures in the floodplain via basic spatial overlap.
(defmodel private-use-100 FloodPrivateAssetsUse100
  (binary-coding FloodPrivateAssetsUse100
    :context [structures floodplains-100]
    :state   #(if (and (= (tl/conc 'floodService:In100YrFloodplain) (:floodplains100 %))
                       (= (tl/conc 'floodService:StructuresPresent) (:structures %)))
                1
                0)))

(defmodel private-use-500 FloodPrivateAssetsUse500
  (binary-coding FloodPrivateAssetsUse500
    :context [structures floodplains-500]
    :state   #(if (and (= (tl/conc 'floodService:In500YrFloodplain) (:floodplains500 %))
                       (= (tl/conc 'floodService:StructuresPresent) (:structures %)))
                1
                0)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel flood-flow-data100 TempFloodData100
  (identification TempFloodData100
    :context [altitude streams floodplains-100]))

(defmodel flood-flow-data500 TempFloodData500
  (identification TempFloodData500
    :context [altitude streams floodplains-500]))

;;Levees and floodplain width: used in the flow model
(defmodel levees Levees
  (binary-coding infrastructure:Levee))

(defmodel floodplain-width FloodplainWidth
  (classification (measurement habitat:FloodplainWidth "m")
    [400  :>]  HighFloodplainWidth
    [200 400]  ModerateFloodplainWidth
    [:<  200]  LowFloodplainWidth
    :otherwise NoFloodplainWidth))         

;; These are currently set to "source-annual" but should also be
;; tested for "source-monthly"
(defmodel data-farmers-100 AvoidedDamageToFarms100 
  (identification AvoidedDamageToFarms100 
    :context [source-annual sink-annual farmers-use-100 flood-flow-data100]))

(defmodel data-farmers-500 AvoidedDamageToFarms500 
  (identification AvoidedDamageToFarms500 
    :context [source-annual sink-annual farmers-use-500 flood-flow-data500]))

(defmodel data-public-100 AvoidedDamageToPublicAssets100 
  (identification AvoidedDamageToPublicAssets100 
    :context [source-annual sink-annual public-use-100 flood-flow-data100]))

(defmodel data-public-500 AvoidedDamageToPublicAssets500 
  (identification AvoidedDamageToPublicAssets500 
    :context [source-annual sink-annual public-use-500 flood-flow-data500]))

(defmodel data-private-100 AvoidedDamageToPrivateAssets100 
  (identification AvoidedDamageToPrivateAssets100 
    :context [source-annual sink-annual private-use-100 flood-flow-data100]))

(defmodel data-private-500 AvoidedDamageToPrivateAssets500 
  (identification AvoidedDamageToPrivateAssets500 
    :context [source-annual sink-annual private-use-500 flood-flow-data500]))

(defmodel data-residents-100 AvoidedDamageToResidents100 
  (identification AvoidedDamageToResidents100 
    :context [source-annual sink-annual residents-use-100 flood-flow-data100]))

(defmodel data-residents-500 AvoidedDamageToResidents500 
  (identification AvoidedDamageToResidents500 
    :context [source-annual sink-annual residents-use-500 flood-flow-data500]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

(defmodel flood-regulation-farmers-100 AvoidedDamageToFarms100
  (span FloodWaterMovement
        Precipitation
        FloodFarmersUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual farmers-use-100 sink-annual altitude
                             streams floodplains-100-code levees]
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

(defmodel flood-regulation-farmers-500 AvoidedDamageToFarms500
  (span FloodWaterMovement
        Precipitation
        FloodFarmersUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual farmers-use-500 sink-annual altitude
                             streams floodplains-500-code levees]
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

(defmodel flood-regulation-public-assets-100 AvoidedDamageToPublicAssets100
  (span FloodWaterMovement
        Precipitation
        FloodPublicAssetsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 3        ;;Originally set at 1; bumped it to 3 in order to run models at high enough resolution to produce a continuous streams layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :context            [altitude source-annual public-use-100 sink-annual 
                             streams floodplains-100-code levees]
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

(defmodel flood-regulation-public-assets-500 AvoidedDamageToPublicAssets500
  (span FloodWaterMovement
        Precipitation
        FloodPublicAssetsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual public-use-500 sink-annual altitude
                             streams floodplains-500-code levees]
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

(defmodel flood-regulation-residents-100 AvoidedDamageToResidents100
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ; Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ; Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 3        ; Originally set at 1; bumped it
                                     ; to 3 in order to run models at
                                     ; high enough resolution to
                                     ; produce a continuous streams
                                     ; layer from hydrography data
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_regulation_residents_100_puget_data.clj")
        :context            [source-annual residents-use-100 sink-annual altitude
                             streams floodplains-100-code levees]
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

(defmodel flood-regulation-residents-500 AvoidedDamageToResidents500
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual residents-use-500 sink-annual altitude
                             streams floodplains-500-code levees]
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

;; DO NOT use these flow models for now.  We don't have a way of
;;  explicitly mapping private assests, aside from housing, which is
;;  treated elsewhere.  So for now, just run housing, public
;;  infrastructure, and farmland as the 3 classes of flow models (each
;;  modeled for the 100- and 500-year floodplain).
(defmodel flood-regulation-private-100 AvoidedDamageToPrivateAssets100
  (span FloodWaterMovement
        Precipitation
        FloodPrivateAssetsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual private-use-100 sink-annual altitude
                             streams floodplains-100-code levees]
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

(defmodel flood-regulation-private-500 AvoidedDamageToPrivateAssets500
  (span FloodWaterMovement
        Precipitation
        FloodPrivateAssetsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500Code Levees)
        :source-threshold   50.0     ; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ; Considering moderate, high, and very high flood sinks
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
        :context            [source-annual private-use-500 sink-annual altitude
                             streams floodplains-500-code levees]
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

;;;-------------------------------------------------------------------
;;; Scenarios
;;;-------------------------------------------------------------------

(defmodel constrained-development-scenario puget:ConstrainedDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCConstrained2060) 
    4                                   puget:HighDensityDevelopedConstrained
    6                                   puget:ModerateDensityDevelopedConstrained
    5                                   puget:LowDensityDevelopedConstrained
    7                                   puget:UrbanOpenSpaceConstrained
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedConstrained))

(defmodel open-development-scenario puget:OpenDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCOpen2060) 
    4                                   puget:HighDensityDevelopedOpen
    6                                   puget:ModerateDensityDevelopedOpen
    5                                   puget:LowDensityDevelopedOpen
    7                                   puget:UrbanOpenSpaceOpen
    #{0 1 2 3 8 9 10 11 12 13 14 15 16} puget:NotDevelopedOpen))

(defscenario open-development-flood
  "Changes values in developed areas to no succession, low canopy cover, moderate hardwood-softwood ratio,low fire frequency, increased greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [open-development-scenario :as od percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen)))
                      (conc 'carbonService:VeryLowCanopyCover)
                    
                      (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'carbonService:ModerateCanopyCover)
                    
                      :otherwise (:pcc %))))
  (model SuccessionalStage
    (classification SuccessionalStage
      :context [open-development-scenario :as od successional-stage :as ss]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'floodService:NoSuccession)
                  (:ss %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
      :context [open-development-scenario :as od soil-group-puget :as sg]
      :state   #(if (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen)))                                    
                  (conc 'floodService:SoilGroupD)
                  (:sg))))
  (model puget:FloodVegetationType
    (classification puget:FloodVegetationType
      :context [open-development-scenario :as od vegetation-type :as vt]
      :state   #(cond (or (is? (:od %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:od %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'puget:DevelopedCultivated)

                      (is? (:od %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'puget:ForestGrasslandShrubland)

                      :otherwise (:vt %))))
  (model PercentImperviousCoverClass
    (classification (ranking puget:ENVISIONUrbanGrowthImperviousOpen2060)
      [80 100 :inclusive] VeryHighImperviousCover
      [50  80]            HighImperviousCover
      [20  50]            ModeratelyHighImperviousCover
      [10  20]            ModeratelyLowImperviousCover
      [ 5  10]            LowImperviousCover
      [ 0   5]            VeryLowImperviousCover)))

(defscenario constrained-development-flood
  "Changes values in developed areas to no succession, low canopy cover, moderate hardwood-softwood ratio,low fire frequency, increased greenhouse gas emissions."
  (model PercentTreeCanopyCoverClass
    (classification PercentTreeCanopyCoverClass
      :context [constrained-development-scenario :as cd percent-canopy-cover :as pcc]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained)))
                      (conc 'carbonService:VeryLowCanopyCover)
                    
                      (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                      (conc 'carbonService:LowCanopyCover)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'carbonService:ModerateCanopyCover)

                      :otherwise (:pcc %))))
  (model SuccessionalStage
    (classification SuccessionalStage
      :context [constrained-development-scenario :as cd successional-stage :as ss]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'floodService:NoSuccession)
                  (:ss %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
      :context [constrained-development-scenario :as cd soil-group-puget :as sg]
      :state   #(if (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'floodService:SoilGroupD)
                  (:sg))))
  (model puget:FloodVegetationType
    (classification puget:FloodVegetationType
      :context [constrained-development-scenario :as cd vegetation-type :as vt]
      :state   #(cond (or (is? (:cd %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:cd %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'puget:DevelopedCultivated)

                      (is? (:cd %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'puget:ForestGrasslandShrubland)

                      :otherwise (:vt %))))
  (model PercentImperviousCoverClass
    (classification (ranking puget:ENVISIONUrbanGrowthImperviousConstrained2060) ;;Check this one with Gary
      [80 100 :inclusive] VeryHighImperviousCover
      [50  80]            HighImperviousCover
      [20  50]            ModeratelyHighImperviousCover
      [10  20]            ModeratelyLowImperviousCover
      [ 5  10]            LowImperviousCover
      [ 0   5]            VeryLowImperviousCover)))