(ns core.models.flood-puget
  (:refer-clojure :rename {count length}) 
  (:refer tl :only [is? conc])
  (:refer modelling :only (defagent defscenario defmodel measurement classification model
                            namespace-ontology categorization ranking numeric-coding
                            probabilistic-measurement probabilistic-classification
                            binary-coding identification bayesian count))
  (:refer aries :only (span)))

(namespace-ontology floodService
  (representation:GenericObservable
    (TempFloodData100)
    (TempFloodData500))
  (thinklab-core:BooleanRanking
        (LandOrSea
            (OnLand) (NotOnLand))))

;; ----------------------------------------------------------------------------------------------
;; common models
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))	
	
(defmodel flow-direction geophysics:FlowDirection
	(ranking geophysics:FlowDirection)) 

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

(defmodel soil-group-puget HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking habitat:HydrologicSoilsGroup)
      1        SoilGroupA
      2        SoilGroupB
      3        SoilGroupC
      4        SoilGroupD))

;; ----------------------------------------------------------------------------------------------
;; Ad-hoc source models
;; ----------------------------------------------------------------------------------------------

;;this layer has problems for now (see .xml) but not currently used.
;;(defmodel precipitation-monthly Precipitation
;;  (measurement habitat:JanuaryPrecipitation "mm"))

(defmodel land-use LandUseLandCover
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding nlcd:NLCDNumeric)
		82	               Agriculture
		#{11 90 95 12}	   WetlandsOpenWater
		21	               DevelopedOpenSpace
		#{41 42 43}        Forest
		#{71 81 52}	       GrassPasture
		#{22	31}          DevelopedLowIntensity
		23	               DevelopedMediumIntensity
		24	               DevelopedHighIntensity))
		
;; Surface temperature - again, should be monthly and matched by temporal extents.  For mean temperature
;;  could use a slightly lower discretization, i.e., < -4, -4 to 4, >4
(defmodel monthly-temperature MonthlyMaximumTemperature
		(classification (measurement geophysics:JanuaryMaximumGroundSurfaceTemperature "\u00b0C")
			 [6 :>] 	HighMonthlyMaximumTemperature
			 [0 6] 	  ModerateMonthlyMaximumTemperature
			 [:< 0] 	LowMonthlyMaximumTemperature))

(defmodel annual-temperature AnnualMaximumTemperature
    (classification (measurement geophysics:AnnualMaximumGroundSurfaceTemperature "\u00b0C")
       [6 :>]   HighAnnualMaximumTemperature
       [0 6]    ModerateAnnualMaximumTemperature
       [:< 0]   LowAnnualMaximumTemperature)) 
			 
;; Snow presence - only the puget-specific statement for now.  This is not currently part of any
;; model but could be incorporated in the future.
(defmodel snow-presence SnowPresence
		(classification (categorization puget:SnowPrecipitationCategory)
			#{"LL" "HL"} LowlandAndHighland
			#{"RD" "SD"} RainDominatedAndSnowDominated
			"RS"         PeakRainOnSnow))

;;These bins are producing uniform snowmelt results everywhere - consider altering if these data are 
;; actually used in another model statement.
(defmodel snowmelt-annual AnnualSnowmelt
    (classification (measurement habitat:AnnualSnowmelt "mm")
        [700 :>]             HighAnnualSnowmelt
        [250 700]            ModerateAnnualSnowmelt
        [:exclusive 0 250]   LowAnnualSnowmelt
        [0]                  NoAnnualSnowmelt))

(defmodel snowmelt-monthly MonthlySnowmelt
    (measurement habitat:JanuarySnowmelt "mm"))

;;Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel runoff-training FloodSourceValue
  (classification (measurement habitat:AnnualRunoff "mm")
                  [:< 200]    VeryLowFloodSource
                  [200 600]   LowFloodSource
                  [600 1200]  ModerateFloodSource
                  [1200 2400] HighFloodSource
                  [2400 :>]   VeryHighFloodSource))

;;Monthly source data is just the sum of precipitation and snowmelt.
;;(defmodel source-monthly FloodSourceMonthly
;;   (measurement FloodSourceMonthly "mm"
;;        :context (precipitation-monthly snowmelt-monthly)
;;        :state   #(+ (:precipitation-monthly %) (:snowmelt-monthly %)))) 

;;Annual source data is simply precipitation-annual (assume all snow melts in each year, which
;; is true everywhere but for glaciers.  Assume that glaciers are neither gaining nor losing
;; mass, which is not true but a simplifying assumption for now.
(defmodel source-annual Precipitation
    (measurement habitat:AnnualPrecipitation "mm"))

;; ----------------------------------------------------------------------------------------------
;; CN source model
;; ----------------------------------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
(defmodel source-cn FloodSource
	  (measurement habitat:AnnualRunoff "mm" 
	 	 	:context  (land-use :as landuse 
                 soil-group-puget :as soilgroup
                 (ranking habitat:PercentImperviousness) :as imperv
                 source-annual :as precipitation)
      :state #(let [
                    ctable 
                       {(tl/conc 'floodService:Agriculture) [64 75 82 85],
                        (tl/conc 'floodService:Forest) [64 75 82 85],
                        (tl/conc 'floodService:GrassPasture) [64 75 82 85],
                        (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85],
                        (tl/conc 'floodService:Agriculture) [64 75 82 85]}
                       ]
                )
))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel slope Slope
		(classification (measurement geophysics:DegreeSlope "\u00b0")
			 [:< 1.15] 	  Level
			 [1.15 4.57] 	GentlyUndulating
			 [4.57 16.70] RollingToHilly
			 [16.70 :>] 	SteeplyDissectedToMountainous))

(defmodel vegetation-type VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding nlcd:NLCDNumeric)
		#{90 95}	         WetlandVegetation
		#{41 42 43 52 71}  ForestGrasslandShrublandVegetation
		#{21 22 23 24 82}	 DevelopedCultivatedVegetation))

(defmodel vegetation-height VegetationHeight
	(classification (measurement habitat:VegetationHeight "ft")
		[120 :>] VeryHighVegetationHeight
		[80 120] HighVegetationHeight
		[50 80]  ModerateVegetationHeight
		[20 50]  LowVegetationHeight
		[:< 20]  VeryLowVegetationHeight))

(defmodel percent-vegetation-cover PercentVegetationCover
	(classification (ranking habitat:PercentVegetationCover)
		[80 100 :inclusive] VeryHighVegetationCover
		[60 80]             HighVegetationCover
		[40 60]             ModerateVegetationCover
		[20 40]             LowVegetationCover
		[0 20]              VeryLowVegetationCover))

(defmodel successional-stage SuccessionalStage
	 (classification (ranking ecology:SuccessionalStage)
	 		#{5 6}                          OldGrowth
	 		4                               LateSuccession
	 		3                               MidSuccession
	 		2                               PoleSuccession
	 		1                               EarlySuccession
	 		#{22 23 24 25 26 27 28 40 41}   NoSuccession))

(defmodel imperviousness PercentImperviousCover
	 (classification (ranking habitat:PercentImperviousness)
	 	   [80 100 :inclusive]   VeryHighImperviousCover
	 	   [50 80]               HighImperviousCover
	 	   [20 50]               ModeratelyHighImperviousCover
	 	   [10 20]               ModeratelyLowImperviousCover
	 	   [5 10]                LowImperviousCover
	 	   [0 5]                 VeryLowImperviousCover))

(defmodel evapotranspiration EvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
                  [90 :>]    VeryHighEvapotranspiration
                  [60 90]    HighEvapotranspiration
                  [30 60]    ModerateEvapotranspiration
                  [12 30]    LowEvapotranspiration
                  [0 12]     VeryLowEvapotranspiration))

;;(defmodel dam-storage DamStorageClass
;;  (classification (measurement DamStorage "mm")
;;                  [30000 :>]      VeryLargeDamStorage
;;                  [9000 30000]    LargeDamStorage
;;                  [3000 9000]     ModerateDamStorage
;;                  [900 3000]      SmallDamStorage
;;                  [0 900]         VerySmallDamStorage
;;                   :otherwise     NoDamStorage))
			
(defmodel mean-days-precipitation-monthly MeanDaysPrecipitationPerMonth
	(classification (ranking habitat:JanuaryDaysOfPrecipitation)
		#{8 9}    VeryHighDaysPrecipitationPerMonth
		#{6 7}    HighDaysPrecipitationPerMonth
		#{4 5}    LowDaysPrecipitationPerMonth
		#{1 2 3}  VeryLowDaysPrecipitationPerMonth))

(defmodel mean-days-precipitation-annual MeanDaysPrecipitationPerYear
  (classification (ranking habitat:AnnualDaysOfPrecipitation)
    #{8 9}    VeryHighDaysPrecipitationPerYear
    #{6 7}    HighDaysPrecipitationPerYear
    #{4 5}    LowDaysPrecipitationPerYear
    #{1 2 3}  VeryLowDaysPrecipitationPerYear))

;;Assumes that detention basins average 3 m, i.e., 3000 mm, in depth, i.e., storage capacity when
;;  empty.  Can alter this as appropriate.  This is likely a more accurate way to measure detention basin storage
;;  but in its current form is incompatible with the BN.  Consider the best way to do this (i.e., combining deterministic
;;  and probabilistic functions when the time comes?)
;;(defmodel detention-basin-storage DetentionBasinStorage
;;  (measurement DetentionBasinStorage "mm" 
;;    :context ((binary-coding infrastructure:DetentionBasin :as detention-basin-storage))
;;    :state #(cond (== (:detention-basin-storage %) 0) 0
;;                  (== (:detention-basin-storage %) 1) 3000)))

;;Use this one for the time being so the sink model will run.  Then review which one to use with Gary.
;;(defmodel detention-basin-storage DetentionBasinStorage
;;  (classification (binary-coding infrastructure:DetentionBasin)
;;    0    DetentionBasinStorageNotPresent
;;    1    DetentionBasinStoragePresent))

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
    (classification  (measurement geophysics:Altitude "m")
       [:exclusive 0 :>] OnLand))

;;Undiscretizer for FloodSink
;;(defmodel flood-sink AnnualFloodSink
;;  (probabilistic-measurement AnnualFloodSink "mm" 
;;                  [30000 90000]     VeryHighFloodSink
;;                  [10000 30000]     HighFloodSink
;;                  [3000 10000]      ModerateFloodSink
;;                  [900 3000]        LowFloodSink
;;                  [0 900]           VeryLowFloodSink))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage GreenInfrastructureStorage
  (probabilistic-measurement GreenInfrastructureStorage "mm" 
                  [115 320]    VeryHighGreenStorage
                  [72 115]     HighGreenStorage
                  [40 72]      ModerateGreenStorage
                  [15 40]      LowGreenStorage
                  [0 15]       VeryLowGreenStorage))

;;Undiscretizer for GrayInfrastructureStorage
;;(defmodel gray-infrastructure-storage GrayInfrastructureStorage
;;  (probabilistic-measurement GrayInfrastructureStorage "mm" 
;;                  [30000 90000]     VeryHighGrayStorage
;;                  [10000 30000]     HighGrayStorage
;;                  [3000 10000]      ModerateGrayStorage
;;                  [900 3000]        LowGrayStorage
;;                  [0 900]           VeryLowGrayStorage))

;;Assumes that detention basins average 3 m, i.e., 3000 mm, in depth, i.e., storage capacity when
;;  empty.  Can alter this as appropriate.
(defmodel detention-basin-storage DetentionBasinStorage
  (measurement DetentionBasinStorage "mm" 
    :context ((binary-coding infrastructure:DetentionBasin) :as detention-basin-storage)
    :state #(cond (== (:detention-basin-storage %) 0) 0
                  (== (:detention-basin-storage %) 1) 3000)))

(defmodel dam-storage DamStorage
  (measurement DamStorage "mm"))

(defmodel gray-infrastructure-sink GrayInfrastructureSink 
  (measurement GrayInfrastructureSink "mm"
    :context (dam-storage detention-basin-storage)
    :state   #(+ (or (:dam-storage %) 0.0) (or (:detention-basin-storage %) 0.0))))

;; Flood sink probability, monthly (need a monthly flood sink undiscretizer here)
;;(defmodel sink-monthly MonthlyFloodSink
;;	  (bayesian MonthlyFloodSink
;;  	:import   "aries.core::FloodSinkMonthly.xdsl"
;;	  	:keep     (
;;	  			MonthlyFloodSink 
;;	  			GreenInfrastructureStorage
;;	  			GrayInfrastructureStorage)
;;      :required (LandOrSea)
;;	 	 	:context  (
;;	 	 			soil-group-puget vegetation-type slope monthly-temperature vegetation-height
;;	 	 			successional-stage imperviousness dam-storage detention-basin-storage
;;	 	 			percent-vegetation-cover mean-days-precipitation-monthly land-selector)))

;; Flood sink probability, annual
;; COMMENT veg height back in once the layers been expanded to a meaningful extent OR Ferd's enabled coexistence of
;;  small layers + priors for areas without evidence.
(defmodel green-infrastructure-sink GreenInfrastructureSink 
    (bayesian GreenInfrastructureSink
      :import   "aries.core::FloodSinkAnnualSimple.xdsl"
      :keep     (GreenInfrastructureStorage)
      :required (LandOrSea) 
      :context (soil-group-puget vegetation-type slope annual-temperature  ;;vegetation-height
                successional-stage imperviousness percent-vegetation-cover 
                mean-days-precipitation-annual land-selector)
      :result green-infrastructure-storage))

(defmodel sink-annual FloodSink
  (measurement FloodSink "mm"
    :context (green-infrastructure-sink gray-infrastructure-sink) 
    :state #(+ 
              (if (nil? (:green-infrastructure-sink %)) 0.0 (.getMean (:green-infrastructure-sink %)))
              (or       (:gray-infrastructure-sink %)   0.0))))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

;;(defmodel floodplains Floodplains
;;	"Presence of a floodplain in given context"
;;	(classification (binary-coding geofeatures:Floodplain)
;;			0 NotInFloodplain
;;			1 InFloodplain))

(defmodel floodplains-100 Floodplains100
  (classification (categorization geofeatures:Floodplain)
                  "A"            In100YrFloodplain
                  :otherwise     NotIn100YrFloodplain))

(defmodel floodplains-500 Floodplains500
  (classification (categorization geofeatures:Floodplain)
                  #{"A" "X500"}      In500YrFloodplain
                  :otherwise         NotIn500YrFloodplain))

;;KB: Don't seem to have any corresponding data here, but the assumption that structures are in the floodplain wherever
;; there is private land is a bad one.  Let's avoid using this for now.
(defmodel structures Structures
	"Assume that any privately owned land in floodplain has vulnerable structures. TODO make more specific when we know more"
	(classification (ranking lulc:PrivatelyOwnedLand)
			0 StructuresNotPresent
			1 StructuresPresent))
			
(defmodel housing aestheticService:PresenceOfHousing
  "Classifies land use from property data."
  (classification (ranking aestheticService:PresenceOfHousing)
        [0 :>]        aestheticService:HousingPresent  
        :otherwise    aestheticService:HousingAbsent)
  ;; fall-back: if no data in the ones above, use NLCD high-intensity development category
	;; TODO check if that's ok
	(classification (numeric-coding nlcd:NLCDNumeric)
		24	       HousingPresent
		:otherwise HousingNotPresent))

(defmodel public-asset PublicAsset
	"Public assets are defined as presence of highways, railways or both. Other classes of public infrastructure could
be added to this list if desired."
	(classification PublicAsset 
		:state   #(if (> (+ (:highway %) (:railway %)) 0) 
								(tl/conc 'floodService:PublicAssetPresent) 
								(tl/conc 'floodService:PublicAssetNotPresent))
		:context ((ranking infrastructure:Highway) :as highway
                  (ranking infrastructure:Railway) :as railway)))

(defmodel farmland Farmland
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding nlcd:NLCDNumeric)
		82	       FarmlandPresent
		:otherwise FarmlandAbsent
;    :agent     "aries/flood/farm"
    :editable  true))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-use-100 FloodFarmersUse100
  (binary-coding FloodFarmersUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains100 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-100)))

(defmodel farmers-use-500 FloodFarmersUse500
  (binary-coding FloodFarmersUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains500 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-500)))

;; Models public infrastructure in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel public-use-100 FloodPublicAssetsUse100
  (binary-coding FloodPublicAssetsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:publicasset %)))
                    1
                    0)
       :context  (public-asset floodplains-100)))

(defmodel public-use-500 FloodPublicAssetsUse500
  (binary-coding FloodPublicAssetsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:publicasset %)))
                        1
                        0))
       :context  (public-asset floodplains-500))

;; Models housing in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel residents-use-100 FloodResidentsUse100
  (binary-coding FloodResidentsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains100 %))
                        (= (tl/conc 'floodService:HousingPresent)      (:housing %)))
                    1
                    0)
       :context (housing floodplains-100)))

(defmodel residents-use-500 FloodResidentsUse500
  (binary-coding FloodResidentsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains500 %))
                        (= (tl/conc 'floodService:HousingPresent)      (:housing %)))
                    1
                    0)
       :context (housing floodplains-500)))

;; Models other private structures in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel private-use-100 FloodPrivateAssetsUse100
  (binary-coding FloodPrivateAssetsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)      (:floodplains100 %))
                        (= (tl/conc 'floodService:StructuresPresent)      (:structures %)))
                    1
                    0)
       :context (structures floodplains-100)))

(defmodel private-use-500 FloodPrivateAssetsUse500
  (binary-coding FloodPrivateAssetsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)      (:floodplains500 %))
                        (= (tl/conc 'floodService:StructuresPresent)      (:structures %)))
                    1
                    0)
       :context (structures floodplains-500)))

;; ---------------------------------------------------------------------------
;; Flow data models & dependencies for the flow models
;; ---------------------------------------------------------------------------

(defmodel flood-flow-data100 TempFloodData100
  (identification TempFloodData100
    :context (altitude streams floodplains-100)))

(defmodel flood-flow-data500 TempFloodData500
  (identification TempFloodData500
    :context (altitude streams floodplains-500)))

;;Levees and floodplain width: used in the flow model
(defmodel levees Levees
  (classification (binary-coding infrastructure:Levee)
      0 LeveesNotPresent
      1 LeveesPresent
;    :agent "aries/flood/levee"
))

(defmodel floodplain-width FloodplainWidth
(classification (measurement habitat:FloodplainWidth "m")
    [400 :>]    HighFloodplainWidth
    [200 400]   ModerateFloodplainWidth
    [:< 200]    LowFloodplainWidth
    :otherwise  NoFloodplainWidth))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage.  These are currently set to "source-annual" but should also be
;; tested for "source-monthly"
(defmodel data-farmers-100 AvoidedDamageToFarms100 
	(identification AvoidedDamageToFarms100 
		:context (
			source-annual :as source
			sink-annual :as sink
			farmers-use-100 :as use
      flood-flow-data100 :as flow)))

(defmodel data-farmers-500 AvoidedDamageToFarms500 
  (identification AvoidedDamageToFarms500 
    :context (
      source-annual :as source
      sink-annual :as sink
      farmers-use-500 :as use
      flood-flow-data500 :as flow)))

(defmodel data-public-100 AvoidedDamageToPublicAssets100 
	(identification AvoidedDamageToPublicAssets100 
		:context (
			source-annual :as source
			sink-annual :as sink
			public-use-100 :as use
      flood-flow-data100 :as flow)))

(defmodel data-public-500 AvoidedDamageToPublicAssets500 
  (identification AvoidedDamageToPublicAssets500 
    :context (
      source-annual :as source
      sink-annual :as sink
      public-use-500 :as use
      flood-flow-data500 :as flow)))

(defmodel data-private-100 AvoidedDamageToPrivateAssets100 
	(identification AvoidedDamageToPrivateAssets100 
		:context (
			source-annual :as source
			sink-annual :as sink
			private-use-100 :as use
      flood-flow-data100 :as flow)))

(defmodel data-private-500 AvoidedDamageToPrivateAssets500 
  (identification AvoidedDamageToPrivateAssets500 
    :context (
      source-annual :as source
      sink-annual :as sink
      private-use-500 :as use
      flood-flow-data500 :as flow)))

(defmodel data-residents-100 AvoidedDamageToResidents100 
	(identification AvoidedDamageToResidents100 
		:context (
			source-annual :as source
			sink-annual :as sink
			residents-use-100 :as use
      flood-flow-data100 :as flow)))

(defmodel data-residents-500 AvoidedDamageToResidents500 
  (identification AvoidedDamageToResidents500 
    :context (
      source-annual :as source
      sink-annual :as sink
      residents-use-500 :as use
      flood-flow-data500 :as flow)))

(defmodel flood-regulation-farmers-100 AvoidedDamageToFarms100
  (span FloodWaterMovement
  	    Precipitation
  	    FloodFarmersUse100
      	FloodSink
      	nil
  	    (geophysics:Altitude geofeatures:River Floodplains100 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
   	    :source-type        :finite
        :sink-type          :finite
   	    :use-type           :infinite
   	    :benefit-type       :non-rival
   	    :downscaling-factor 1
   	    :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual farmers-use-100 sink-annual altitude streams floodplains-100 levees)))

(defmodel flood-regulation-farmers-500 AvoidedDamageToFarms500
  (span FloodWaterMovement
        Precipitation
        FloodFarmersUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual farmers-use-500 sink-annual altitude streams floodplains-500 levees)))

(defmodel flood-regulation-public-assets-100 AvoidedDamageToPublicAssets100
  (span FloodWaterMovement
        Precipitation
        FloodPublicAssetsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual public-use-100 sink-annual altitude streams floodplains-100 levees)))

(defmodel flood-regulation-public-assets-500 AvoidedDamageToPublicAssets500
  (span FloodWaterMovement
        Precipitation
        FloodPublicAssetsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual public-use-500 sink-annual altitude streams floodplains-500 levees)))

(defmodel flood-regulation-residents-100 AvoidedDamageToResidents100
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual residents-use-100 sink-annual altitude streams floodplains-100 levees)))

(defmodel flood-regulation-residents-500 AvoidedDamageToResidents500
  (span FloodWaterMovement
        Precipitation
        FloodResidentsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual residents-use-500 sink-annual altitude streams floodplains-500 levees)))

;; DO NOT use these flow models for now.  We don't have a way of explicitly mapping private assests, aside from housing, which 
;;  is treated elsewhere.  So for now, just run housing, public infrastructure, and farmland as the 3 classes of flow models (each
;;  modeled for the 100- and 500-year floodplain).
(defmodel flood-regulation-private-100 AvoidedDamageToPrivateAssets100
  (span FloodWaterMovement
        Precipitation
        FloodPrivateAssetsUse100
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains100 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual private-use-100 sink-annual altitude streams floodplains-100 levees)))

(defmodel flood-regulation-private-500 AvoidedDamageToPrivateAssets500
  (span FloodWaterMovement
        Precipitation
        FloodPrivateAssetsUse500
        FloodSink
        nil
        (geophysics:Altitude geofeatures:River Floodplains500 Levees)
        :source-threshold   50.0     ;; Consider nearly but not all sources of precipitation, as floods can happen in dry areas too
        :sink-threshold     3000.0   ;; Considering moderate, high, and very high flood sinks
        :use-threshold      0.0      ;;Set at zero since output values for this are a 0/1
        :trans-threshold    5.0      ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10 
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data.clj")
        :keep (Runoff                             PotentialRunoffMitigation 
               PotentiallyVulnerablePopulations   PotentiallyDamagingFloodFlow 
               PotentiallyDamagingRunoff          PotentialFloodDamageReceived
               ActualFloodFlow                    FloodDamagingRunoff 
               UtilizedRunoffMitigation           FloodDamageReceived 
               BenignRunoff                       UnutilizedRunoffMitigation
               AbsorbedFloodFlow                  FloodMitigatedRunoff 
               FloodMitigationBenefitsAccrued) 
        :context (source-annual private-use-500 sink-annual altitude streams floodplains-500 levees)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios 

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defmodel constrained-development-scenario puget:ConstrainedDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCConstrained2060) 
      4                                     puget:HighDensityDevelopedConstrained
      6                                     puget:ModerateDensityDevelopedConstrained
      5                                     puget:LowDensityDevelopedConstrained
      7                                     puget:UrbanOpenSpaceConstrained
      #{0 1 2 3 8 9 10 11 12 13 14 15 16}   puget:NotDevelopedConstrained))

(defmodel open-development-scenario puget:OpenDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCOpen2060) 
      4                                     puget:HighDensityDevelopedOpen
      6                                     puget:ModerateDensityDevelopedOpen
      5                                     puget:LowDensityDevelopedOpen
      7                                     puget:UrbanOpenSpaceOpen
      #{0 1 2 3 8 9 10 11 12 13 14 15 16}   puget:NotDevelopedOpen))

(defscenario open-development-flood
 "Changes values in developed areas to no succession, low canopy cover, moderate hardwood-softwood ratio,low fire frequency, increased greenhouse gas emissions."
  (model PercentVegetationCover
    (classification PercentVegetationCover
        :context (open-development-scenario percent-vegetation-cover)
        :state #(cond (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen)))
                      (conc 'carbonService:VeryLowVegetationCover)
                      
                      (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                      (conc 'carbonService:LowVegetationCover)

                      (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'carbonService:ModerateVegetationCover)
                      
                      :otherwise (:percent-vegetation-cover %))))
  (model SuccessionalStage
    (classification SuccessionalStage
          :context (open-development-scenario successional-stage)
          :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'floodService:NoSuccession)
                  (:successional-stage %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
         :context (open-development-scenario soil-group-puget)
         :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                         (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                         (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                         (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))                                    
                  (conc 'floodService:SoilGroupD)
                  (:soil-group-puget))))
  (model VegetationType
    (classification VegetationType
        :context (open-development-scenario vegetation-type)
        :state #(cond (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen)))
                  (conc 'floodService:DevelopedCultivatedVegetation)

                  (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen))
                  (conc 'floodService:ForestGrasslandShrublandVegetation)

                  :otherwise (:vegetation-type %))))
  (model PercentImperviousCover
         (classification (ranking puget:ENVISIONUrbanGrowthImperviousOpen2060) ;;Check this one with Gary - THIS SHOULD BE GOOD.
           [80 100 :inclusive]   VeryHighImperviousCover
	 	   [50 80]               HighImperviousCover
	 	   [20 50]               ModeratelyHighImperviousCover
	 	   [10 20]               ModeratelyLowImperviousCover
	 	   [5 10]                LowImperviousCover
	 	   [0 5]                 VeryLowImperviousCover)))

(defscenario constrained-development-flood
 "Changes values in developed areas to no succession, low canopy cover, moderate hardwood-softwood ratio,low fire frequency, increased greenhouse gas emissions."
  (model PercentVegetationCover
    (classification PercentVegetationCover
        :context (constrained-development-scenario percent-vegetation-cover)
        :state #(cond (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrained)))
                      (conc 'carbonService:VeryLowVegetationCover)
                      
                      (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                      (conc 'carbonService:LowVegetationCover)

                      (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'carbonService:ModerateVegetationCover)

                :otherwise (:percent-vegetation-cover %))))
  (model SuccessionalStage
    (classification SuccessionalStage
       :context (constrained-development-scenario successional-stage)
       :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                       (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                       (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                       (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'floodService:NoSuccession)
                  (:successional-stage %))))
  (model HydrologicSoilsGroup
    (classification HydrologicSoilsGroup
       :context (constrained-development-scenario soil-group-puget)
       :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                       (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                       (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                       (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'floodService:SoilGroupD)
                  (:soil-group-puget))))
  (model VegetationType
    (classification VegetationType
        :context (constrained-development-scenario vegetation-type)
        :state #(cond (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained)))
                  (conc 'floodService:DevelopedCultivatedVegetation)

                  (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained))
                  (conc 'floodService:ForestGrasslandShrublandVegetation)

                  :otherwise (:vegetation-type %))))
  (model PercentImperviousCover
         (classification (ranking puget:ENVISIONUrbanGrowthImperviousConstrained2060) ;;Check this one with Gary
           [80 100 :inclusive]   VeryHighImperviousCover
	 	   [50 80]               HighImperviousCover
	 	   [20 50]               ModeratelyHighImperviousCover
	 	   [10 20]               ModeratelyLowImperviousCover
	 	   [5 10]                LowImperviousCover
	 	   [0 5]                 VeryLowImperviousCover)))