(ns core.models.flood-mark
  (:refer-clojure :rename {count length})
  (:refer modelling :only (defagent defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; ----------------------------------------------------------------------------------------------
;; common models
;; ----------------------------------------------------------------------------------------------

(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))   

(defmodel flow-direction 'geophysics:FlowDirection
  (ranking 'geophysics:FlowDirection)) 

(defmodel streams 'geofeatures:River
  (binary-coding 'geofeatures:River))

(defmodel precipitation 'floodService:Precipitation
  (classification (measurement 'habitat:AnnualPrecipitation "mm")
                  [:< 75]     'floodService:VeryLowPrecipitation
                  [75 150]    'floodService:LowPrecipitation
                  [150 300]   'floodService:ModeratePrecipitation
                  [300 600]   'floodService:HighPrecipitation
                  [600 :>]    'floodService:VeryHighPrecipitation))

(defmodel imperviousness 'floodService:PercentImperviousCover
  (classification (ranking 'habitat:PercentImperviousness)
                  [80 100 :inclusive]    'floodService:VeryHighImperviousCover
                  [50 80]                'floodService:HighImperviousCover
                  [20 50]                'floodService:ModeratelyHighImperviousCover
                  [10 20]                'floodService:ModeratelyLowImperviousCover
                  [5 10]                 'floodService:LowImperviousCover
                  [0 5]                  'floodService:VeryLowImperviousCover))

;; ----------------------------------------------------------------------------------------------
;; ad-hoc source models
;; ----------------------------------------------------------------------------------------------

;;Error in Ferd's code in handling this layer: removing from the context list and setting the prior
;; at a uniform level reflecting the data for the site of interest.
(defmodel rainfall-erosivity 'floodService:RainfallErosivityClass
  (classification (ranking 'soilretentionEcology:RainfallRunoffErosivityIndex)
                  [90 :>]  'floodService:VeryHighRainfallErosivity
                  [70 89]  'floodService:HighRainfallErosivity
                  [50 69]  'floodService:ModerateRainfallErosivity
                  [30 49]  'floodService:LowRainfallErosivity
                  [:< 29]  'floodService:VeryLowRainfallErosivity)) 

;;Use runoff as training data - or possibly for the sink model (talk to a hydrologist)
(defmodel flood-source-training 'floodService:FloodSourceValue
  (classification (measurement 'habitat:AnnualRunoff "mm")
                  [:< 200]    'floodService:VeryLowFloodSource
                  [200 600]   'floodService:LowFloodSource
                  [600 1200]  'floodService:ModerateFloodSource
                  [1200 2400] 'floodService:HighFloodSource
                  [2400 :>]   'floodService:VeryHighFloodSource))

(defmodel flood-source-value 'floodService:FloodSourceValue
  (classification 'floodService:FloodSourceValue
                  [0 200]        'floodService:VeryLowFloodSource
                  [200 600]      'floodService:LowFloodSource
                  [600 1200]     'floodService:ModerateFloodSource
                  [1200 2400]    'floodService:HighFloodSource
                  [2400 11000]   'floodService:VeryHighFloodSource))

;; Flood source probability, ad hoc method
(defmodel source 'floodService:FloodSource
  (bayesian 'floodService:FloodSource 
            :import   "aries.core::FloodSourceValueAdHocMark.xdsl"
            :context  (precipitation imperviousness)
            :keep     ('floodService:FloodSourceValue)
            :observed (flood-source-value)))

;; ----------------------------------------------------------------------------------------------
;; CN source model
;; ----------------------------------------------------------------------------------------------

;; Flood source probability (runoff levels), SCS curve number method
;; See: https://engineering.purdue.edu/mapserve/LTHIA7/documentation/scs.htm
;;(defmodel source-cn 'floodService:FloodSource
;;    (measurement 'habitat:AnnualRunoff "mm" 
;;          :context  (land-use :as landuse 
;;                 soil-group-puget :as soilgroup
;;                 (ranking 'habitat:PercentImperviousness) :as imperv
;;                 precipitation :as precipitation)
;;      :state #(let [ctable {(tl/conc 'floodService:Agriculture)        [64 75 82 85]
;;                            (tl/conc 'floodService:Forest)             [64 75 82 85]
;;                            (tl/conc 'floodService:GrassPasture)       [64 75 82 85]
;;                            (tl/conc 'floodService:DevelopedOpenSpace) [64 75 82 85]}]
;;                )
;;))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group 'floodService:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking 'habitat:HydrologicSoilsGroup)
                  1        'floodService:SoilGroupA
                  2        'floodService:SoilGroupB
                  3        'floodService:SoilGroupC
                  4        'floodService:SoilGroupD))

(defmodel slope 'floodService:Slope
  (classification (measurement 'geophysics:DegreeSlope "\u00b0")
                  [:< 1.15]    'floodService:Level
                  [1.15 4.57]  'floodService:GentlyUndulating
                  [4.57 16.70] 'floodService:RollingToHilly
                  [16.70 :>]   'floodService:SteeplyDissectedToMountainous))

;;REDO THIS AS VEGETATION TYPE SOCAL
(defmodel vegetation-type 'floodService:VegetationType
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{90 95}           'floodService:WetlandVegetation
                  #{41 42 43 52 71}  'floodService:ForestGrasslandShrublandVegetation
                  #{21 22 23 24 82}  'floodService:DevelopedCultivatedVegetation))

(defmodel percent-vegetation-cover 'floodService:PercentVegetationCover
  (classification (ranking 'habitat:PercentVegetationCover)
                  [80 100] 'floodService:VeryHighVegetationCover
                  [60 80]  'floodService:HighVegetationCover
                  [40 60]  'floodService:ModerateVegetationCover
                  [20 40]  'floodService:LowVegetationCover
                  [0 20]   'floodService:VeryLowVegetationCover))

;;Problems with coarse-grain pixels; removed this from the bayesian statement and set the prior
;; to its actual value from the data (LowActualEvapotranspiration) - a good temporary solution for
;; WCH but change if you ran it again for Southern California.
(defmodel evapotranspiration 'floodService:EvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [90 :>]    'floodService:VeryHighEvapotranspiration
                  [60 90]    'floodService:HighEvapotranspiration
                  [30 60]    'floodService:ModerateEvapotranspiration
                  [12 30]    'floodService:LowEvapotranspiration
                  [0 12]     'floodService:VeryLowEvapotranspiration)) 

(defmodel infiltration 'floodService:SoilInfiltrationClass
  (classification (measurement 'habitat:AnnualInfiltration "mm")
                  [25 :>]   'floodService:VeryHighSoilInfiltration
                  [13 25]   'floodService:HighSoilInfiltration
                  [8 13]    'floodService:ModerateSoilInfiltration
                  [3 8]     'floodService:LowSoilInfiltration
                  [0 3]     'floodService:VeryLowSoilInfiltration))

;;This only presence/absence data as data are lacking for southern California.
(defmodel dam-storage 'floodService:DamStorageClass
  (classification (binary-coding 'floodService:DamStorage)
                  0  'floodService:NoDamStorage
                  1  'floodService:SmallDamStorage))

;;Undiscretizer for FloodSink
(defmodel flood-sink 'floodService:FloodSink
  (classification 'floodService:FloodSink
                  :units      "mm" 
                  [30000 90000]     'floodService:VeryHighFloodSink
                  [10000 30000]     'floodService:HighFloodSink
                  [3000 10000]      'floodService:ModerateFloodSink
                  [900 3000]        'floodService:LowFloodSink
                  [0 900]           'floodService:VeryLowFloodSink))

;;Undiscretizer for GreenInfrastructureStorage
(defmodel green-infrastructure-storage 'floodService:GreenInfrastructureStorage
  (classification 'floodService:GreenInfrastructureStorage
                  :units      "mm" 
                  [115 320]    'floodService:VeryHighGreenStorage
                  [72 115]     'floodService:HighGreenStorage
                  [40 72]      'floodService:ModerateGreenStorage
                  [15 40]      'floodService:LowGreenStorage
                  [0 15]       'floodService:VeryLowGreenStorage))

;;Undiscretizer for GrayInfrastructureStorage
(defmodel gray-infrastructure-storage 'floodService:GrayInfrastructureStorage
  (classification 'floodService:GrayInfrastructureStorage
                  :units      "mm" 
                  [30000 90000]     'floodService:VeryHighGrayStorage
                  [10000 30000]     'floodService:HighGrayStorage
                  [3000 10000]      'floodService:ModerateGrayStorage
                  [900 3000]        'floodService:LowGrayStorage
                  [0 900]           'floodService:VeryLowGrayStorage))

;; Flood sink probability
;; TODO missing data
(defmodel sink 'floodService:FloodSink
  "Interface to Flood resident use bayesian network"
  (bayesian 'floodService:FloodSink 
            :import   "aries.core::FloodSinkMark.xdsl"
            :context  (soil-group slope imperviousness percent-vegetation-cover dam-storage)
            :observed (flood-sink) 
            :keep     ('floodService:FloodSink)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

;;Uses for all beneficiary groups are defined for both the 100- and 500-year floodplain.  These are then incorporated into 
;; the identification and SPAN statements so beneficiary and flow maps can be run for both spatial extents of floodplains.
(defmodel floodplains-100 'floodService:Floodplains100
  (classification (categorization 'geofeatures:Floodplain)
                  #{"ANI" "X" "X500"} 'floodService:NotIn100YrFloodplain
                  #{"A" "AO"}         'floodService:In100YrFloodplain))

(defmodel floodplains-500 'floodService:Floodplains500
  (classification (categorization 'geofeatures:Floodplain)
                  #{"ANI" "X"}       'floodService:NotIn500YrFloodplain
                  #{"A" "AO" "X500"} 'floodService:In500YrFloodplain))

(defmodel public-asset 'floodService:PublicAsset
  "Public assets are defined as presence of highways, railways or both."
  (classification 'floodService:PublicAsset 
                  :state   #(if (> (+ (:highway %) (:railway %)) 0) 
                              (tl/conc 'floodService:PublicAssetPresent) 
                              (tl/conc 'floodService:PublicAssetNotPresent))
                  :context ((ranking 'infrastructure:Highway) :as highway
                            (ranking 'infrastructure:Railway) :as railway)))

(defmodel farmland 'floodService:Farmland
  "Just a reclass of the NLCD land use layer"
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  82         'floodService:FarmlandPresent
                  :otherwise 'floodService:FarmlandAbsent
                                        ;    :agent     "aries/flood/farm"
                  :editable  true))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-use-100 'floodService:FloodFarmersUse100
  (binary-coding 'floodService:FloodFarmersUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)   (:floodplains100 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-100)))

(defmodel farmers-use-500 'floodService:FloodFarmersUse500
  (binary-coding 'floodService:FloodFarmersUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)   (:floodplains500 %))
                        (= (tl/conc 'floodService:FarmlandPresent)     (:farmland %)))
                    1
                    0)
       :context (farmland floodplains-500)))

;; Models public infrastructure in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel public-use-100 'floodService:FloodPublicAssetsUse100
  (binary-coding 'floodService:FloodPublicAssetsUse100
       :state #(if (and (= (tl/conc 'floodService:In100YrFloodplain)  (:floodplains100 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:publicasset %)))
                    1
                    0)
       :context  (public-asset floodplains-100)))

(defmodel public-use-500 'floodService:FloodPublicAssetsUse500
  (binary-coding 'floodService:FloodPublicAssetsUse500
       :state #(if (and (= (tl/conc 'floodService:In500YrFloodplain)  (:floodplains500 %))
                        (= (tl/conc 'floodService:PublicAssetPresent) (:publicasset %)))
                   1
                   0)
       :context  (public-asset floodplains-500)))

;; ---------------------------------------------------------------------------
;; flow data models
;; ---------------------------------------------------------------------------

(defmodel flood-flow-data100 'floodService:TempFloodData100$
  (identification 'floodService:TempFloodData100
    :context (altitude streams floodplains-100)))

(defmodel flood-flow-data500 'floodService:TempFloodData500$
  (identification 'floodService:TempFloodData500
    :context (altitude streams floodplains-500)))

;; ---------------------------------------------------------------------------------------------------          
;; overall models 
;; ---------------------------------------------------------------------------------------------------          

;; all data, for testing and storage
(defmodel data-farmers-100 'floodService:AvoidedDamageToFarms100
  (identification 'floodService:AvoidedDamageToFarms100
                  :context (source :as source
                            sink :as sink
                            farmers-use-100 :as use
                            flood-flow-data100 :as flow)))

(defmodel data-farmers-500 'floodService:AvoidedDamageToFarms500
  (identification 'floodService:AvoidedDamageToFarms500
                  :context (source :as source
                            sink :as sink
                            farmers-use-500 :as use
                            flood-flow-data500 :as flow)))

(defmodel data-public-100 'floodService:AvoidedDamageToPublicAssets100
  (identification 'floodService:AvoidedDamageToPublicAssets100
                  :context (source :as source
                            sink :as sink
                            public-use-100 :as use
                            flood-flow-data100 :as flow)))

(defmodel data-public-500 'floodService:AvoidedDamageToPublicAssets500
  (identification 'floodService:AvoidedDamageToPublicAssets500
                  :context (source :as source
                            sink :as sink
                            public-use-500 :as use
                            flood-flow-data500 :as flow)))

;;(defmodel data-private 'floodService:AvoidedDamageToPrivateAssets 
  ;;(identification 'floodService:AvoidedDamageToPrivateAssets 
    ;;              :context (source :as source
    ;;                        sink :as sink
    ;;                        private-use :as use)))

;;(defmodel data-residents 'floodService:AvoidedDamageToResidents 
  ;;(identification 'floodService:AvoidedDamageToResidents 
    ;;              :context (source :as source
      ;;                      sink :as sink
      ;;                      residents-use :as use)))

;; flow model for farmers in the 100-year floodplain   
(defmodel flood-regulation-farmers-100 'floodService:AvoidedDamageToFarms100
  (span 'floodService:FloodWaterMovement
        'floodService:FloodSourceValue
        'floodService:FloodFarmersUse100
        'floodService:FloodSink
        nil 
        'floodService:TempFloodData100
;;        :source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
;;        :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        :source-threshold   600.0    ;; Excludes all but >50% ModerateFloodSource
        :sink-threshold     30000.0  ;; Excludes all but >50% VeryHighFloodSink
        :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1  ;; MUST NOT trigger resampling! Fucking hydrosheds extent is prime!
        :rv-max-states      10
<<<<<<< Updated upstream
        :keep ('floodService:Runoff 'floodService:PotentialRunoffMitigation 'floodService:PotentiallyVulnerablePopulations
              'floodService:PotentiallyDamagingFloodFlow 'floodService:PotentiallyDamagingRunoff 'floodService:PotentialFloodDamageReceived
              'floodService:ActualFloodFlow 'floodService:FloodDamagingRunoff 'floodService:UtilizedRunoffMitigation
              'floodService:FloodDamageReceived 'floodService:BenignRunoff 'floodService:UnutilizedRunoffMitigation
              'floodService:AbsorbedFloodFlow 'floodService:FloodMitigatedRunoff 'floodService:FloodMitigationBenefitsAccrued) 
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_farmers100.clj")
=======
        :save-file          (str (System/getProperty "user.home") "/flood_data_farmers100.clj")
>>>>>>> Stashed changes
        :context (source farmers-use-100 sink flood-flow-data100)))

;; flow model for farmers in the 500-year floodplain  
(defmodel flood-regulation-farmers-500 'floodService:AvoidedDamageToFarms500
  (span 'floodService:FloodWaterMovement 
        'floodService:FloodSourceValue
        'floodService:FloodFarmersUse500
        'floodService:FloodSink
        nil
        'floodService:TempFloodData500
        :source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 8
        :rv-max-states      10
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_farmers500.clj")
        :context (source farmers-use-500 sink flood-flow-data500)))

;; flow model for public-assets in the 100-year floodplain
(defmodel flood-regulation-public-assets-100 'floodService:AvoidedDamageToFarms100
  (span 'floodService:FloodWaterMovement
        'floodService:FloodSourceValue
        'floodService:FloodPublicAssetsUse100
        'floodService:FloodSink
        nil
        'floodService:TempFloodData100
        :source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 8
        :rv-max-states      10
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_public100.clj")
        :context (source public-use-100 sink flood-flow-data100)))

;; flow model for public-assets in the 500-year floodplain
(defmodel flood-regulation-public-assets-500 'floodService:AvoidedDamageToFarms500
  (span 'floodService:FloodWaterMovement
        'floodService:FloodSourceValue
        'floodService:FloodPublicAssetsUse500
        'floodService:FloodSink
        nil
        'floodService:TempFloodData500
        :source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 8
        :rv-max-states      10
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_public500.clj")
        :context (source public-use-500 sink flood-flow-data500)))

;;Levees and floodplain width: used in the flow model
;;No data for levees in Orange County at this point but leaving the defmodel statement in for now.     
(defmodel levees 'floodService:Levees
  "Presence of a levee in given context"
  (classification (binary-coding 'infrastructure:Levee)
                  0 'floodService:LeveesNotPresent
                  1 'floodService:LeveesPresent
                                        ;    :agent "aries/flood/levee"
                  ))
(defmodel floodplain-width 'floodService:FloodplainWidth
  (classification (measurement 'habitat:FloodplainWidth "m")
                  [400 :>]     'floodService:HighFloodplainWidth
                  [200 400]    'floodService:ModerateFloodplainWidth
                  [:< 200]     'floodService:LowFloodplainWidth
                  :otherwise   'floodService:NoFloodplainWidth))
