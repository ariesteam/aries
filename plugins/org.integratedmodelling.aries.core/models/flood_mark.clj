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

(defmodel soil-group 'floodService:HydrologicSoilsGroup
  "Relevant soil group"
  (classification (ranking 'habitat:HydrologicSoilsGroup)
                  1        'floodService:SoilGroupA
                  2        'floodService:SoilGroupB
                  3        'floodService:SoilGroupC
                  4        'floodService:SoilGroupD))

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

(defmodel rainfall-erosivity 'floodService:RainfallErosivityClass
  (classification (ranking 'soilretentionEcology:RainfallRunoffErosivityIndex)
                  [90 :>]  'floodService:VeryHighRainfallErosivity
                  [70 89]  'floodService:HighRainfallErosivity
                  [50 69]  'floodService:ModerateRainfallErosivity
                  [30 49]  'floodService:LowRainfallErosivity
                  [:< 29]  'floodService:VeryLowRainfallErosivity)) 

;;Use runoff as training data here
(defmodel flood-source-value 'floodService:FloodSourceValue
  (classification (measurement 'habitat:AnnualRunoff "mm")
                  [:< 200]    'floodService:VeryLowFloodSource
                  [200 600]   'floodService:LowFloodSource
                  [600 1200]  'floodService:ModerateFloodSource
                  [1200 2400] 'floodService:HighFloodSource
                  [2400 :>]   'floodService:VeryHighFloodSource))

;; Flood source probability, ad hoc method
(defmodel source 'floodService:FloodSource
  (bayesian 'floodService:FloodSource 
            :import   "aries.core::FloodSourceValueAdHocMark.xdsl"
            :context  (precipitation imperviousness rainfall-erosivity)
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

(defmodel evapotranspiration 'floodService:EvapotranspirationClass
  (classification (measurement 'habitat:ActualEvapotranspiration "mm")
                  [90 :>]    'floodService:VeryHighEvapotranspiration
                  [60 90]    'floodService:HighEvapotranspiration
                  [30 60]    'floodService:ModerateEvapotranspiration
                  [12 30]    'floodService:LowEvapotranspiration
                  [0 12]     'floodService:VeryLowEvapotranspiration)) 

;;GARY: Do we need to convert to mm, or should it do it automatically?  (in=25.4 mm)
(defmodel infiltration 'floodService:SoilInfiltrationClass
  (classification (measurement 'habitat:Infiltration "in")
                  [0.976 :>]       'floodService:VeryHighSoilInfiltration
                  [0.519 0.976]    'floodService:HighSoilInfiltration
                  [0.327 0.519]    'floodService:ModerateSoilInfiltration
                  [0.133 0.327]    'floodService:LowSoilInfiltration
                  [0 0.133]        'floodService:VeryLowSoilInfiltration)) 

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
            :keep     ('floodService:FloodSink 
                       'floodService:GreenInfrastructureStorage
                       'floodService:GrayInfrastructureStorage)
            :context  (soil-group slope imperviousness percent-vegetation-cover dam-storage
                        infiltration evapotranspiration)))

;; ----------------------------------------------------------------------------------------------
;; use models
;; ----------------------------------------------------------------------------------------------

;;Use undiscretizers are simple: 0/1
;; F the undiscretizers: use the same language as sediment_dr.clj

(defmodel floodplains 'floodService:Floodplains
  "Presence of a floodplain in given context"
  (classification (binary-coding 'geofeatures:Floodplain)
                  0 'floodService:NotInFloodplain
                  1 'floodService:InFloodplain))

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
(defmodel farmers-use 'floodService:FloodFarmersUse 
  (binary-coding 'floodService:FloodFarmersUse
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 1.0))
                    1
                    0)
       :context ((binary-coding 'floodService:FarmlandPresent :as farmlandpresent)
                 (binary-coding 'geofeatures:Floodplain :as floodplains)))) 

;; Models public infrastructure in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel public-use 'floodService:FloodPublicAssetsUse
  (binary-coding 'floodService:FloodPublicAssetsUse
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:publicasset %) 1.0))
                    1
                    0)
       :context ((binary-coding 'floodService:PublicAsset :as publicasset)
                 (binary-coding 'geofeatures:Floodplain :as floodplains))))

;; ---------------------------------------------------------------------------------------------------          
;; overall models 
;; ---------------------------------------------------------------------------------------------------          

;; all data, for testing and storage
(defmodel data-farmers 'floodService:AvoidedDamageToFarms 
  (identification 'floodService:AvoidedDamageToFarms 
                  :context (source :as source
                            sink :as sink
                            farmers-use :as use)))

(defmodel data-public 'floodService:AvoidedDamageToPublicAssets 
  (identification 'floodService:AvoidedDamageToPublicAssets 
                  :context (source :as source
                            sink :as sink
                            public-use :as use)))

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

;; flow model   for farmers     
(defmodel flood-regulation-farmers 'floodService:AvoidedDamageToFarms
  (span 'floodService:FarmlandFlooding 
        'floodService:FloodSourceValue
        'floodService:FarmersInFloodHazardZones
        'floodService:FloodSink
        'floodService:WaterMovement
        'geophysics:Altitude
        :source-threshold   10,
        :sink-threshold     0.3,
        :use-threshold      0.3,
        :trans-threshold    1.0,
        :sink-type          :finite,
        :use-type           :infinite,
        :benefit-type       :non-rival,
        :downscaling-factor 3,
        :rv-max-states      10 
        :context (source farmers-use sink altitude)))

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
