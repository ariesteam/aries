(ns core.models.water-la-antigua
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization 
                            namespace-ontology ranking numeric-coding binary-coding identification 
                            probabilistic-measurement probabilistic-classification probabilistic-ranking bayesian count))
  (:refer aries :only (span)))

(namespace-ontology waterSupplyService
  (representation:GenericObservable
    (TempSurfaceWaterData)))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;Runoff would be preferable to precipitation, but its at a very coarse spatial resolution
;; (i.e., <3 full pixels for the La Antigua watershed.  Use precip for now, with the goal of 
;; incorporating a better runoff model (plus sink models that actually capture infiltration & ET).
(defmodel precipitation-annual AnnualPrecipitation
  (measurement habitat:AnnualPrecipitation "mm"))

;;Incorporate runoff data in the future once we've done a better job with the hydro modeling.
;;(defmodel runoff soilretentionEcology:AnnualRunoff
	;;(classification (measurement soilretentionEcology:Runoff "mm/year")
		;;[0 200] 	    soilretentionEcology:VeryLowAnnualRunoff
		;;[200 600] 	  soilretentionEcology:LowAnnualRunoff
		;;[600 1200]  	soilretentionEcology:ModerateAnnualRunoff
		;;[1200 2400] 	soilretentionEcology:HighAnnualRunoff
		;;[2400 :>] 	  soilretentionEcology:VeryHighAnnualRunoff))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

;;Ad hoc sink model adapted from the ad hoc flood sink model.  Includes infiltration & evapotranspiration
;;processes.  Deterministic models could likely be used.

(defmodel slope SlopeClass
    (classification (measurement geophysics:DegreeSlope "\u00B0")
       [0 1.15]               Level
       [1.15 4.57]            GentlyUndulating
       [4.57 16.70]           RollingToHilly
       [16.70 90 :inclusive]  SteeplyDissectedToMountainous))

(defmodel soil-group HydrologicSoilsGroup
  (classification (ranking habitat:HydrologicSoilsGroup)
      1       SoilGroupA
      2       SoilGroupB
      3       SoilGroupC
      4       SoilGroupD))

(defmodel imperviousness PercentImperviousCoverClass
   (classification (ranking habitat:PercentImperviousness)
       [80 100 :inclusive]   VeryHighImperviousCover
       [50 80]               HighImperviousCover
       [20 50]               ModeratelyHighImperviousCover
       [10 20]               ModeratelyLowImperviousCover
       [5 10]                LowImperviousCover
       [1 5]                 VeryLowImperviousCover))

(defmodel vegetation-type VegetationType
  "Just a reclass of the Veracruz land use layer"
  (classification (categorization veracruz-lulc:VeracruzLULCCategory)
    "Bosque mesofilo de montana"                                                 CloudForest
    #{"Pastizal cultivado" "Pastizal inducido" "Zona Urbana" "riego" "temporal"} DevelopedCultivated
    #{"Selva alta subperennifolia" "Bosque cultivado" "Bosque de encino" "Bosque de encino-pino" "Bosque de oyamel" "Bosque de pino" "Bosque de pino-encino" "Bosque de tascate"} DryForest
    #{"Matorral desertico rosetofilo" "Pradera de alta montana" "Vegetacion de dunas costeras" "Vegetacion halofila" "Popal"} GrasslandShrubland
    #{"Selva baja caducifolia" "Selva mediana subcaducifolia"}                   Rainforest))
    
(defmodel percent-vegetation-cover PercentVegetationCoverClass
  (classification (ranking habitat:PercentVegetationCover)
    [80 100] VeryHighVegetationCover
    [60 80]  HighVegetationCover
    [40 60]  ModerateVegetationCover
    [20 40]  LowVegetationCover
    [1 20]   VeryLowVegetationCover))

(defmodel evapotranspiration floodService:EvapotranspirationClass
  (classification (measurement habitat:ActualEvapotranspiration "mm")
                  [90 :>]    floodService:VeryHighEvapotranspiration
                  [60 90]    floodService:HighEvapotranspiration
                  [30 60]    floodService:ModerateEvapotranspiration
                  [12 30]    floodService:LowEvapotranspiration
                  [0 12]     floodService:VeryLowEvapotranspiration))

(defmodel dam-presence Dams
  (classification (binary-coding NonRivalWaterUseCode)
      1         DamPresent
     :otherwise DamAbsent))

;;Undiscretization values based on evapotranspiration layer (which could be included in this BN)
;; but with breakpoint values doubled to account for the effects of soil infiltration, dams, etc.
(defmodel sink-undiscretizer SurfaceWaterSinkClass
  (probabilistic-measurement SurfaceWaterSinkClass "mm" 
;;    [180 :>]           VeryHighSurfaceWaterSink
    [180 260]            VeryHighSurfaceWaterSink
    [100 180]            HighSurfaceWaterSink
    [50 100]             ModerateSurfaceWaterSink
    [1 50]               LowSurfaceWaterSink
    [0 1]                NoSurfaceWaterSink))
;;    [:exclusive 0 50]  LowSurfaceWaterSink
;;    0                  NoSurfaceWaterSink))

(defmodel sink SurfaceWaterSinkClass
    (bayesian SurfaceWaterSinkClass
      :import   "aries.core::SurfaceWaterSupplySink.xdsl"
      ;;:context  (soil-group vegetation-type slope imperviousness dam-presence percent-vegetation-cover)
      :context  (soil-group vegetation-type slope imperviousness percent-vegetation-cover) 
      :keep     (SurfaceWaterSinkClass)
      :observed (sink-undiscretizer)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

;;Industrial surface water use
;;This is all we have to model industrial use right now: presence/absence of an industrial
;;user and whether they use ground or surface water. 
;;Current extraction is unknown.  Values below are 100% guesses - need information from local plants.
;;2 & 3 are zeros because they represent groundwater use, which we're not yet modeling.
(defmodel industrial-users IndustrialWaterUse
  (measurement IndustrialWaterUse "mm" ;;This is an annual value.
    :context ((numeric-coding IndustrialWaterUseCode :as industrial-water-use))
    :state #(cond (== (:industrial-water-use %) 0) 50   ;;Paper factory, using surface water
                  (== (:industrial-water-use %) 1) 100  ;;Bottled water plant, using surface water
                  (== (:industrial-water-use %) 2)  0   ;;Nestle plant, using groundwater
                  (== (:industrial-water-use %) 3)  0   ;;Coca-Cola plant, using groundwater
                  :otherwise                        0)))

;;Nonrival surface water use: Hydropower plus rafting
;;This is all we have to model rafting and hydropower use right now: presence/absence of a user
;;user. It's a little strange to lump hydro and rafting together, but we'll
;;do it for now.
(defmodel non-rival-water-users NonRivalWaterUse
  (binary-coding NonRivalWaterUse
    :context ((binary-coding NonRivalWaterUseCode :as non-rival-water-users))
    :state #(cond (== (:non-rival-water-users %) 0) 1  ;;Rafting use
                  (== (:non-rival-water-users %) 1) 1  ;;Hydropower use
                  :otherwise                        0)))

;;Residential surface water use
;;The ranking model should really be a count with spatial ctx (data are persons/30 arc-second pixel)
;;The first example is for a probability distribution function (discrete values of the probability states)
;;The second example, used, is for a cumulative distribution function (ranges)
;;Neither of these are enabled yet, so we're just using a deterministic function for now.
;;Residential surface water use: currently only looking at surface water use (80% of the total, per Rowan's BN. 
;;Top node discretization for water use is from Alberta: worth asking about better local sources.
;;www1.agric.gov.ab.ca/$department/deptdocs.nsf/all/agdex1349
(defmodel residential-surface-water-use ResidentialSurfaceWaterUse
  (measurement ResidentialSurfaceWaterUse "mm" ;;This is an annual value
    :context ((count policytarget:PopulationDensity "/km^2" :as population-density))
    :state   #(* 0.8 0.082855 (:population-density %))))
;;  :state   #(rv-scalar-multiply {10 25/100, 20 50/100, 30 25/100} (* 0.8 (:population-density %))) 
;;  :state   #(rv-scalar-multiply {70.81 0, 78.84 25/100, 86.87 75/100, 94.9 1} (* 0.8 (:population-density %))))) 

;;Agricultural surface water use. Step 1: Estimate total livestock water needs.
;;Livestock use below is from different sources for pigs and other livestock: its unlikely
;; that pigs should use more water per capita than cattle (Rowan to check on this)
(defmodel livestock-total-water-use LivestockWaterUse
  (measurement LivestockWaterUse "mm"  ;;This is an annual value
    :context ((count CattlePopulation "/km^2" :as cattle-population)
              (count SheepPopulation  "/km^2" :as sheep-population)
              (count PigsPopulation   "/km^2" :as pigs-population)
              (count GoatsPopulation  "/km^2" :as goats-population))
    :state    #(+ (* (:sheep-population  %) 0.002745) ;;this is in m^3/1000 animals, the conversion ultimately giving mm
                  (* (:goats-population  %) 0.002745)
                  (* (:cattle-population %) 0.011032)
                  (* (:pigs-population   %) 0.013310))))

(defmodel livestock-total-water-use-discretized LivestockTotalWaterUseClass
  (classification livestock-total-water-use
    [1.15 :>]  HighLivestockTotalWaterUse
    [0.5 1.15] ModerateLivestockTotalWaterUse
    [:<  0.5]  LowLivestockTotalWaterUse))

;;Agricultural surface water use. Step 2: Consider proximity to surface water.
(defmodel surface-water-proximity ProximityToSurfaceWaterClass
  (classification (measurement ProximityToSurfaceWater "m")
    [500 :>]     LowSurfaceWaterProximity
    [250 500]    ModerateSurfaceWaterProximity
    [:< 250]     HighSurfaceWaterProximity))

;;Agricultural surface water use. Step 3: Estimate crop irrigation water needs.
(defmodel irrigation-water-use IrrigationWaterUseClass
  (measurement IrrigationWaterUse "mm"  ;;This is an annual value
     :context ((categorization veracruz-lulc:VeracruzLULCCategory :as lulc))
     :state   #(if (= (:lulc %) "riego")
                  2000
                  0)))

;;Classification of irrigationWaterUse into 6 classes.
(defmodel irrigation-water-use-discretized IrrigationWaterUseClass
  (classification irrigation-water-use
    0           NoIrrigationUse
    [:< 1600]   VeryLowIrrigationUse
    [1600 1850] LowIrrigationUse
    [1850 2150] ModerateIrrigationUse
    [2150 2400] HighIrrigationUse
    [2400 :>]   VeryHighIrrigationUse))

;;Undiscretization of agricultural surface water use
(defmodel use-undiscretizer AgriculturalSurfaceWaterUseClass
  (probabilistic-measurement AgriculturalSurfaceWaterUseClass "mm" 
;;    [2000 :>]    HighAgriculturalSurfaceWaterUse
    [2000 3000]    HighAgriculturalSurfaceWaterUse
    [1000 2000]  ModerateAgriculturalSurfaceWaterUse
    [0 1000]    LowAgriculturalSurfaceWaterUse)) 

(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUseClass
  (bayesian AgriculturalSurfaceWaterUseClass  
    :import   "aries.core::SurfaceWaterUseAgriculture.xdsl"
    :context  (surface-water-proximity livestock-total-water-use-discretized irrigation-water-use-discretized)
    :keep     (AgriculturalSurfaceWaterUseClass)
    :observed (use-undiscretizer))) 

;;Below would be the logical and simple way to do things.  However these features are not yet enabled.

;;Agricultural surface water use. Step 5: Add crop irrigation and livestock surface water use.
 ;;(defmodel agricultural-surface-water-use AgriculturalSurfaceWaterUse
  ;;(measurement AgriculturalSurfaceWaterUse "mm"  ;;This is an annual value
    ;;  :context (irrigation-water-use livestock-surface-water-use)
    ;;  :state    #(+ (:irrigation-water-use %) (:livestock-surface-water-use %))))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;Everything below needs to be updated correctly for water.
 	 								
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel flow-direction geophysics:FlowDirection
  (ranking geophysics:FlowDirection)) 

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))	

(defmodel surface-water-flow-data TempSurfaceWaterData
  (identification TempSurfaceWaterData
    :context (altitude streams))) 								
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data WaterSupply 
  (identification WaterSupply 
  :context (irrigation-water-use
            agricultural-surface-water-use
            industrial-users
            non-rival-water-users
            sink
            precipitation-annual
            streams)))

;; flow model for surface water
(defmodel surface-flow SurfaceWaterMovement
  (span SurfaceWaterMovement
        AnnualPrecipitation
        AgriculturalSurfaceWaterUseClass
        SurfaceWaterSinkClass
        nil
        (geophysics:Altitude geofeatures:River)
        :source-threshold   1500.0
        :sink-threshold     25.0
        :use-threshold      500.0
        :trans-threshold    10.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :save-file          "/home/gjohnson/code/java/imt/identifications/water_la_antigua_data.clj"
        :keep               (SurfaceWaterSupply
                             MaximumSurfaceWaterSink
                             SurfaceWaterDemand
                             PossibleSurfaceWaterFlow
                             PossibleSurfaceWaterSupply
                             PossibleSurfaceWaterUse
                             ActualSurfaceWaterFlow
                             UsedSurfaceWaterSupply
                             ActualSurfaceWaterSink
                             SatisfiedSurfaceWaterDemand
                             UnusableSurfaceWaterSupply
                             UnusableSurfaceWaterSink
                             InaccessibleSurfaceWaterDemand
                             SunkSurfaceWaterFlow
                             SunkSurfaceWaterSupply
                             BlockedSurfaceWaterDemand)
        :context            (precipitation-annual
                             sink
                             agricultural-surface-water-use
                             surface-water-flow-data)))
