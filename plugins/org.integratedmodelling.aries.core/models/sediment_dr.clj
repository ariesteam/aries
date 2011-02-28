(ns core.models.sediment-dr
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification 
                            namespace-ontology categorization ranking numeric-coding
                            probabilistic-measurement probabilistic-classification probabilistic-ranking 
                            binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; fv - shouldn't this be soilretentionService?
(namespace-ontology soilretentionEcology)

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel soil-group HydrologicSoilsGroup
	"Relevant soil group"
	(classification (ranking habitat:HydrologicSoilsGroup)
			1       SoilGroupA
			2       SoilGroupB
			3       SoilGroupC
			4       SoilGroupD))

(defmodel slope SlopeClass
	(classification (measurement geophysics:DegreeSlope "\u00b0")
			 [0 1.15] 	  Level
			 [1.15 4.57] 	GentlyUndulating
			 [4.57 16.70] RollingToHilly
			 [16.70 :>] 	SteeplyDissectedToMountainous))

(defmodel soil-texture SoilTextureClass
    (classification (categorization habitat:SoilTexture)
      "Coarse"    CoarseSoilTexture
      "Medium"    MediumSoilTexture
      "Fine"      FineSoilTexture)) 

;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
     (classification (ranking habitat:SoilErodibility)
       [:< 0.02]         VeryLowSoilErodibility
       [0.02 0.0275]     LowSoilErodibility
       [0.0275 0.0325]   ModerateSoilErodibility
       [0.0325 0.0375]   HighSoilErodibility
       [0.0375 :>]       VeryHighSoilErodibility))

;;Annual precipitation for Mg & DR
(defmodel precipitation-annual AnnualPrecipitationClass
	"FIXME this is annual precipitation."
	(classification (measurement habitat:AnnualPrecipitation "mm")
    [:< 600] 	    VeryLowAnnualPrecipitation
		[600 1200] 	  LowAnnualPrecipitation
		[1200 1800]   ModerateAnnualPrecipitation
		[1800 2200] 	HighAnnualPrecipitation
		[2200 :>] 	  VeryHighAnnualPrecipitation))

;;Tropical storm probability, use only in DR & Mg
(defmodel storm-probability TropicalStormProbabilityClass
 (classification (ranking habitat:TropicalStormProbability)
      [5 :>]         HighTropicalStormProbability  ;;In Madagascar this is set as >5, but in DR the
      [1 5]          ModerateTropicalStormProbability
      :otherwise     NoTropicalStormProbability)) 


;;Annual runoff, whereas snowmelt, precipitation, and temperature are monnthly, so this is problematic.
;;Could divide yearly runoff by 12 but obviously its not evenly distributed throughout the year.
;;Or could strongly consider just running it on an annual time step, as that's what the data support.
(defmodel runoff AnnualRunoffClass
	(classification (measurement habitat:AnnualRunoff "mm")
		[0 200] 	    VeryLowAnnualRunoff
		[200 600] 	  LowAnnualRunoff
		[600 1200]  	ModerateAnnualRunoff
		[1200 2400] 	HighAnnualRunoff
		[2400 :>] 	  VeryHighAnnualRunoff))

;;Vegetation type
;; FV the models in here are DR-specific so I see no point in using general specs. It does
;; create problems because the BNs are not prepared to get the GLC classes, which do come
;; up when the DR data have holes. Commenting out the non-DR contingencies - we should put them
;; back when models are general, but then only after making the BNs aware of all possible values.
(defmodel vegetation-type VegetationType
	"Just a reclass of the NLCD land use layer"
;	(classification (numeric-coding nlcd:NLCDNumeric)
;		#{41 42 43 71 90 95} ForestGrasslandWetland
;		#{52 81}             ShrublandPasture
;		#{21 22 23 24 31 82} CropsBarrenDeveloped)
;  (classification (numeric-coding mglulc:MGLULCNumeric)
;    #{1 2 4 5 6 10 14}                         ForestWetland
;    #{3 7 23}                                  DegradedForest
;		#{8 9 20 21 22 24 25 26 28 29 30 31 32 33} Savanna
;    #{11 12 13 16 17}                          CroplandDeveloped)
  (classification (numeric-coding domlulc:DOMLULCNumeric)
    #{1 2 4 6 8 9 11 18 35} ForestAndShrubland
    #{22 24 62}             WaterWetlandsMangroves
	 	#{41 45 53}             ShadeCoffeeCocoa
    #{23 36 38 40 59}       IntensiveCroplandAndPasture
    #{42}                   UrbanAndRoads)
;  (classification (numeric-coding glc:GLCNumeric)
;		#{1 2 3 4 5 6 7 8 9 15} ForestGrasslandWetland
;		#{10 11 12 13 14 17 18} ShrublandPasture
;    #{16 19 22}             CropsBarrenDeveloped)
)

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover PercentVegetationCoverClass
	(classification (numeric-coding habitat:PercentVegetationCover)
		[70 100]  HighVegetationCover
		[30 70]   ModerateVegetationCover
		[1 30]    LowVegetationCover))

;;Sediment source value
(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
  ;; FV - sorry, my bad - theres a bug so the right way doesn't work as a prototype obs. Will be fixed asap.
  ;; please leave as is for now or the BN won't compile.
	(probabilistic-measurement SedimentSourceValueAnnualClass "t/ha"
;	(classification (measurement SedimentSourceValueAnnual "t/ha")
      [0 0.01]              NoAnnualSedimentSource
      [0.01 15]             LowAnnualSedimentSource 
      [15 40]               ModerateAnnualSedimentSource
      [40 100]              HighAnnualSedimentSource))
  		
;; source bayesian model for Dominican Republic
;; FV there is much evidence setting for intermediate nodes here - those should be used for
;; training, when the PI eventually implements it. Commented those below.
(defmodel source-dr SedimentSourceValueAnnual
  (bayesian SedimentSourceValueAnnual
    :import   "aries.core::SedimentSourceValueDRAdHoc.xdsl"
    :keep     (SedimentSourceValueAnnualClass)
    :required (SlopeClass)
    :result   sediment-source-value-annual 
    :context  (soil-group slope soil-texture 
              (comment  soil-erodibility)
              precipitation-annual  
              storm-probability 
              (comment runoff) 
              vegetation-type percent-vegetation-cover))) 

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs ReservoirsClass 
  (classification (binary-coding geofeatures:Reservoir)
      1          ReservoirPresent
      :otherwise ReservoirAbsent))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00B0")
    [:<   1.15]  LowStreamGradient
    [1.15 2.86]  ModerateStreamGradient
    [2.86 :>]    HighStreamGradient))

(defmodel floodplain-vegetation-cover FloodplainVegetationCoverClass 
  (classification (numeric-coding habitat:PercentFloodplainVegetationCover)
    [0 20]   VeryLowFloodplainVegetationCover
    [20 40]  LowFloodplainVegetationCover
    [40 60]  ModerateVegetationCover
    [60 80]  HighFloodplainVegetationCover
    [80 100] VeryHighFloodplainVegetationCover))

(defmodel floodplains Floodplains
  (classification (binary-coding geofeatures:Floodplain)
      0 NotInFloodplain
      1 InFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual AnnualSedimentSinkClass 
  ;; FV temporarily subst with dumb classification - see comment for sediment-source-value-annual
  (probabilistic-measurement AnnualSedimentSinkClass "t/ha"
;;  (classification (measurement AnnualSedimentSink "t/ha")
       [10 15]              HighAnnualSedimentSink
       [5 10]               ModerateAnnualSedimentSink
       [0.01 5]             LowAnnualSedimentSink
       [0 0.01]             NoAnnualSedimentSink)) 

;;If we successfully get FPWidth data for Mg & DR, add these to the "context" part of the model.
(defmodel sediment-sink-dr AnnualSedimentSink
  (bayesian AnnualSedimentSink 
    :import  "aries.core::SedimentSinkDR.xdsl"
    :keep    (AnnualSedimentSinkClass)
    :required (StreamGradientClass)
    :result   sediment-sink-annual 
    :context  (reservoirs stream-gradient floodplain-vegetation-cover)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

;;(defmodel floodplains FloodplainsClass
;;	(classification (binary-coding geofeatures:Floodplain)
;;			0          InFloodplain
;;			:otherwise NotInFloodplain))

;;(defmodel farmland Farmland
;;	"Just a reclass of the regionally appropriate LULC layer"
;;  (classification (binary-coding FarmlandCode)
;;    1          FarmlandPresent
;;    0          FarmlandAbsent))
;;Above statement (soilretentionEcology:Farmland) is for coffee farmers in the DR; to use farmland 
;; from DR LULC data, comment out the above and turn on the statement below (domlulc:DOMLULCNumeric)
(defmodel farmland FarmlandCode
  (classification (numeric-coding domlulc:DOMLULCNumeric)
	  	#{23 36 38 40 41 45 53 59}	FarmlandPresent
		  :otherwise                  FarmlandAbsent))

;;Reservoirs use for DR: presence/absence only.
(defmodel hydroelectric-use-presence HydroelectricUsePresenceClass
	(classification (binary-coding geofeatures:Reservoir)
			0			HydroelectricUseAbsent
			1			HydroelectricUsePresent))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-dr DepositionProneFarmers 
  (binary-coding DepositionProneFarmers
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 1.0))
                    1
                    0)
       :context (
          (binary-coding FarmlandCode           :as farmlandpresent)
          (binary-coding geofeatures:Floodplain :as floodplains)))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
;; FV FIXME I don't see any SedimentSourceValueAnnual in the context???
(defmodel farmers-erosion-use-dr ErosionProneFarmers
  (ranking ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 1.0)
                  (cond (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                        1
                        (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                        2
                        :otherwise
                        0)
                  0)
       :context ((binary-coding FarmlandCode :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;(defmodel fishermen-use-puget FishermenUse 
	  ;;(bayesian FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUse.xdsl"
	 ;; 	:keep     (FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density)))

;;defmodel fishermen-use-mg FishermenUse 
	;;  (bayesian FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUseMg.xdsl"
	 ;; 	:keep     (FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands mangroves reefs seagrass population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
   (identification FarmlandSoilDeposition 
     :context (
       source-dr
       sediment-sink-dr
       farmers-deposition-use-dr
       altitude
       streams)))

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
   (identification ReservoirSoilDeposition 
     :context (
       source-dr
       sediment-sink-dr
       hydroelectric-use-presence
       altitude
       streams)))

;;Sediment flow model for recipients of beneficial sedimentation
(defmodel sediment-beneficial BeneficialSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnual 
        DepositionProneFarmers
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep (MaximumSedimentSource MaximumPotentialDeposition 
               PotentialSedimentDepositionBeneficiaries PossibleSedimentFlow
               PossibleSedimentSource PossibleSedimentDepositionBeneficiaries
               ActualSedimentFlow ActualSedimentSource
               UtilizedDeposition ActualSedimentDepositionBeneficiaries
               UnutilizedSedimentSource InaccessibleSedimentDepositionBeneficiaries
               AbsorbedSedimentFlow NegatedSedimentSource
               LostValuableSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains streams))) 

;;Sediment flow model for recipients of avoided detrimental sedimentation
(defmodel sediment-detrimental-farmers DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnual 
        DepositionProneFarmers ;;change the beneficiary group as needed
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep (MaximumSedimentSource MaximumPotentialDeposition 
               PotentialReducedSedimentDepositionBeneficiaries PossibleSedimentFlow
               PossibleSedimentSource PossibleReducedSedimentDepositionBeneficiaries
               ActualSedimentFlow ActualSedimentSource
               UtilizedDeposition ActualReducedSedimentDepositionBeneficiaries
               UnutilizedDeposition AbsorbedSedimentFlow
               NegatedSedimentSource BlockedHarmfulSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains streams))) ;;change the beneficiary group as needed

;;Sediment flow model for recipients of avoided detrimental sedimentation
(defmodel sediment-detrimental-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnualClass 
        HydroelectricUsePresenceClass  ;;change the beneficiary group as needed
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep (MaximumSedimentSource MaximumPotentialDeposition 
               PotentialReducedSedimentDepositionBeneficiaries PossibleSedimentFlow
               PossibleSedimentSource PossibleReducedSedimentDepositionBeneficiaries
               ActualSedimentFlow ActualSedimentSource
               UtilizedDeposition ActualReducedSedimentDepositionBeneficiaries
               UnutilizedDeposition AbsorbedSedimentFlow
               NegatedSedimentSource BlockedHarmfulSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-dr hydroelectric-use-presence sediment-sink-dr altitude floodplains streams))) ;;change the beneficiary group as needed


;;Sediment flow model for recipients of reduced turbidity
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        SedimentSourceValueAnnual 
        DepositionProneFarmers  ;;change the beneficiary group as needed
        AnnualSedimentSinkClass 
        nil
        (geophysics:Altitude Floodplains geofeatures:River)
        :source-threshold   2.0
        :sink-threshold     1.0
        :use-threshold      0.5
        :trans-threshold    0.25
        :source-type        :finite
        :sink-type          :finite
        :use-type           :finite
        :benefit-type       :non-rival
        :rv-max-states      10
        :downscaling-factor 2
        :keep (MaximumSedimentSource MaximumPotentialDeposition 
               PotentialReducedTurbidityBeneficiaries PossibleSedimentFlow
               PossibleSedimentSource PossibleReducedTurbidityBeneficiaries
               ActualSedimentFlow ActualSedimentSource
               UtilizedDeposition ActualReducedTurbidityBeneficiaries
               UnutilizedDeposition AbsorbedSedimentFlow 
               NegatedSedimentSource ReducedTurbidity)
        ;;:save-file          (str (System/getProperty "user.home") "/carbon_data.clj")
        :context (source-dr farmers-deposition-use-dr sediment-sink-dr altitude floodplains streams))) ;;change the beneficiary group as needed