(ns core.models.sediment-puget
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization 
                            namespace-ontology ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification probabilistic-ranking 
                            identification bayesian count))
  (:refer aries :only (span)))

;; This is the model for Puget Sound.

;; fv - shouldn't this be soilretentionService?
;; kb - yes, but need to refactor files, as all the concepts are in soilretentionEcology and some concepts are also in 
;; soilretentionservice -> this was an old naming convention we used, you see it with salmon & nutrients as well ("service"
;; and "ecology" ontologies that should be merged)
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

(defmodel slope-stability SlopeStabilityClass
    (classification (numeric-coding habitat:SlopeStability)	 		
            1           HighSlopeStability
	 		2           ModerateSlopeStability
	 		3           LowSlopeStability))

;;This discretization is for SSURGO/STATSGO, paying attention to texture over inclusion of various sized rock fragments.
(defmodel soil-texture SoilTextureClass
    (classification (numeric-coding habitat:SoilTexture)
      #{2 3 8 9 12 13 15 17 18 19 20 21 22 25 26 27 29 31 32 34 35 36 37 39 40 43 47 48 50 51 55 59 62 64 65 66 67 68 69 73 74 75 76 78 79 81 82 84 85 86 87 88 89 91 92 96 98 99 105 107 108 109 110 111 112 114 115 117 118 121 123 125 127 128 129 130 132 133 134 137 139 141 142 143 144 147 150 152 153 154 155 157 159 160 161 162 164 165 167 172 173 175 176 180 184 185 187 190 191 192 195} CoarseSoilTexture
      #{1 4 5 6 10 11 14 24 28 30 33 38 42 49 57 60 61 63 70 71 72 77 80 83 90 93 94 95 97 102 103 104 116 124 126 140 151 163 166 168 169 179 181 189} MediumSoilTexture
      #{7 16 23 41 44 45 46 52 53 54 56 58 100 101 106 113 119 120 122 131 135 136 138 145 146 148 149 156 170 171 174 177 182 183 186 188 193 194} FineSoilTexture))

;;Soil erodibility factor from USLE (unitless).
(defmodel soil-erodibility SoilErodibilityClass
     (classification (numeric-coding habitat:SoilErodibility)
       [:< 0.1]    VeryLowSoilErodibility
       [0.1 0.225]   LowSoilErodibility
       [0.225 0.3]   ModerateSoilErodibility
       [0.3 0.375]   HighSoilErodibility
       [0.375 :>]     VeryHighSoilErodibility))

(defmodel precipitation-annual AnnualPrecipitationClass
	"FIXME this is annual precipitation."
	(classification (measurement habitat:AnnualPrecipitation "mm")
    [:< 600] 	    VeryLowAnnualPrecipitation
		[600 1200] 	  LowAnnualPrecipitation
		[1200 1800]   ModerateAnnualPrecipitation
		[1800 2200] 	HighAnnualPrecipitation
		[2200 :>] 	  VeryHighAnnualPrecipitation))

(defmodel runoff AnnualRunoffClass
	(classification (measurement habitat:AnnualRunoff "mm")
		[0 200] 	    VeryLowAnnualRunoff
		[200 600] 	  LowAnnualRunoff
		[600 1200]  	ModerateAnnualRunoff
		[1200 2400] 	HighAnnualRunoff
		[2400 :>] 	  VeryHighAnnualRunoff))

;;CANT do a global vegetation type defmodel if classes are different: split this up & use the local
;; vegetation type defmodel into the BN
;;Vegetation type
(defmodel vegetation-type VegetationType
	"Just a reclass of the NLCD land use layer"
	(classification (numeric-coding nlcd:NLCDNumeric)
		#{41 42 43 71 90 95} ForestGrasslandWetland
		#{52 81}             ShrublandPasture
		#{21 22 23 24 31 82} CropsBarrenDeveloped))

;;Discretization based on Quinton et al. (1997)
(defmodel percent-vegetation-cover PercentVegetationCoverClass
  (classification (numeric-coding habitat:PercentVegetationCover)
    [70 100 :inclusive]  HighVegetationCover
    [30 70]              ModerateVegetationCover
    [0 30]               LowVegetationCover))

(defmodel successional-stage SuccessionalStageClass
	 (classification (numeric-coding ecology:SuccessionalStage)  
	 		#{5 6}      OldGrowth
	 		4           LateSuccession
	 		3           MidSuccession
      2           PoleSuccession
	 		1           EarlySuccession
	 		:otherwise  NoSuccession))

;;Sediment source value - we have evidence for this but can't yet train so keep this commented out for now and use the
;; undiscretization statement below (?)
;;(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
;; (classification (measurement SedimentSourceValueAnnualClass "kg/ha")
;;  		0                          NoAnnualSedimentSource
;;  		[:exclusive 0 30000]       LowAnnualSedimentSource 
;;  		[30000 100000]             ModerateAnnualSedimentSource
;;  		[100000 :>]                HighAnnualSedimentSource))

(defmodel sediment-source-value-annual SedimentSourceValueAnnualClass
 (probabilistic-measurement SedimentSourceValueAnnualClass "kg/ha"
      [0 0.01]                   NoAnnualSedimentSource
      [0.01 30000]               LowAnnualSedimentSource 
      [30000 100000]             ModerateAnnualSedimentSource
      [100000 300000]            HighAnnualSedimentSource))

;; source bayesian model for Puget Sound   	 
(defmodel source-puget SedimentSourceValueAnnual
  (bayesian SedimentSourceValueAnnual 
    :import   "aries.core::SedimentSourceValueAdHoc.xdsl"
    :keep     (SedimentSourceValueAnnualClass) 
    :required (SlopeClass)
    :result   sediment-source-value-annual
    :context  (soil-group slope soil-texture precipitation-annual vegetation-type percent-vegetation-cover 
              successional-stage slope-stability)))

;; Add deterministic model for USLE: Have data for it for the western U.S. and globally.

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs-class ReservoirsClass 
  (classification (binary-coding geofeatures:Reservoir)
      1          ReservoirPresent
      :otherwise ReservoirAbsent))

(defmodel stream-gradient StreamGradientClass 
  (classification (measurement habitat:StreamGradient "\u00b0")
    [:<   1.15]  LowStreamGradient
    [1.15 2.86]  ModerateStreamGradient
    [2.86 :>]    HighStreamGradient))

(defmodel floodplain-vegetation-cover FloodplainVegetationCoverClass 
  (classification (ranking habitat:PercentFloodplainVegetationCover)
    [0 20]              VeryLowFloodplainVegetationCover
    [20 40]             LowFloodplainVegetationCover
    [40 60]             ModerateVegetationCover
    [60 80]             HighFloodplainVegetationCover
    [80 100 :inclusive] VeryHighFloodplainVegetationCover))

(defmodel floodplain-width FloodplainWidthClass 
  (classification (measurement habitat:FloodplainWidth "m")
    [0 350]     VeryNarrowFloodplain
    [350 800]   NarrowFloodplain
    [800 1300]  WideFloodplain
    [1300 :>]   VeryWideFloodplain))

;;These are arbitrary numbers discretized based on the "low" soil erosion level defined by the US & global datasets, respectively.
;; Have these numbers reviewed by someone knowledgable about sedimentation.
(defmodel sediment-sink-annual AnnualSedimentSinkClass 
  (probabilistic-measurement AnnualSedimentSinkClass "kg/ha"
       [20000 30000]          HighAnnualSedimentSink
       [10000 20000]          ModerateAnnualSedimentSink
       [0.01 10000]           LowAnnualSedimentSink
       [0 0.01]               NoAnnualSedimentSink)) 

(defmodel sediment-sink-us AnnualSedimentSink
  (bayesian AnnualSedimentSink    
    :import  "aries.core::SedimentSink.xdsl"
    :keep     (AnnualSedimentSinkClass)
    :required (FloodplainWidthClass)
    :result   sediment-sink-annual
    :context  (reservoirs-class stream-gradient floodplain-vegetation-cover floodplain-width)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

(defmodel reservoirs geofeatures:Reservoir
  (binary-coding geofeatures:Reservoir))

(defmodel floodplains Floodplains
	(classification (categorization geofeatures:Floodplain)
			#{"A" "X500"} InFloodplain
			:otherwise    NotInFloodplain))

(defmodel farmland Farmland
	"Just a reclass of the regionally appropriate LULC layer"
	(classification (numeric-coding nlcd:NLCDNumeric)
		82	       FarmlandPresent
		:otherwise FarmlandAbsent))

;;Use normal dam storage (ac-ft in the U.S. or m^3 in the rest of the world) as a proxy for 
;;hyroelectric generation capacity (use) - in reality dam height & flow are important factors but 
;;we don't have flow data.

;; Need to insert different discretizations for the US and global models
(defmodel hydroelectric-use-level HydroelectricUseLevel
  (measurement HydroelectricUseLevel "m^3" :as hydro-use-level))

;; Models farmland in the floodplain, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-deposition-use-puget DepositionProneFarmers 
  (binary-coding DepositionProneFarmers
       :context ((ranking        nlcd:NLCDNumeric       :as farmlandpresent)
                 (categorization geofeatures:Floodplain :as floodplains))
       :state #(if (and (= (:floodplains %) 1.0)
                        (= (:farmlandpresent %) 82.0))
                    1
                    0))) 

;; Models farmland in regions with erodible soils, the non-Bayesian way (i.e., basic spatial overlap).
(defmodel farmers-erosion-use-puget ErosionProneFarmers
  (ranking ErosionProneFarmers
       :state #(if (= (:farmlandpresent %) 82.0)
                  (cond (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:ModerateAnnualSedimentSource))
                        1
                        (= (:sediment-source-value-annual %) (tl/conc 'sedimentretentionEcology:HighAnnualSedimentSource))
                        2
                        :otherwise
                        0)
                  0)
       :context ((ranking nlcd:NLCDNumeric :as farmlandpresent))))

;;Still need defmodels for all components of fisheries BNs.  What about deterministic nodes?
;;Need an undiscretization defmodel before this, for the "observed"? In the long run, could take 2 paths:
;; 1) ditch fisheries BNs & use source/use models for actual fisheries
;; 2) use BNs as generalized fisheries impact model.
;;(defmodel fishermen-use-puget FishermenUse 
	  ;;(bayesian FishermenUse  
	 ;; 	:import   "aries.core::SedimentFishermenUse.xdsl"
	 ;; 	:keep     (FishermenUse)
	 ;;	 	:context  (lakes rivers coastline coastal-wetlands salmon-spawning-grounds public-access population-density)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))	 								

(defmodel levees infrastructure:Levee
  (binary-coding infrastructure:Levee))

(defmodel streams geofeatures:River
  (binary-coding geofeatures:River))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

(defmodel reservoir-soil-deposition-data ReservoirSoilDeposition
   (identification ReservoirSoilDeposition 
     :context (
       source-puget
       sediment-sink-us
       hydroelectric-use-level
       levees)))

(defmodel farmland-soil-deposition-data FarmlandSoilDeposition
   (identification FarmlandSoilDeposition 
     :context (
       source-puget
       sediment-sink-us
       farmers-deposition-use-puget
       levees)))

;; Sediment flow model for farmers in floodplains
(defmodel sediment-farmers BeneficialSedimentTransport ;; or DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnual
        DepositionProneFarmers
        AnnualSedimentSink
        nil
        (geophysics:Altitude Floodplains infrastructure:Levee geofeatures:River) 
        :source-threshold   1000.0 ;;Note that threshold values are different in the Puget sediment SPAN models than in DR or Mg. This is because units are different, so keep these values (or similar ones)
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        :keep (MaximumSedimentSource                       MaximumPotentialDeposition 
               PotentialSedimentDepositionBeneficiaries    PossibleSedimentFlow
               PossibleSedimentSource                      PossibleSedimentDepositionBeneficiaries
               ActualSedimentFlow                          ActualSedimentSource
               UtilizedDeposition                          ActualSedimentDepositionBeneficiaries
               UnutilizedSedimentSource                    InaccessibleSedimentDepositionBeneficiaries
               AbsorbedSedimentFlow                        NegatedSedimentSource
               LostValuableSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_farmers_data.clj")
        :context (source-puget farmers-deposition-use-puget sediment-sink-us altitude levees streams floodplains)))

;; Sediment flow model for deposition in hydro reservoirs
(defmodel sediment-reservoirs DetrimentalSedimentTransport
  (span SedimentTransport
        SedimentSourceValueAnnual
        Reservoirs
        AnnualSedimentSink
        nil
        (geophysics:Altitude Floodplains infrastructure:Levee geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        :keep (MaximumSedimentSource                                MaximumPotentialDeposition 
               PotentialReducedSedimentDepositionBeneficiaries      PossibleSedimentFlow
               PossibleSedimentSource                               PossibleReducedSedimentDepositionBeneficiaries
               ActualSedimentFlow                                   ActualSedimentSource
               UtilizedDeposition                                   ActualReducedSedimentDepositionBeneficiaries
               UnutilizedDeposition                                 AbsorbedSedimentFlow
               NegatedSedimentSource                                BlockedHarmfulSediment)
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_reservoirs_data.clj")
        :context (source-puget reservoirs sediment-sink-us altitude levees streams floodplains)))

;; FIXME: There is no WaterIntakeUse observation defined above.
;; Sediment flow model for assessing turbidity
(defmodel sediment-turbidity DetrimentalTurbidity
  (span SedimentTransport
        SedimentSourceValueAnnual
        WaterIntakeUse  ;;change the beneficiary group as needed.  This one is for drinking water intakes (though we currently lack information on their location)
        AnnualSedimentSink 
        nil
        (geophysics:Altitude Floodplains infrastructure:Levee geofeatures:River)
        :source-threshold   1000.0
        :sink-threshold      500.0
        :use-threshold         0.0
        :trans-threshold     100.0
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :non-rival ;; this will cause the model to store sediment values on users who are not co-located with sinks
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        :keep (MaximumSedimentSource                            MaximumPotentialDeposition 
               PotentialReducedTurbidityBeneficiaries           PossibleSedimentFlow
               PossibleSedimentSource                           PossibleReducedTurbidityBeneficiaries
               ActualSedimentFlow                               ActualSedimentSource
               UtilizedDeposition                               ActualReducedTurbidityBeneficiaries
               UnutilizedDeposition                             AbsorbedSedimentFlow 
               NegatedSedimentSource                            ReducedTurbidity)
        ;;:save-file          (str (System/getProperty "user.home") "/sediment_turbidity_data.clj")
        :context (source-puget sediment-sink-us altitude levees streams floodplains))) ;;change the beneficiary group as needed
