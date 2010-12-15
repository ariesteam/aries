;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; --------------------------------------------------------------------------------------------------

(ns marine.models.coastal
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))
      
;; --------------------------------------------------------------------------------------
;; Source models
;; --------------------------------------------------------------------------------------

;;(defmodel storm-probability 'coastalProtection:TropicalStormProbability
;;  (ranking 'habitat:TropicalStormProbability))

(defmodel bathymetry-class 'coastalProtection:BathymetryClass
  (classification (measurement 'geophysics:Bathymetry "m")
    [0 :>]              'coastalProtection:Overland
    [0 -20]             'coastalProtection:VeryShallow
    [-20 -50]           'coastalProtection:Shallow
    [-50 -200]          'coastalProtection:Deep
    [:< -200]           'coastalProtection:VeryDeep))

(defmodel atmospheric-pressure 'coastalProtection:AtmosphericPressureClass
  (classification (measurement 'geophysics:AtmosphericPressure "mb") ;;check to see if this is a valid unit.
    [990 :>]  'coastalProtection:ModeratelyLowAtmosphericPressure
    [970 990] 'coastalProtection:LowAtmosphericPressure
    [:< 970]  'coastalProtection:VeryLowAtmosphericPressure))

;;Discretization based on the Southwest Indian Ocean Tropical Cyclone Scale. May need a different
;; scale for other parts of the world.
(defmodel wind-speed 'coastalProtection:WindSpeedClass
  (classification (measurement 'geophysics:WindSpeed "km/h") ;;check to see if this is a valid unit.
     [165 :>]    'coastalProtection:VeryHighWindSpeed
     [117 165]   'coastalProtection:HighWindSpeed
     [88 117]    'coastalProtection:ModeratelyHighWindSpeed
     [62 88]     'coastalProtection:ModeratelyLowWindSpeed
     [50 62]     'coastalProtection:LowWindSpeed
     [:< 50]     'coastalProtection:VeryLowWindSpeed))

;;Undiscretization & BN statements
(defmodel storm-surge 'coastalProtection:StormSurgeClass
  (classification 'coastalProtection:StormSurgeClass
                  :units      "m"
                  [5 :>]      'coastalProtection:VeryHighStormSurge
                  [4 5]       'coastalProtection:HighStormSurge
                  [3 4]       'coastalProtection:ModerateStormSurge
                  [2 3]       'coastalProtection:LowStormSurge
                  [1 2]       'coastalProtection:VeryLowStormSurge
                  [:< 1]      'coastalProtection:NoStormSurge))

(defmodel coastal-wave-source 'coastalProtection:CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian 'coastalProtection:CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     ('coastalProtection:StormSurgeClass)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class)))

;; --------------------------------------------------------------------------------------
;; Sink (coastal protection) model
;; --------------------------------------------------------------------------------------

(defmodel mangrove 'coastalProtection:MangrovePresenceClass
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
    5          'coastalProtection:MangrovePresent
    :otherwise 'coastalProtection:MangroveAbsent))

;; Most polygons do not report any data on bleaching, these are placed under "moderate bleaching".  There's a misspelling for "HIgh" in the data that's accounted for in the discretization below.
(defmodel coral-quality 'coastalProtection:CoralQuality
	(classification (categorization 'coastalProtection:CoralBleaching)
 		#{"None" "Low"}			   'coastalProtection:MinimallyBleachedCoralPresent
 		#{"HIgh" "High"}	     'coastalProtection:HighlyBleachedCoralPresent
 	  #{"Moderate" ""}       'coastalProtection:ModeratelyBleachedCoralPresent
    :otherwise             'coastalProtection:NoCoralPresent))
 
;; TODO only two classes represented from presence/absence; no idea how to 
;; model density based on existing data.
(defmodel seagrass 'coastalProtection:SeagrassPresenceClass
	(classification (binary-coding 'coastalProtection:SeagrassPresence)
		0 'coastalProtection:SeagrassAbsent
		1 'coastalProtection:SeagrassPresent))

(defmodel dune 'coastalProtection:DunePresenceClass
  (classification (categorization 'geofeatures:Dune)
    #{"dune"}   'coastalProtection:DunePresent    
    :otherwise  'coastalProtection:DuneAbsent))

;; THIS NEEDS TO BE FIXED - TALK TO BRIAN 12/15
;; Ken: units uncertain, layer is horribly large and cumbersome to work with
;; TODO check - BN has only 4 classes, so I put the last 2 together and eliminated
;; VeryHighSlope with breakpoint at 9,000,000
(defmodel slope 'coastalProtection:BathymetricSlope
	(classification (ranking 'geophysics:BathymetricSlope)
		[4000000 :>]      'coastalProtection:HighSlope
		[2000000 4000000] 'coastalProtection:ModerateSlope
		[200000 2000000]  'coastalProtection:LowSlope
		[:< 200000]      'coastalProtection:VeryLowSlope))

;;Terrestrial vegetation types from Mg LULC layer
(defmodel terrestrial-vegetation 'coastalProtection:TerrestrialVegetationType
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
         #{1 3 4 8 10 20 21 23 30 31}             'coastalProtection:Forested ;;Includes tree-dominated savannas
         #{6 7}                                   'coastalProtection:Shrubland
         #{14}                                    'coastalProtection:Wetland
         #{9 11 12 13 18 22 24 25 26 28 29 32 33} 'coastalProtection:Herbaceous ;;Includes agriculture, grass-dominated savannas
         #{16 17 19 27}                           'coastalProtection:Unvegetated)) 

(defmodel depth-elevation 'coastalProtection:OceanDepthAndLandElevation
  (classification (measurement 'geophysics:Bathymetry "m")
    [20 :>]             'coastalProtection:HighLandElevation
    [5 20]              'coastalProtection:ModerateLandElevation
    [0 5]               'coastalProtection:LowLandElevation
    [0 -60]             'coastalProtection:Pelagic
    [-60 -200]          'coastalProtection:Shelf
    [-200 -2000]        'coastalProtection:Slope
    [:< -2000]          'coastalProtection:DeepWater))

;;Assumes some artificial flood protection near Toamasina, the main port city in Madagascar.  Development around the small ports is minimal.
(defmodel artificial-coastal-protection 'coastalProtection:ArtificialCoastalProtection
  (classification (binary-coding 'infrastructure:Port)
    3          'coastalProtection:ArtificialCoastalProtectionPresent
    :otherwise 'coastalProtection:ArtificialCoastalProtectionAbsent))

;;FIX DISCRETIZATION BELOW (i.e., value ranges for mitigation)
(defmodel coastal-flood-protection 'coastalProtection:TotalCoastalFloodProtection
  (classification 'coastalProtection:TotalCoastalFloodProtection
                  :units      "m"
                  [3 :>]        'coastalProtection:HighCoastalFloodProtection
                  [1 3]         'coastalProtection:ModerateCoastalFloodProtection
                  [0.1 1]       'coastalProtection:LowCoastalFloodProtection
                  [0 0.1]       'coastalProtection:NoCoastalFloodProtection))

;; flood protection
(defmodel coastal-flood-sink 'coastalProtection:CoastalFloodSink
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'coastalProtection:CoastalFloodSink 
	  	:import   "aries.marine::CoastalFloodSink.xdsl"
	  	:keep     ('coastalProtection:TotalCoastalFloodProtection)
      :observed (coastal-flood-protection)
	 	 	:context  (mangrove coral-quality seagrass dune terrestrial-vegetation artificial-coastal-protection
                 depth-elevation slope)))

;; --------------------------------------------------------------------------------------
;; Use models
;; --------------------------------------------------------------------------------------

;;Returns the deciles of risk to life and property
(defmodel risk-to-life 'coastalProtection:CycloneDependentLivesAtRisk
	(ranking 'policytarget:LivesAtRiskStorm))

(defmodel risk-to-assets 'coastalProtection:CycloneSensitiveEconomicValue
	(ranking 'policytarget:AssetsAtRiskStorm))

;; --------------------------------------------------------------------------------------
;; Flow models
;; --------------------------------------------------------------------------------------

;;This SPAN statement has just been copied from flood_mark, but the "keep" 
;; list has been updated to correctly reflect the coastal flood flow concepts.
(defmodel coastal-protection-flow 'floodService:AvoidedDamageToFarms100
  (span 'floodService:FloodWaterMovement
        'coastalProtection:CoastalWaveSource
        'coastalProtection:CycloneDependentLivesAtRisk
        'coastalProtection:CoastalFloodSink
        nil 
        'floodService:TempFloodData100
        :source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        :sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        :use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        :trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :finite
        :sink-type          :finite
        :use-type           :infinite
        :benefit-type       :rival
        :downscaling-factor 8
        :rv-max-states      10
        :keep ('coastal:CoastalWaveSource 'coastal:PotentialWaveMitigation 'coastal:PotentiallyWaveVulnerablePopulations
              'coastal:PotentiallyDamagingWaveFlow 'coastal:PotentiallyDamagingWaveSource 'coastal:PotentialFloodDamageReceived
              'coastal:ActualWaveFlow 'coastal:FloodDamagingWaveSource 'coastal:UtilizedWaveMitigation
              'coastal:FloodDamageReceived 'coastal:BenignWaveSource 'coastal:UnutilizedWaveMitigation
              'coastal:AbsorbedWaveFlow 'coastal:MitigatedWaveSource 'coastal:FloodMitigationBenefitsAccrued) 
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_farmers100.clj")
        :context (coastal-wave-source risk-to-life coastal-flood-sink)))

(defmodel coastal-protection-data 'coastalProtection:CoastalStormProtection
	(identification 'coastalProtection:CoastalStormProtection 
		:context (coastal-flood-sink risk-to-life risk-to-assets coastal-wave-source)))

	 	 	