;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; --------------------------------------------------------------------------------------------------

(ns marine.models.coastal
	(:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))
  
;; --------------------------------------------------------------------------------------
;; sink (coastal protection) model
;; --------------------------------------------------------------------------------------

;; Converted to m so should work now but need to test
(defmodel mangrove-width 'coastalProtection:MangroveWidth
	(classification (measurement 'coastalProtection:MangroveWidth "m")
		[2000 :>]       'coastalProtection:HighMangroveWidth
		[400 2000]      'coastalProtection:ModerateMangroveWidth
		[:< 400]        'coastalProtection:LowMangroveWidth
		nil             'coastalProtection:NoMangroveWidth))

;; TODO almost all coral polygons in existing data do not report bleaching; I 
;; don't know what text should be in the categories to define other states, so
;; only HighBleaching is reported here if the field isn't empty.
(defmodel bleaching 'coastalProtection:CoralBleaching
	(classification (categorization 'coastalProtection:CoralBleaching)
 		#{nil "None"}			  'coastalProtection:NoBleaching
 		#{"HIgh" "High"}	  'coastalProtection:HighBleaching
 	  #{"Low" "Moderate"} 'coastalProtection:ModerateBleaching))
	
;; TODO only two classes represented from presence/absence; no idea how to 
;; model density based on existing data.
(defmodel seagrass 'coastalProtection:SeagrassDensity
	(classification (binary-coding 'coastalProtection:SeagrassDensity)
		0 'coastalProtection:NoSeagrassDensity
		1 'coastalProtection:HighSeagrassDensity))
		
;; sea floor slope 
;; Ken: units uncertain, layer is horribly large and cumbersome to work with
;; TODO check - BN has only 4 classes, so I put the last 2 together and eliminated
;; VeryHighSlope with breakpoint at 9,000,000
(defmodel slope 'coastalProtection:BathymetricSlope
	(classification (ranking 'geophysics:BathymetricSlope)
		[4000000 :>]      'coastalProtection:HighSlope
		[2000000 4000000] 'coastalProtection:ModerateSlope
		[200000 2000000]  'coastalProtection:LowSlope
		[:< 200000]      'coastalProtection:VeryLowSlope))
			
;; flood protection
(defmodel coastal-flood-sink 'coastalProtection:CoastalFloodSink
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'coastalProtection:CoastalFloodSink 
	  	:import   "aries.marine::CoastalFloodSink.xdsl"
	  	:keep     ('coastalProtection:TotalCoastalFloodProtection)
	 	 	:context  (bleaching seagrass slope mangrove-width)))

;; --------------------------------------------------------------------------------------
;; use models
;; --------------------------------------------------------------------------------------

;;Rather than classifying it and losing information, just return the deciles of risk to life and property.
(defmodel risk-to-life 'coastalProtection:CycloneDependentLivesAtRisk
	(ranking 'policytarget:LivesAtRiskStorm))

(defmodel risk-to-assets 'coastalProtection:CycloneSensitiveEconomicValue
	(ranking 'policytarget:AssetsAtRiskStorm))
		
;; --------------------------------------------------------------------------------------
;; source models
;; --------------------------------------------------------------------------------------

(defmodel storm-probability 'coastalProtection:TropicalStormProbability
	(ranking 'habitat:TropicalStormProbability))

;; --------------------------------------------------------------------------------------
;; all together now
;; --------------------------------------------------------------------------------------

;;This SPAN statement has just been copied from flood_mark, but the "keep" 
;; list has been updated to correctly reflect the coastal flood flow concepts.
(defmodel coastal-protection-flow 'floodService:AvoidedDamageToFarms100
  (span 'floodService:FloodWaterMovement
        'floodService:FloodSourceValue
        'floodService:FloodFarmersUse100
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
        :benefit-type       :rival
        :downscaling-factor 8
        :rv-max-states      10
        :keep ('coastal:CoastalWaveSource 'coastal:PotentialWaveMitigation 'coastal:PotentiallyWaveVulnerablePopulations
              'coastal:PotentiallyDamagingWaveFlow 'coastal:PotentiallyDamagingWaveSource 'coastal:PotentialFloodDamageReceived
              'coastal:ActualWaveFlow 'coastal:FloodDamagingWaveSource 'coastal:UtilizedWaveMitigation
              'coastal:FloodDamageReceived 'coastal:BenignWaveSource 'coastal:UnutilizedWaveMitigation
              'coastal:AbsorbedWaveFlow 'coastal:MitigatedWaveSource 'coastal:FloodMitigationBenefitsAccrued) 
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_farmers100.clj")
        :context (storm-probability risk-to-life coastal-flood-sink)))

(defmodel coastal-protection-data 'coastalProtection:CoastalStormProtection
	(identification 'coastalProtection:CoastalStormProtection 
		:context (coastal-flood-sink risk-to-life risk-to-assets storm-probability)))

	 	 	