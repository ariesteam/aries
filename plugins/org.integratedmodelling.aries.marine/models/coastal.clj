;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; --------------------------------------------------------------------------------------------------

(ns marine.models.coastal
  (:refer-clojure :rename {count length})
  (:refer modelling :only (defscenario defmodel measurement classification categorization
                           ranking numeric-coding binary-coding identification bayesian count get-data))
  (:refer aries :only (span)))

;; --------------------------------------------------------------------------------------
;; Source models
;; --------------------------------------------------------------------------------------

;;(defmodel storm-probability 'coastalProtection:TropicalStormProbability
;;  (ranking 'habitat:TropicalStormProbability))

(defmodel storm-tracks 'coastalProtection:StormTracks
  (categorization 'coastalProtection:StormTracks)) 

(defmodel buffer 'coastalProtection:BufferMg100km
  (binary-coding 'coastalProtection:BufferMg100km)) 

(defmodel bathymetry-class 'coastalProtection:BathymetryClass
  (classification (measurement 'geophysics:Bathymetry "m")
    [0 :>]              'coastalProtection:Overland
    [-20 0]             'coastalProtection:VeryShallow
    [-50 -20]           'coastalProtection:Shallow
    [-200 -50]          'coastalProtection:Deep
    [:< -200]           'coastalProtection:VeryDeep))

(defmodel atmospheric-pressure 'coastalProtection:AtmosphericPressureClass
  (classification (ranking 'geophysics:AtmosphericPressure) ;;This should be a measurement, in mb, but this is not yet a valid unit in Thinklab.
    [990 :>]  'coastalProtection:ModeratelyLowAtmosphericPressure
    [970 990] 'coastalProtection:LowAtmosphericPressure
    [:< 970]  'coastalProtection:VeryLowAtmosphericPressure))

;;Discretization based on the Southwest Indian Ocean Tropical Cyclone Scale. May need a different
;; scale for other parts of the world.
(defmodel wind-speed 'coastalProtection:WindSpeedClass
  (classification (measurement 'geophysics:WindSpeed "km/h") 
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
                  [5 6]      'coastalProtection:VeryHighStormSurge
                  [4 5]      'coastalProtection:HighStormSurge
                  [3 4]      'coastalProtection:ModerateStormSurge
                  [2 3]      'coastalProtection:LowStormSurge
                  [0 2]      'coastalProtection:VeryLowStormSurge
                  [0 0]      'coastalProtection:NoStormSurge))
;;                  [14 17]      'coastalProtection:VeryHighStormSurge
;;                  [11 14]      'coastalProtection:HighStormSurge
;;                  [8 11]       'coastalProtection:ModerateStormSurge
;;                  [5 8]        'coastalProtection:LowStormSurge
;;                  [2 5]        'coastalProtection:VeryLowStormSurge
;;                  [0 2]        'coastalProtection:NoStormSurge))

;;(defmodel coastal-wave-source 'coastalProtection:CoastalWaveSource
;;    "Interface to Flood public asset use bayesian network"
;;    (bayesian 'coastalProtection:CoastalWaveSource
;;      :import   "aries.marine::CoastalFloodSource.xdsl"
;;      :keep     ('coastalProtection:StormSurgeClass)
;;      :observed (storm-surge)
;;      :context  (wind-speed atmospheric-pressure bathymetry-class)))
;;
;;This takes the BN values ONLY where they intersect the 100 km buffer along the storm track for a given storm (Daisy in the first case)
;;(defmodel source-100km-daisy 'coastalProtection:CoastalWaveSourceDaisy
;;  (measurement 'coastalProtection:CoastalWaveSourceDaisy "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(if (and (= (:storm-tracks %) "daisy")
;;                      (= (:buffer %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0)))
;;
;;(defmodel source-100km-geralda 'coastalProtection:CoastalWaveSourceGeralda
;;  (measurement 'coastalProtection:CoastalWaveSourceGeralda "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(if (and (= (:storm-tracks %) "geralda")
;;                      (= (:buffer %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0)))
;;
;;(defmodel source-100km-litanne 'coastalProtection:CoastalWaveSourceLitanne
;;  (measurement 'coastalProtection:CoastalWaveSourceLitanne "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(do (println %) (if (and (= (:stormtracks %) "litanne")
;;                      (= (:buffermg100km %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0))))

;; alternative logics for Litanne

(defmodel source-100km-litanne-selector 'coastalProtection:LitannePresence
  (classification 'coastalProtection:LitannePresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "litanne")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:LitannePresent))))

(defmodel coastal-wave-source-litanne 'coastalProtection:CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian 'coastalProtection:CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     ('coastalProtection:StormSurgeClass)
      :required ('coastalProtection:LitannePresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-litanne-selector)))

;; Daisy

(defmodel source-100km-daisy-selector 'coastalProtection:DaisyPresence
  (classification 'coastalProtection:DaisyPresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "daisy")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:DaisyPresent))))

(defmodel coastal-wave-source-daisy 'coastalProtection:CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian 'coastalProtection:CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     ('coastalProtection:StormSurgeClass)
      :required ('coastalProtection:DaisyPresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-daisy-selector)))

;; Geralda

(defmodel source-100km-geralda-selector 'coastalProtection:GeraldaPresence
  (classification 'coastalProtection:GeraldaPresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "geralda")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:GeraldaPresent))))

(defmodel coastal-wave-source-geralda 'coastalProtection:CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian 'coastalProtection:CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     ('coastalProtection:StormSurgeClass)
      :required ('coastalProtection:GeraldaPresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-geralda-selector)))

;; end of alternative strategies

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
         #{"None" "Low"}      'coastalProtection:MinimallyBleachedCoralPresent
         #{"HIgh" "High"}     'coastalProtection:HighlyBleachedCoralPresent
         #{"Moderate" ""}     'coastalProtection:ModeratelyBleachedCoralPresent
         :otherwise           'coastalProtection:NoCoralPresent))
 
;; TODO only two classes represented from presence/absence; no idea how to 
;; model density based on existing data.
(defmodel seagrass 'coastalProtection:SeagrassPresenceClass
	(classification (binary-coding 'coastalProtection:SeagrassPresence)
		0 'coastalProtection:SeagrassAbsent
		1 'coastalProtection:SeagrassPresent))

;;Terrestrial vegetation types from Mg LULC layer
(defmodel terrestrial-vegetation 'coastalProtection:TerrestrialVegetationType
  (classification (numeric-coding 'mglulc:MGLULCNumeric)
         #{1 3 4 8 10 20 21 23 30 31}             'coastalProtection:Forested ;;Includes tree-dominated savannas
         #{6 7}                                   'coastalProtection:Shrubland
         #{14}                                    'coastalProtection:Wetland
         #{9 11 12 13 18 22 24 25 26 28 29 32 33} 'coastalProtection:Herbaceous ;;Includes agriculture, grass-dominated savannas
         #{16 17 19 27}                           'coastalProtection:Other)) 

;;Assumes some artificial flood protection near Toamasina, the main port city in Madagascar.  Development around the small ports is minimal.
(defmodel artificial-coastal-protection 'coastalProtection:ArtificialCoastalProtection
  (classification (ranking 'infrastructure:Port)
    3          'coastalProtection:ArtificialCoastalProtectionPresent
    :otherwise 'coastalProtection:ArtificialCoastalProtectionAbsent))

;;The discretization below is a first cut, may need to be changed based on results of the flow model.
(defmodel coastal-flood-protection 'coastalProtection:TotalCoastalFloodProtection
  (classification 'coastalProtection:TotalCoastalFloodProtection
                  :units      "m"
;;                  [1 :>]        'coastalProtection:HighCoastalFloodProtection
;;                  [1 2]        'coastalProtection:HighCoastalFloodProtection
;;                  [0.5 1]       'coastalProtection:ModerateCoastalFloodProtection
;;                  [0.1 0.5]     'coastalProtection:LowCoastalFloodProtection
;;                  [0 0.1]       'coastalProtection:NoCoastalFloodProtection))
                  [0.07 0.1]      'coastalProtection:HighCoastalFloodProtection
                  [0.03 0.07]     'coastalProtection:ModerateCoastalFloodProtection
                  [0 0.03]        'coastalProtection:LowCoastalFloodProtection
                  [0 0]           'coastalProtection:NoCoastalFloodProtection))

;; selects overland and shallow areas to clip coastal protection bn
(defmodel protection-selector 'coastalProtection:ProtectionPresence
  (classification (measurement 'geophysics:Bathymetry "m")
    [-50 :>] 'coastalProtection:ProtectionPresent))

;; Wave mitigation by ecosystems, i.e., the ecosystem service.
(defmodel coastal-flood-sink 'coastalProtection:CoastalFloodSink
  	"Interface to Flood public asset use bayesian network"
	  (bayesian 'coastalProtection:CoastalFloodSink 
	  	:import   "aries.marine::CoastalFloodSink.xdsl"
	  	:keep     ('coastalProtection:TotalCoastalFloodProtection)
        :observed (coastal-flood-protection)
        :required ('coastalProtection:ProtectionPresence)
        :context  (mangrove coral-quality seagrass terrestrial-vegetation protection-selector artificial-coastal-protection)))

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

(defmodel dune 'coastalProtection:DunePresenceClass
  (classification (binary-coding 'geofeatures:Dune)
    #{"dune"}   'coastalProtection:DunePresent    
    :otherwise  'coastalProtection:DuneAbsent))

(defmodel slope 'coastalProtection:BathymetricSlope
  (classification (measurement 'geophysics:BathymetricSlope "\u00b0")
    [16.70 :>]      'coastalProtection:HighSlope
    [4.57 16.70]    'coastalProtection:ModerateSlope
    [1.15 4.57]     'coastalProtection:LowSlope
    [:< 1.15]       'coastalProtection:VeryLowSlope))

(defmodel depth-elevation 'coastalProtection:OceanDepthAndLandElevation
  (classification (measurement 'geophysics:Bathymetry "m")
    [20 :>]             'coastalProtection:HighLandElevation
    [5 20]              'coastalProtection:ModerateLandElevation
    [0 5]               'coastalProtection:LowLandElevation
    [-60 0]             'coastalProtection:Pelagic
    [-200 -60]          'coastalProtection:Shelf
    [-2000 -200]        'coastalProtection:Slope
    [:< -2000]          'coastalProtection:DeepWater))

;;The discretization below is a first cut, may need to be changed based on results of the flow model.
(defmodel geomorphic-flood-protection 'coastalProtection:GeomorphicFloodProtection
  (classification 'coastalProtection:GeomorphicFloodProtection
                  :units      "m"
;;                  [1 :>]        'coastalProtection:HighGeomorphicProtection
;;                  [1   2]       'coastalProtection:HighGeomorphicProtection
;;                  [0.5 1]       'coastalProtection:ModerateGeomorphicProtection
;;                  [0 0.5]       'coastalProtection:LowGeomorphicProtection))
                  [0.07 0.1]    'coastalProtection:HighGeomorphicProtection
                  [0.03 0.07]   'coastalProtection:ModerateGeomorphicProtection
                  [0 0.03]      'coastalProtection:LowGeomorphicProtection
                  [0 0]         'coastalProtection:NoGeomorphicProtection))

;; Wave mitigation by geomorphic features (i.e., baseline wave mitigation in the absence of ecosystems)
(defmodel geomorphic-flood-sink 'coastalProtection:GeomorphicWaveReduction
    "Interface to Flood public asset use bayesian network"
    (bayesian 'coastalProtection:GeomorphicWaveReduction 
      :import   "aries.marine::CoastalFloodSink.xdsl"
      :keep     ('coastalProtection:GeomorphicFloodProtection)
      :observed (geomorphic-flood-protection)
      :context  (dune depth-elevation slope)))

(defmodel storm-track-daisy 'coastalProtection:StormTrack
  (binary-coding 'coastalProtection:StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "daisy") 1 0)))

(defmodel storm-track-geralda 'coastalProtection:StormTrack
  (binary-coding 'coastalProtection:StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "geralda") 1 0)))

(defmodel storm-track-litanne 'coastalProtection:StormTrack
  (binary-coding 'coastalProtection:StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "litanne") 1 0)))

(defmodel coastal-protection-data 'coastalProtection:CoastalStormProtection
	(identification 'coastalProtection:CoastalStormProtection
                    :context (coastal-wave-source-daisy
                              coastal-wave-source-geralda
                              coastal-wave-source-litanne
                              risk-to-life
                              risk-to-assets
                              coastal-flood-sink
                              storm-track-daisy
                              storm-track-geralda
                              storm-track-litanne
                              geomorphic-flood-sink)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-daisy-lives 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneDependentLivesAtRisk
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-daisy-lives-data.clj"
        :context            (coastal-wave-source-daisy risk-to-life coastal-flood-sink storm-track-daisy geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-geralda-lives 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneDependentLivesAtRisk
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-geralda-lives-data.clj"
        :context            (coastal-wave-source-geralda risk-to-life coastal-flood-sink storm-track-geralda geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-litanne-lives 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneDependentLivesAtRisk
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-litanne-lives-data.clj"
        :context            (coastal-wave-source-litanne risk-to-life coastal-flood-sink storm-track-litanne geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-daisy-assets 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneSensitiveEconomicValue
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-daisy-assets-data.clj"
        :context            (coastal-wave-source-daisy risk-to-assets coastal-flood-sink storm-track-daisy geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-geralda-assets 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneSensitiveEconomicValue
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-geralda-assets-data.clj"
        :context            (coastal-wave-source-geralda risk-to-assets coastal-flood-sink storm-track-geralda geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-litanne-assets 'coastalProtection:CoastalStormProtection
  (span 'coastalProtection:CoastalStormMovement
        'coastalProtection:StormSurgeClass             ;; CoastalWaveSource
        'coastalProtection:CycloneSensitiveEconomicValue
        'coastalProtection:TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        ('coastalProtection:StormTrack 'coastalProtection:GeomorphicFloodProtection)
        :source-threshold   0.0
        :sink-threshold     0.0
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         true
        ;;:save-file          "coastal-protection-flow-litanne-assets-data.clj"
        :context            (coastal-wave-source-litanne risk-to-assets coastal-flood-sink storm-track-litanne geomorphic-flood-sink)
        :keep               ('coastalProtection:CoastalWaveSource
                             'coastalProtection:PotentialWaveMitigation
                             'coastalProtection:PotentiallyWaveVulnerablePopulations
                             'coastalProtection:PotentiallyDamagingWaveFlow
                             'coastalProtection:PotentiallyDamagingWaveSource
                             'coastalProtection:PotentialWaveDamageReceived
                             'coastalProtection:ActualWaveFlow
                             'coastalProtection:FloodDamagingWaveSource
                             'coastalProtection:UtilizedWaveMitigation
                             'coastalProtection:WaveDamageReceived
                             'coastalProtection:BenignWaveSource
                             'coastalProtection:UnutilizedWaveMitigation
                             'coastalProtection:AbsorbedWaveFlow
                             'coastalProtection:MitigatedWaveSource
                             'coastalProtection:WaveMitigationBenefitsAccrued)))
