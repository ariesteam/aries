;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; --------------------------------------------------------------------------------------------------

(ns marine.models.coastal
  (:refer-clojure :rename {count length})
  (:refer modelling :only (defscenario defmodel measurement classification categorization
                           ranking numeric-coding binary-coding identification bayesian
                           namespace-ontology count get-data))
  (:refer aries :only (span)))

(namespace-ontology coastalProtection)

;; --------------------------------------------------------------------------------------
;; Source models
;; --------------------------------------------------------------------------------------

;;(defmodel storm-probability TropicalStormProbability
;;  (ranking habitat:TropicalStormProbability))

(defmodel storm-tracks StormTracks
  (categorization StormTracks)) 

(defmodel buffer BufferMg100km
  (binary-coding BufferMg100km)) 

(defmodel bathymetry-class BathymetryClass
  (classification (measurement geophysics:Bathymetry "m")
    [0 :>]              Overland
    [-20 0]             VeryShallow
    [-50 -20]           Shallow
    [-200 -50]          Deep
    [:< -200]           VeryDeep))

(defmodel atmospheric-pressure AtmosphericPressureClass
  (classification (ranking geophysics:AtmosphericPressure) ;;This should be a measurement, in mb, but this is not yet a valid unit in Thinklab.
    [990 :>]  ModeratelyLowAtmosphericPressure
    [970 990] LowAtmosphericPressure
    [:< 970]  VeryLowAtmosphericPressure))

;;Discretization based on the Southwest Indian Ocean Tropical Cyclone Scale. May need a different
;; scale for other parts of the world.
(defmodel wind-speed WindSpeedClass
  (classification (measurement geophysics:WindSpeed "km/h") 
     [165 :>]    VeryHighWindSpeed
     [117 165]   HighWindSpeed
     [88 117]    ModeratelyHighWindSpeed
     [62 88]     ModeratelyLowWindSpeed
     [50 62]     LowWindSpeed
     [:< 50]     VeryLowWindSpeed))

;;Undiscretization & BN statements
(defmodel storm-surge StormSurgeClass
  (classification StormSurgeClass
                  :units      "m"
                  [5 6]      VeryHighStormSurge
                  [4 5]      HighStormSurge
                  [3 4]      ModerateStormSurge
                  [2 3]      LowStormSurge
                  [0 2]      VeryLowStormSurge
                  [0 0]      NoStormSurge))
;;                  [14 17]      VeryHighStormSurge
;;                  [11 14]      HighStormSurge
;;                  [8 11]       ModerateStormSurge
;;                  [5 8]        LowStormSurge
;;                  [2 5]        VeryLowStormSurge
;;                  [0 2]        NoStormSurge))

;;(defmodel coastal-wave-source CoastalWaveSource
;;    "Interface to Flood public asset use bayesian network"
;;    (bayesian CoastalWaveSource
;;      :import   "aries.marine::CoastalFloodSource.xdsl"
;;      :keep     (StormSurgeClass)
;;      :observed (storm-surge)
;;      :context  (wind-speed atmospheric-pressure bathymetry-class)))
;;
;;This takes the BN values ONLY where they intersect the 100 km buffer along the storm track for a given storm (Daisy in the first case)
;;(defmodel source-100km-daisy CoastalWaveSourceDaisy
;;  (measurement CoastalWaveSourceDaisy "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(if (and (= (:storm-tracks %) "daisy")
;;                      (= (:buffer %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0)))
;;
;;(defmodel source-100km-geralda CoastalWaveSourceGeralda
;;  (measurement CoastalWaveSourceGeralda "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(if (and (= (:storm-tracks %) "geralda")
;;                      (= (:buffer %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0)))
;;
;;(defmodel source-100km-litanne CoastalWaveSourceLitanne
;;  (measurement CoastalWaveSourceLitanne "m"
;;    :context (storm-tracks        :as storm-tracks
;;              buffer              :as buffer
;;              coastal-wave-source :as coastal-wave-source)
;;    :state  #(do (println %) (if (and (= (:stormtracks %) "litanne")
;;                      (= (:buffermg100km %) 1))
;;                 (get-data (:coastalwavesource %)) ;; this should give us the mean value
;;                 0.0))))

;; alternative logics for Litanne

(defmodel source-100km-litanne-selector LitannePresence
  (classification LitannePresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "litanne")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:LitannePresent))))

(defmodel coastal-wave-source-litanne CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     (StormSurgeClass)
      :required (LitannePresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-litanne-selector)))

;; Daisy

(defmodel source-100km-daisy-selector DaisyPresence
  (classification DaisyPresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "daisy")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:DaisyPresent))))

(defmodel coastal-wave-source-daisy CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     (StormSurgeClass)
      :required (DaisyPresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-daisy-selector)))

;; Geralda

(defmodel source-100km-geralda-selector GeraldaPresence
  (classification GeraldaPresence
    :context (storm-tracks buffer)
    :state  #(if (and (= (:storm-tracks %) "geralda")
                      (= (:buffer-mg100km %) 1))
                 (tl/conc 'coastalProtection:GeraldaPresent))))

(defmodel coastal-wave-source-geralda CoastalWaveSource
    "Interface to Flood public asset use bayesian network"
    (bayesian CoastalWaveSource
      :import   "aries.marine::CoastalFloodSource.xdsl"
      :keep     (StormSurgeClass)
      :required (GeraldaPresence)
      :observed (storm-surge)
      :context  (wind-speed atmospheric-pressure bathymetry-class source-100km-geralda-selector)))

;; end of alternative strategies

;; --------------------------------------------------------------------------------------
;; Sink (coastal protection) model
;; --------------------------------------------------------------------------------------

(defmodel mangrove MangrovePresenceClass
  (classification (numeric-coding mglulc:MGLULCNumeric)
    5          MangrovePresent
    :otherwise MangroveAbsent))

;; Most polygons do not report any data on bleaching, these are placed under "moderate bleaching".  There's a misspelling for "HIgh" in the data that's accounted for in the discretization below.
(defmodel coral-quality CoralQuality
  (classification (categorization CoralBleaching)
         #{"None" "Low"}      MinimallyBleachedCoralPresent
         #{"HIgh" "High"}     HighlyBleachedCoralPresent
         #{"Moderate" ""}     ModeratelyBleachedCoralPresent
         :otherwise           NoCoralPresent))
 
;; TODO only two classes represented from presence/absence; no idea how to 
;; model density based on existing data.
(defmodel seagrass SeagrassPresenceClass
	(classification (binary-coding SeagrassPresence)
		0 SeagrassAbsent
		1 SeagrassPresent))

;;Terrestrial vegetation types from Mg LULC layer
(defmodel terrestrial-vegetation TerrestrialVegetationType
  (classification (numeric-coding mglulc:MGLULCNumeric)
         #{1 3 4 8 10 20 21 23 30 31}             Forested ;;Includes tree-dominated savannas
         #{6 7}                                   Shrubland
         #{14}                                    Wetland
         #{9 11 12 13 18 22 24 25 26 28 29 32 33} Herbaceous ;;Includes agriculture, grass-dominated savannas
         #{16 17 19 27}                           Other)) 

;;Assumes some artificial flood protection near Toamasina, the main port city in Madagascar.  Development around the small ports is minimal.
(defmodel artificial-coastal-protection ArtificialCoastalProtection
  (classification (ranking infrastructure:Port)
    3          ArtificialCoastalProtectionPresent
    :otherwise ArtificialCoastalProtectionAbsent))

;;The discretization below is a first cut, may need to be changed based on results of the flow model.
;; These numbers need to be adjusted: the waves move too far inland.
(defmodel coastal-flood-protection TotalCoastalFloodProtection
  (classification TotalCoastalFloodProtection
                  :units      "m"
;;                  [1 :>]        HighCoastalFloodProtection
;;                  [1 2]        HighCoastalFloodProtection
;;                  [0.5 1]       ModerateCoastalFloodProtection
;;                  [0.1 0.5]     LowCoastalFloodProtection
;;                  [0 0.1]       NoCoastalFloodProtection))
                  [0.07 0.1]      HighCoastalFloodProtection
                  [0.03 0.07]     ModerateCoastalFloodProtection
                  [0 0.03]        LowCoastalFloodProtection
                  [0 0]           NoCoastalFloodProtection))

;; selects overland and shallow areas to clip coastal protection bn
(defmodel protection-selector ProtectionPresence
  (classification (measurement geophysics:Bathymetry "m")
    [-50 :>] ProtectionPresent))

;; Wave mitigation by ecosystems, i.e., the ecosystem service.
(defmodel coastal-flood-sink CoastalFloodSink
  	"Interface to Flood public asset use bayesian network"
	  (bayesian CoastalFloodSink 
	  	:import   "aries.marine::CoastalFloodSink.xdsl"
	  	:keep     (TotalCoastalFloodProtection)
        :observed (coastal-flood-protection)
        :required (ProtectionPresence)
        :context  (mangrove coral-quality seagrass terrestrial-vegetation protection-selector artificial-coastal-protection)))

;; --------------------------------------------------------------------------------------
;; Use models
;; --------------------------------------------------------------------------------------

;;Returns the deciles of risk to life and property
(defmodel risk-to-life CycloneDependentLivesAtRisk
	(ranking policytarget:LivesAtRiskStorm))

(defmodel risk-to-assets CycloneSensitiveEconomicValue
	(ranking policytarget:AssetsAtRiskStorm))

;; --------------------------------------------------------------------------------------
;; Flow models
;; --------------------------------------------------------------------------------------

(defmodel dune DunePresenceClass
  (classification (binary-coding geofeatures:Dune)
    #{"dune"}   DunePresent    
    :otherwise  DuneAbsent))

(defmodel slope BathymetricSlope
  (classification (measurement geophysics:BathymetricSlope "\u00b0")
    [16.70 :>]      HighSlope
    [4.57 16.70]    ModerateSlope
    [1.15 4.57]     LowSlope
    [:< 1.15]       VeryLowSlope))

(defmodel depth-elevation OceanDepthAndLandElevation
  (classification (measurement geophysics:Bathymetry "m")
    [20 :>]             HighLandElevation
    [5 20]              ModerateLandElevation
    [0 5]               LowLandElevation
    [-60 0]             Pelagic
    [-200 -60]          Shelf
    [-2000 -200]        Slope
    [:< -2000]          DeepWater))

;;The discretization below is a first cut, may need to be changed based on results of the flow model.
;; These numbers need to be adjusted: the waves move too far inland.
(defmodel geomorphic-flood-protection GeomorphicFloodProtection
  (classification GeomorphicFloodProtection
                  :units      "m"
;;                  [1 :>]        HighGeomorphicProtection
;;                  [1   2]       HighGeomorphicProtection
;;                  [0.5 1]       ModerateGeomorphicProtection
;;                  [0 0.5]       LowGeomorphicProtection))
                  [0.07 0.1]    HighGeomorphicProtection
                  [0.03 0.07]   ModerateGeomorphicProtection
                  [0 0.03]      LowGeomorphicProtection
                  [0 0]         NoGeomorphicProtection))

;; Wave mitigation by geomorphic features (i.e., baseline wave mitigation in the absence of ecosystems)
(defmodel geomorphic-flood-sink GeomorphicWaveReduction
    "Interface to Flood public asset use bayesian network"
    (bayesian GeomorphicWaveReduction 
      :import   "aries.marine::CoastalFloodSink.xdsl"
      :keep     (GeomorphicFloodProtection)
      :observed (geomorphic-flood-protection)
      :context  (dune depth-elevation slope)))

(defmodel storm-track-daisy StormTrack
  (binary-coding StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "daisy") 1 0)))

(defmodel storm-track-geralda StormTrack
  (binary-coding StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "geralda") 1 0)))

(defmodel storm-track-litanne StormTrack
  (binary-coding StormTrack
    :context (storm-tracks)
    :state  #(if (= (:storm-tracks %) "litanne") 1 0)))

(defmodel storm-track-all StormTrack
  (binary-coding StormTrack))

(defmodel coastal-protection-data CoastalStormProtection
	(identification CoastalStormProtection
                    :context (coastal-wave-source-litanne ;; TODO should sum up the storms
                              risk-to-life
                              risk-to-assets
                              coastal-flood-sink
                              storm-track-all
                              geomorphic-flood-sink)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-daisy-lives CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneDependentLivesAtRisk
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
;;        :animation?         true
        ;;:save-file          "coastal-protection-flow-daisy-lives-data.clj"
        :context            (coastal-wave-source-daisy risk-to-life coastal-flood-sink storm-track-daisy geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-geralda-lives CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneDependentLivesAtRisk
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
;;        :animation?         true
        ;;:save-file          "coastal-protection-flow-geralda-lives-data.clj"
        :context            (coastal-wave-source-geralda risk-to-life coastal-flood-sink storm-track-geralda geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-litanne-lives CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneDependentLivesAtRisk
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
 ;;       :animation?         true
        ;;:save-file          "coastal-protection-flow-litanne-lives-data.clj"
        :context            (coastal-wave-source-litanne risk-to-life coastal-flood-sink storm-track-litanne geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-daisy-assets CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneSensitiveEconomicValue
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
 ;;       :animation?         true
        ;;:save-file          "coastal-protection-flow-daisy-assets-data.clj"
        :context            (coastal-wave-source-daisy risk-to-assets coastal-flood-sink storm-track-daisy geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-geralda-assets CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneSensitiveEconomicValue
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
;;        :animation?         true
        ;;:save-file          "coastal-protection-flow-geralda-assets-data.clj"
        :context            (coastal-wave-source-geralda risk-to-assets coastal-flood-sink storm-track-geralda geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))

;;Could have as many as 6 SPAN statements: one each for risk-to-life & risk-to-assets, 1 each for 3 storm events.
(defmodel coastal-protection-flow-litanne-assets CoastalStormProtection
  (span CoastalStormMovement
        StormSurgeClass             ;; CoastalWaveSource
        CycloneSensitiveEconomicValue
        TotalCoastalFloodProtection ;;CoastalFloodSink
        nil 
        (StormTrack GeomorphicFloodProtection)
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
;;        :animation?         true
        ;;:save-file          "coastal-protection-flow-litanne-assets-data.clj"
        :context            (coastal-wave-source-litanne risk-to-assets coastal-flood-sink storm-track-litanne geomorphic-flood-sink)
        :keep               (CoastalWaveSource
                             PotentialWaveMitigation
                             PotentiallyWaveVulnerablePopulations
                             PotentiallyDamagingWaveFlow
                             PotentiallyDamagingWaveSource
                             PotentialWaveDamageReceived
                             ActualWaveFlow
                             FloodDamagingWaveSource
                             UtilizedWaveMitigation
                             WaveDamageReceived
                             BenignWaveSource
                             UnutilizedWaveMitigation
                             AbsorbedWaveFlow
                             MitigatedWaveSource
                             WaveMitigationBenefitsAccrued)))
