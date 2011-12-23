;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; UNEP marine project
;;; Models for coastal protection in Madagascar
;;;
;;; Valid Contexts: core.contexts.beta/mg_coastal*
;;;
;;;-------------------------------------------------------------------

(ns marine.models.coastal
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement
                           classification categorization ranking
                           numeric-coding binary-coding identification
                           bayesian probabilistic-measurement
                           namespace-ontology count get-data])
  (:refer aries :only [span]))

(namespace-ontology coastalProtection)

;;;-------------------------------------------------------------------
;;; Source models
;;;-------------------------------------------------------------------

;;(defmodel storm-probability TropicalStormProbability
;;  "Returns the likelihood of tropical storm occurrence from 1 to 10."
;;  (ranking habitat:TropicalStormProbability))

(defmodel storm-tracks StormTracks
  "Returns the names of historic storm tracks present in the context."
  (categorization StormTracks))

(defmodel buffer BufferMg100km
  "Indicates whether a location is within a 100km buffer around Madagascar."
  (binary-coding BufferMg100km))

(defmodel bathymetry-class BathymetryClass
  "Reclasses bathymetry into 5 discrete categories."
  (classification (measurement geophysics:Bathymetry "m")
    [0 :>]     Overland
    [-20 0]    VeryShallow
    [-50 -20]  Shallow
    [-200 -50] Deep
    [:< -200]  VeryDeep))

;; The ranking should be a measurement in mb, but this is not yet a
;; valid unit in Thinklab.
(defmodel atmospheric-pressure AtmosphericPressureClass
  "Reclasses storm atmospheric pressure into 3 discrete categories."
  (classification (ranking geophysics:AtmosphericPressure)
    [990 :>]  ModeratelyLowAtmosphericPressure
    [970 990] LowAtmosphericPressure
    [:< 970]  VeryLowAtmosphericPressure))

;; Change discretization in other parts of the world based on
;; regionally appropriate tropical storm scale.
(defmodel wind-speed WindSpeedClass
  "Reclasses storm wind speed into 6 discrete classes based on the
   Southwest Indian Ocean Tropical Cyclone Scale."
  (classification (measurement geophysics:WindSpeed "km/h")
    [165 :>]  VeryHighWindSpeed
    [117 165] HighWindSpeed
    [88 117]  ModeratelyHighWindSpeed
    [62 88]   ModeratelyLowWindSpeed
    [50 62]   LowWindSpeed
    [:< 50]   VeryLowWindSpeed))

(defmodel storm-surge StormSurgeClass
  "Maps StormSurgeClass subconcepts to numeric value ranges in meters."
  (probabilistic-measurement StormSurgeClass "m"
    [5 6] VeryHighStormSurge
    [4 5] HighStormSurge
    [3 4] ModerateStormSurge
    [2 3] LowStormSurge
    [0 2] VeryLowStormSurge
    [0 0] NoStormSurge))

;; Litanne
(defmodel source-100km-litanne-selector LitannePresence
  "Crops the Tropical Storm Litanne track to within a 100km buffer
   around Madagascar."
  (classification LitannePresence
    :context [storm-tracks buffer]
    :state   #(if (and (= (:storm-tracks %) "litanne")
                       (= (:buffer-mg100km %) 1))
                (tl/conc 'coastalProtection:LitannePresent))))

(defmodel coastal-wave-source-litanne CoastalWaveSource
  "Computes the discrete marginal distribution of Tropical Storm
   Litanne's StormSurgeClass from a Bayesian network, conditioned on
   the variables in the context list."
  (bayesian CoastalWaveSource
    :import   "aries.marine::CoastalFloodSourceMg.xdsl"
    :context  [wind-speed atmospheric-pressure bathymetry-class source-100km-litanne-selector]
    :required [LitannePresence]
    :keep     [StormSurgeClass]
    :result   storm-surge))

;; Daisy
(defmodel source-100km-daisy-selector DaisyPresence
  "Crops the Tropical Storm Daisy track to within a 100km buffer
   around Madagascar."
  (classification DaisyPresence
    :context [storm-tracks buffer]
    :state   #(if (and (= (:storm-tracks %) "daisy")
                       (= (:buffer-mg100km %) 1))
                (tl/conc 'coastalProtection:DaisyPresent))))

(defmodel coastal-wave-source-daisy CoastalWaveSource
  "Computes the discrete marginal distribution of Tropical Storm
   Daisy's StormSurgeClass from a Bayesian network, conditioned on the
   variables in the context list."
  (bayesian CoastalWaveSource
    :import   "aries.marine::CoastalFloodSourceMg.xdsl"
    :context  [wind-speed atmospheric-pressure bathymetry-class source-100km-daisy-selector]
    :required [DaisyPresence]
    :keep     [StormSurgeClass]
    :result   storm-surge))

;; Geralda
(defmodel source-100km-geralda-selector GeraldaPresence
  "Crops the Tropical Storm Geralda track to within a 100km buffer
   around Madagascar."
  (classification GeraldaPresence
    :context [storm-tracks buffer]
    :state   #(if (and (= (:storm-tracks %) "geralda")
                       (= (:buffer-mg100km %) 1))
                (tl/conc 'coastalProtection:GeraldaPresent))))

(defmodel coastal-wave-source-geralda CoastalWaveSource
  "Computes the discrete marginal distribution of Tropical Storm
   Geralda's StormSurgeClass from a Bayesian network, conditioned on
   the variables in the context list."
  (bayesian CoastalWaveSource
    :import   "aries.marine::CoastalFloodSourceMg.xdsl"
    :context  [wind-speed atmospheric-pressure bathymetry-class source-100km-geralda-selector]
    :required [GeraldaPresence]
    :keep     [StormSurgeClass]
    :result   storm-surge))

;;;-------------------------------------------------------------------
;;; Sink models
;;;-------------------------------------------------------------------

(defmodel mangrove MangrovePresenceClass
  "Indicates presence or absence of mangroves from the Madagascar LULC data."
  (classification (numeric-coding mglulc:MGLULCNumeric)
    5          MangrovePresent
    :otherwise MangroveAbsent))

;; Most polygons do not report any data on bleaching, these are placed
;; under "moderate bleaching".  There's a misspelling for "HIgh" in
;; the data that's accounted for in the discretization below.
(defmodel coral-quality CoralQuality
  "Reclasses coral bleaching into 4 discrete categories."
  (classification (categorization CoralBleaching)
    #{"None" "Low"}  MinimallyBleachedCoralPresent
    #{"Moderate" ""} ModeratelyBleachedCoralPresent
    #{"HIgh" "High"} HighlyBleachedCoralPresent
    :otherwise       NoCoralPresent))

;; TODO Only two classes represented from presence/absence; no idea
;; how to model density based on existing data.
(defmodel seagrass SeagrassPresenceClass
  "Indicates presence or absence of seagrass."
  (classification (binary-coding SeagrassPresence)
    1 SeagrassPresent
    0 SeagrassAbsent))

(defmodel terrestrial-vegetation TerrestrialVegetationType
  "Reclasses Madagascar LULC into 5 discrete categories."
  (classification (numeric-coding mglulc:MGLULCNumeric)
    #{1 3 4 8 10 20 21 23 30 31}             Forest ; Includes tree-dominated savannas
    #{6 7}                                   Shrubland
    #{14}                                    Wetland
    #{9 11 12 13 18 22 24 25 26 28 29 32 33} Herbaceous ; Includes agriculture, grass-dominated savannas
    #{16 17 19 27}                           Other))

;; Assumes some artificial flood protection near Toamasina, the main
;; port city in Madagascar.  Development around the small ports is
;; minimal.
(defmodel artificial-coastal-protection ArtificialCoastalProtection
  "Indicates presence or absence of ports."
  (classification (ranking infrastructure:Port)
    3          ArtificialCoastalProtectionPresent
    :otherwise ArtificialCoastalProtectionAbsent))

;; The discretization below is a first cut, may need to be changed
;; based on results of the flow model.
(defmodel coastal-flood-protection TotalCoastalFloodProtection
  "Maps TotalCoastalFloodProtection subconcepts to numeric value
   ranges in meters."
  (probabilistic-measurement TotalCoastalFloodProtection "m"
    [0.07 0.1]  HighCoastalFloodProtection
    [0.03 0.07] ModerateCoastalFloodProtection
    [0 0.03]    LowCoastalFloodProtection
    [0 0]       NoCoastalFloodProtection))

(defmodel protection-selector ProtectionPresence
  "Selects overland and shallow areas from bathymetry data to clip the
   coastal flood sink Bayesian network."
  (classification (measurement geophysics:Bathymetry "m")
    [-50 :>] ProtectionPresent))

;; Wave mitigation by ecosystems, i.e., the ecosystem service.
(defmodel coastal-flood-sink CoastalFloodSink
  "Computes the discrete marginal distribution of
   TotalCoastalFloodProtection from a Bayesian network, conditioned on
   the variables in the context list."
  (bayesian CoastalFloodSink
    :import   "aries.marine::CoastalFloodSinkMg.xdsl"
    :context  [mangrove coral-quality seagrass terrestrial-vegetation
               protection-selector artificial-coastal-protection]
    :required [ProtectionPresence]
    :keep     [TotalCoastalFloodProtection]
    :result   coastal-flood-protection))

;;;-------------------------------------------------------------------
;;; Use models
;;;-------------------------------------------------------------------

(defmodel risk-to-life CycloneDependentLivesAtRisk
  "Returns the risk to life from cyclones in deciles."
  (ranking policytarget:LivesAtRiskStorm))

(defmodel risk-to-assets CycloneSensitiveEconomicValue
  "Returns the risk to property from cyclones in deciles."
  (ranking policytarget:AssetsAtRiskStorm))

;;;-------------------------------------------------------------------
;;; Flow data models
;;;-------------------------------------------------------------------

(defmodel dune DunePresenceClass
  "Indicates presence or absence of dunes."
  (classification (binary-coding geofeatures:Dune)
    #{"dune"}  DunePresent
    :otherwise DuneAbsent))

(defmodel slope BathymetricSlope
  "Reclasses bathymetric slope into 4 discrete categories."
  (classification (measurement geophysics:BathymetricSlope "\u00b0")
    [16.70 :>]   HighSlope
    [4.57 16.70] ModerateSlope
    [1.15 4.57]  LowSlope
    [:< 1.15]    VeryLowSlope))

(defmodel depth-elevation OceanDepthAndLandElevation
  "Reclasses bathymetry into 7 discrete categories."
  (classification (measurement geophysics:Bathymetry "m")
    [20 :>]      HighLandElevation
    [5 20]       ModerateLandElevation
    [0 5]        LowLandElevation
    [-60 0]      Pelagic
    [-200 -60]   Shelf
    [-2000 -200] Slope
    [:< -2000]   DeepWater))

;; The discretization below is a first cut, may need to be changed based on results of the flow model.
;; These numbers need to be adjusted: the waves move too far inland.
(defmodel geomorphic-flood-protection GeomorphicFloodProtection
  "Maps GeomorphicFloodProtection subconcepts to numeric value ranges
   in meters."
  (probabilistic-measurement GeomorphicFloodProtection "m"
    [0.001 0.002]  HighGeomorphicProtection
    [0.0005 0.001] ModerateGeomorphicProtection
    [0 0.0005]     LowGeomorphicProtection
    [0 0]          NoGeomorphicProtection))

(defmodel geomorphic-flood-sink GeomorphicWaveReduction
  "Computes the discrete marginal distribution of
   GeomorphicFloodProtection from a Bayesian network, conditioned on
   the variables in the context list. Should be interpreted as wave
   mitigation by geomorphic features (i.e., baseline wave mitigation
   in the absence of ecosystems)."
  (bayesian GeomorphicWaveReduction
    :import  "aries.marine::CoastalFloodSinkMg.xdsl"
    :context [dune depth-elevation slope]
    :keep    [GeomorphicFloodProtection]
    :result  geomorphic-flood-protection))

(defmodel storm-track-daisy StormTrack
  "Indicates the presence of Tropical Storm Daisy's track."
  (binary-coding StormTrack
    :context [storm-tracks]
    :state   #(if (= (:storm-tracks %) "daisy") 1 0)))

(defmodel storm-track-geralda StormTrack
  "Indicates the presence of Tropical Storm Geralda's track."
  (binary-coding StormTrack
    :context [storm-tracks]
    :state   #(if (= (:storm-tracks %) "geralda") 1 0)))

(defmodel storm-track-litanne StormTrack
  "Indicates the presence of Tropical Storm Litanne's track."
  (binary-coding StormTrack
    :context [storm-tracks]
    :state   #(if (= (:storm-tracks %) "litanne") 1 0)))

;;;-------------------------------------------------------------------
;;; Identification models
;;;-------------------------------------------------------------------

(defmodel coastal-protection-data CoastalStormProtection
  "Runs all the toplevel source, sink, use, and flow data models and
   collects their results."
  (identification CoastalStormProtection
    :context [coastal-wave-source-daisy
              coastal-wave-source-geralda
              coastal-wave-source-litanne
              risk-to-life
              risk-to-assets
              coastal-flood-sink
              geomorphic-flood-sink
              storm-track-daisy
              storm-track-geralda
              storm-track-litanne]))

;;;-------------------------------------------------------------------
;;; Flow models
;;;-------------------------------------------------------------------

;;; We create two models per storm, differentiated by use type (assets vs. lives)

(defmodel coastal-protection-flow-daisy-lives CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Daisy
   Use:    Lives at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneDependentLivesAtRisk
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-daisy-lives-data.clj")
        :context            [coastal-wave-source-daisy risk-to-life coastal-flood-sink storm-track-daisy geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel coastal-protection-flow-geralda-lives CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Geralda
   Use:    Lives at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneDependentLivesAtRisk
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-geralda-lives-data.clj")
        :context            [coastal-wave-source-geralda risk-to-life coastal-flood-sink storm-track-geralda geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel coastal-protection-flow-litanne-lives CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Litanne
   Use:    Lives at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneDependentLivesAtRisk
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-litanne-lives-data.clj")
        :context            [coastal-wave-source-litanne risk-to-life coastal-flood-sink storm-track-litanne geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel coastal-protection-flow-daisy-assets CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Daisy
   Use:    Property at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneSensitiveEconomicValue
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-daisy-assets-data.clj")
        :context            [coastal-wave-source-daisy risk-to-assets coastal-flood-sink storm-track-daisy geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel coastal-protection-flow-geralda-assets CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Geralda
   Use:    Property at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneSensitiveEconomicValue
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-geralda-assets-data.clj")
        :context            [coastal-wave-source-geralda risk-to-assets coastal-flood-sink storm-track-geralda geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))

(defmodel coastal-protection-flow-litanne-assets CoastalStormProtection
  "Computes SPAN flow results for coastal storm protection.
   Source: Tropical Storm Litanne
   Use:    Property at risk from cyclones"
  (span CoastalStormMovement
        CoastalWaveSource
        CycloneSensitiveEconomicValue
        CoastalFloodSink
        nil
        (StormTrack GeomorphicWaveReduction)
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
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/coastal-protection-flow-litanne-assets-data.clj")
        :context            [coastal-wave-source-litanne risk-to-assets coastal-flood-sink storm-track-litanne geomorphic-flood-sink]
        :keep               [TheoreticalSource
                             TheoreticalSink
                             TheoreticalUse
                             PossibleFlow
                             PossibleSource
                             PossibleUse
                             ActualFlow
                             ActualSource
                             ActualSink
                             ActualUse
                             InaccessibleSource
                             InaccessibleSink
                             BlockedFlow
                             BlockedSource
                             BlockedUse]))
