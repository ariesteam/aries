;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for subsistence fisheries
;; --------------------------------------------------------------------------------------------------

(ns marine.models.fisheries
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization
                           ranking namespace-ontology numeric-coding binary-coding
                           identification probabilistic-measurement bayesian count])
  (:refer aries :only [span]))

(namespace-ontology fisheries)

;; --------------------------------------------------------------------------------------
;; Source models
;; --------------------------------------------------------------------------------------

;; Statements below estimate harvest by pixel of three species of
;; marine pelagic fishes valued for human consumption. Assumptions
;; about the harvested quantity of each species are documented in the
;; ARIES modeling guide. These values can be adjusted as needed with
;; improved data or expert knowledge.
(defmodel slender-emperor-harvest LethrinusBorbonicusHarvest
  (measurement LethrinusBorbonicusHarvest "kg/km^2*year"
    :context ((measurement LethrinusBorbonicusAbundanceMg "kg/km^2*year"))
    :state   #(if-let [abundance (:lethrinus-borbonicus-abundance-mg %)]
                (* abundance 8712431))))

(defmodel sky-emperor-harvest LethrinusMahsenaHarvest
  (measurement LethrinusMahsenaHarvest "kg/km^2*year"
    :context ((measurement LethrinusMahsenaAbundanceMg "kg/km^2*year"))
    :state   #(if-let [abundance (:lethrinus-mahsena-abundance-mg %)]
                (* abundance 8712431))))

(defmodel mangrove-red-snapper-harvest LutjanusArgentimaculatusHarvest
  (measurement LutjanusArgentimaculatusHarvest "kg/km^2*year"
    :context ((measurement LutjanusArgentimaculatusAbundanceMg "kg/km^2*year"))
    :state   #(if-let [abundance (:lutjanus-argentimaculatus-abundance-mg %)]
                (* abundance 8712431))))

(defmodel total-pelagic-subsistence-harvest TotalSubsistenceHarvest
  (measurement TotalSubsistenceHarvest "kg/km^2*year"
    :context (slender-emperor-harvest sky-emperor-harvest mangrove-red-snapper-harvest)
    :state   #(reduce +
                      (remove nil?
                              [(:lethrinus-borbonicus-harvest      %)
                               (:lethrinus-mahsena-harvest         %)
                               (:lutjanus-argentimaculatus-harvest %)]))))

;; KB, 8/11/10: Statements below are to link habitat change to fish
;; change. This is not part of the 1st generation flow models, and
;; could be added to subsequent marine modeling work.
;;
;; TODO almost all coral polygons in existing data do not report
;; bleaching; I don't know what text should be in the categories to
;; define other states, so only HighBleaching is reported here if the
;; field isn't empty.
;;
;; TODO this is the same as for coastalProtection, it's a weird file -
;; we should use a more general category anyway if this is going to
;; stay the only datafile for long.
(defmodel bleaching-fisheries CoralBleaching
  (classification (categorization coastalProtection:CoralBleaching)
    #{nil "None"}       NoBleaching
    #{"HIgh" "High"}    HighBleaching
    #{"Low" "Moderate"} ModerateBleaching))
  
;; Converted to km^2 so should work now but need to test      
;;
;; FIXME: Concept conflict! You can't have a measurement and
;; classification of the same observable.
(defmodel reef-area CoralReefArea
  (classification (measurement CoralReefArea "km^2")
    [250 :>]          HighReefArea
    [25 250]          ModerateReefArea
    [:exclusive 0 25] LowReefArea
    :otherwise        NoReefArea))

;; FIXME: Concept conflict! You can't have a categorization and
;; classification of the same observable.
(defmodel estuary-area EstuaryArea
  (classification (categorization EstuaryArea)
    "Small"    SmallEstuary
    "Large"    LargeEstuary
    :otherwise NoEstuary))

(defmodel nitrogen NitrogenRunoff
  (classification (measurement policytarget:NitrogenFromFertilizerAndManure "kg/ha*year")
    [90 :>] HighNitrogenRunoff
    [40 90] ModerateNitrogenRunoff
    [15 40] LowNitrogenRunoff
    [:< 15] NoNitrogenRunoff))

(defmodel fish-habitat-quality FishHabitat
  "Interface to subsistence use bayesian network"
  (bayesian FishHabitat
    :import  "aries.marine::FisheriesA_hololepidotus.xdsl"
    :keep    (ReefQuality EstuaryQuality)
    :context (bleaching-fisheries reef-area estuary-area nitrogen)))

;; --------------------------------------------------------------------------------------
;; Use models
;; --------------------------------------------------------------------------------------

;; FV FIXME
;; The distance to coast thing urgently needs to reclassify a proper
;; distance layer or algorithm with actual numbers and not specific of
;; fisheries. The discretization should be bounded so that non-coastal
;; proximate areas return nulls, so it can be used to clip the
;; bayesian network instead of having to write a
;; "subsistence-selector" as below.
(defmodel coastal-proximity CoastalProximity
  (classification (measurement DistanceToCoast "km")
    1       HighCoastalProximity
    5       ModerateCoastalProximity
    [25 :>] LowCoastalProximity))

(defmodel poverty Poverty
  (classification (ranking policytarget:PovertyPercentage)
    [50 :>] HighPoverty
    [25 50] ModeratePoverty
    [:< 25] LowPoverty))

(defmodel population-density-class PopulationDensityClass
  (classification (count policytarget:PopulationDensity "/km^2")
    [2000 :>]   VeryHighPopulationDensity
    [1000 2000] HighPopulationDensity
    [200 1000]  ModeratePopulationDensity
    [50 200]    LowPopulationDensity
    [:< 50]     VeryLowPopulationDensity))

;; Assume high subsistence use = per capita demand of 6.8 kg fish/yr,
;; moderate use = 4.6 kg fish/yr low use = 2.3 kg fish/yr. This
;; calculates total demand.
(defmodel subsistence-fishing-undiscretized SubsistenceUse
  (probabilistic-measurement SubsistenceUse "kg/km^2*year" ; per person, multiply by population density.
    ;; FIXME: Evil hack warning! Ferd doesnt have any way in his
    ;; modelling API for CategoricalDistributionDatasource to extract
    ;; the undiscretized values of a deterministic distribution.
    ;; Therefore, we're going to be silly and just make them into
    ;; ranges centered around the values we want. My code (Gary) will
    ;; just take their midpoints anyway, so it's no big deal.
    [ 5.6 8.0] HighSubsistenceUse
    [ 3.6 5.6] ModerateSubsistenceUse
    [ 1.0 3.6] LowSubsistenceUse
    [-1.0 1.0] NoSubsistenceUse))
;;    6.8  HighSubsistenceUse
;;    4.6  ModerateSubsistenceUse
;;    2.3  LowSubsistenceUse
;;    0    NoSubsistenceUse))

;;(defmodel subsistence-fishing-undiscretized SubsistenceUse
;;  (classification SubsistenceUse
;;    :units "kg/km^2*year"
;;    :context ((count policytarget:PopulationDensity "/km^2" :as population-density-count))
;;    ;; This classification syntax is documented as working but isn't implemented!
;;    (* (:population-density-count self) 6.8) HighSubsistenceUse
;;    (* (:population-density-count self) 4.6) ModerateSubsistenceUse
;;    (* (:population-density-count self) 2.3) LowSubsistenceUse
;;    0                                        NoSubsistenceUse))

;; FIXME this should be removed when the distance to coast model is done properly
(defmodel subsistence-selector ProximityBuffer
  (classification ProximityBuffer
    :context (coastal-proximity)
    :state   #(if-let [proximity-class (:coastal-proximity %)]
                (if (or (= proximity-class (tl/conc 'fisheries:HighCoastalProximity))
                        (= proximity-class (tl/conc 'fisheries:ModerateCoastalProximity))
                        (= proximity-class (tl/conc 'fisheries:LowCoastalProximity)))
                  (tl/conc 'fisheries:ProximityBufferPresent)))))

(defmodel subsistence-fishing SubsistenceFishing
  "Interface to subsistence use bayesian network"
  (bayesian SubsistenceFishing
    :import   "aries.marine::FisheriesSubsistenceUse.xdsl"
    :required (ProximityBuffer) ;; FIXME substitute with distance to coast when it's correctly written and based on decent data
    :keep     (SubsistenceUse)
    :result   subsistence-fishing-undiscretized
    :context  (poverty population-density-class coastal-proximity subsistence-selector)))

;; --------------------------------------------------------------------------------------
;; Flow models
;; --------------------------------------------------------------------------------------

(defmodel paths infrastructure:Path
  (binary-coding infrastructure:Path))

(defmodel population-density PopulationDensity
  (count policytarget:PopulationDensity "/km^2"))

(defmodel fisheries-subsistence-data SubsistenceFishProvision
  (identification SubsistenceFishProvision
    :context (total-pelagic-subsistence-harvest
              subsistence-fishing
              paths
              population-density)))

;; flow model using temporary SPANK implementation so that both Gary and the PI can sleep.
(defmodel fisheries-ass-saver SubsistenceFishProvision
  (modelling/spank SubsistenceFishProvision
    :context (total-pelagic-subsistence-harvest subsistence-fishing paths population-density)))

(defmodel fisheries-subsistence-flow SubsistenceFishProvision
  (span SubsistenceFishAccessibility
        TotalSubsistenceHarvest
        SubsistenceFishing
        nil
        nil
        (infrastructure:Path PopulationDensity)
        :source-threshold   0.0
        :sink-threshold     nil
        :use-threshold      0.0
        :trans-threshold    0.1
        :source-type        :finite
        :sink-type          nil
        :use-type           :finite
        :benefit-type       :rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;; :save-file          (str (System/getProperty "user.home") "/subsistence-fisheries-data.clj")
        :context            (total-pelagic-subsistence-harvest subsistence-fishing paths population-density)
        :keep               (SubsistenceFishSupply
                             SubsistenceFishDemand
                             SubsistenceFishFlow
                             UtilizedSubsistenceFish
                             SatisfiedSubsistenceFishDemand
                             UnutilizedSubsistenceFish
                             UnsatisfiedSubsistenceFishDemand)))
