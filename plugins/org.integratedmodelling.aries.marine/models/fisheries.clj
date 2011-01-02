;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for subsistence fisheries
;; --------------------------------------------------------------------------------------------------

(ns marine.models.fisheries
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))

;; --------------------------------------------------------------------------------------
;; Source models
;; --------------------------------------------------------------------------------------

;;Statements below estimate harvest by pixel of three species of marine pelagic fishes
;; valued for human consumption.  Assumptions about the harvested quantity of each species are documented
;; in the ARIES modeling guide.  These values can be adjusted as needed with improved data or expert knowledge.
(defmodel slender-emperor-harvest 'fisheries:LethrinusBorbonicusHarvest
  (measurement 'fisheries:LethrinusBorbonicusHarvest "kg/km^2*year" 
      :context ((measurement 'fisheries:LethrinusBorbonicusAbundanceMg "kg/km^2*year" :as abundance))
      :state   #(* (:abundance %) 8712431)))

(defmodel sky-emperor-harvest 'fisheries:LethrinusMahsenaHarvest
  (measurement 'fisheries:LethrinusMahsenaHarvest "kg/km^2*year" 
      :context ((measurement 'fisheries:LethrinusMahsenaAbundanceMg "kg/km^2*year" :as abundance))
      :state   #(* (:abundance %) 8712431)))

(defmodel mangrove-red-snapper-harvest 'fisheries:LutjanusArgentimaculatusHarvest
  (measurement 'fisheries:LutjanusArgentimaculatusHarvest "kg/km^2*year" 
      :context ((measurement 'fisheries:LutjanusArgentimaculatusAbundanceMg "kg/km^2*year" :as abundance))
      :state   #(* (:abundance %) 8712431)))

(defmodel total-pelagic-subsistence-harvest 'fisheries:TotalSubsistenceHarvest
  (measurement 'fisheries:TotalSubsistenceHarvest "kg/km^2*year"
      :context (slender-emperor-harvest sky-emperor-harvest mangrove-red-snapper-harvest)
      :state   #(apply + (remove nil?
                                 [(:lethrinusborbonicusharvest      %)
                                  (:lethrinusmahsenaharvest         %)
                                  (:lutjanusargentimaculatusharvest %)]))))

;; KB, 8/11/10: Statements below are to link habitat change to fish change.  This is not
;;   part of the 1st generation flow models, and could be added to subsequent marine modeling work.
;; TODO almost all coral polygons in existing data do not report bleaching; I 
;; don't know what text should be in the categories to define other states, so
;; only HighBleaching is reported here if the field isn't empty.
;; TODO this is the same as for coastalProtection, it's a weird file - we should use a more
;; general category anyway if this is going to stay the only datafile for long.
(defmodel bleaching-fisheries 'fisheries:CoralBleaching
  (classification (categorization 'coastalProtection:CoralBleaching)
    #{nil "None"}       'fisheries:NoBleaching
    #{"HIgh" "High"}    'fisheries:HighBleaching
    #{"Low" "Moderate"} 'fisheries:ModerateBleaching))
  
;; Converted to km^2 so should work now but need to test      
(defmodel reef-area 'fisheries:CoralReefArea
  (classification (measurement 'fisheries:CoralReefArea "km^2")
    [250 :>]            'fisheries:HighReefArea
    [25 250]            'fisheries:ModerateReefArea
    [:exclusive 0 25]   'fisheries:LowReefArea
    nil                 'fisheries:NoReefArea))
    
(defmodel estuary-area 'fisheries:EstuaryArea
  (classification (categorization 'fisheries:EstuaryArea)
    "Small"    'fisheries:SmallEstuary
    "Large"    'fisheries:LargeEstuary
    :otherwise 'fisheries:NoEstuary))

(defmodel nitrogen 'fisheries:NitrogenRunoff
  (classification (measurement 'policytarget:NitrogenFromFertilizerAndManure "kg/ha*year")
    [90 :>] 'fisheries:HighNitrogenRunoff
    [40 90]  'fisheries:ModerateNitrogenRunoff
    [15 40]  'fisheries:LowNitrogenRunoff
    [:< 15]  'fisheries:NoNitrogenRunoff))
    
(defmodel fish-habitat-quality 'fisheries:FishHabitat
    "Interface to subsistence use bayesian network"
    (bayesian 'fisheries:FishHabitat 
      :import   "aries.marine::FisheriesA_hololepidotus.xdsl"
      :keep     ('fisheries:ReefQuality 'fisheries:EstuaryQuality)
      :context  (bleaching-fisheries reef-area estuary-area nitrogen))) 

;; --------------------------------------------------------------------------------------
;; Use models
;; --------------------------------------------------------------------------------------

;; FV FIXME
;; the distance to coast thing urgently needs to reclassify a proper distance layer or algorithm
;; with actual numbers and not specific of fisheries
;; 
;; the discretization should be bounded so that non-coastal proximate areas return nulls, so it can be used
;; to clip the bayesian network instead of having to write a "subsistence-selector" as below
;; 
(defmodel coastal-proximity 'fisheries:CoastalProximity
	(classification (measurement 'fisheries:DistanceToCoast "km")
        1       'fisheries:HighCoastalProximity
        5       'fisheries:ModerateCoastalProximity
        [25 :>] 'fisheries:LowCoastalProximity))
;;		[0  1] 'fisheries:HighCoastalProximity
;;      [1  5] 'fisheries:ModerateCoastalProximity
;;		[5 :>] 'fisheries:LowCoastalProximity))

(defmodel poverty 'fisheries:Poverty
	(classification (ranking 'policytarget:PovertyPercentage)
		[50 :>]   'fisheries:HighPoverty
		[25 50]   'fisheries:ModeratePoverty
		[:< 25]   'fisheries:LowPoverty))

(defmodel population-density 'fisheries:PopulationDensity
	(classification (count 'policytarget:PopulationDensity "/km^2")
		[2000 :>]    'fisheries:VeryHighPopulationDensity
		[1000 2000]  'fisheries:HighPopulationDensity
		[200 1000]   'fisheries:ModeratePopulationDensity
		[50 200]     'fisheries:LowPopulationDensity
		[:< 50]      'fisheries:VeryLowPopulationDensity))

;; Assume high subsistence use = per capita demand of 6.8 kg fish/yr, moderate use = 4.6 kg fish/yr
;; low use = 2.3 kg fish/yr.  This calculates total demand.
(defmodel subsistence-fishing-undiscretized 'fisheries:SubsistenceUse
  (classification 'fisheries:SubsistenceUse
    :units "kg/km^2*year" ; per person, multiply by population-density.
    6.8  'fisheries:HighSubsistenceUse
    4.6  'fisheries:ModerateSubsistenceUse
    2.3  'fisheries:LowSubsistenceUse
    0    'fisheries:NoSubsistenceUse))

;;(defmodel subsistence-fishing-undiscretized 'fisheries:SubsistenceUse
;;  (classification 'fisheries:SubsistenceUse
;;    :units "kg/km^2*year"
;;    :context ((count 'policytarget:PopulationDensity "/km^2" :as population-density-count))
;;    ;; This classification syntax is documented as working but isn't implemented!
;;    (* (:population-density-count self) 6.8)  'fisheries:HighSubsistenceUse
;;    (* (:population-density-count self) 4.6)  'fisheries:ModerateSubsistenceUse
;;    (* (:population-density-count self) 2.3)  'fisheries:LowSubsistenceUse
;;    0                                         'fisheries:NoSubsistenceUse))

;; FIXME this should be removed when the distance to coast model is done properly
(defmodel subsistence-selector 'fisheries:ProximityBuffer
  (classification 'fisheries:ProximityBuffer
    :context (coastal-proximity)
    :state  #(if (or (tl/is? (:coastalproximity %) 'fisheries:ModerateCoastalProximity)
                     (tl/is? (:coastalproximity %) 'fisheries:LowCoastalProximity)                   
                     (tl/is? (:coastalproximity %) 'fisheries:HighCoastalProximity))
               (tl/conc 'fisheries:ProximityBufferPresent) 
               nil)))

(defmodel subsistence-fishing 'fisheries:SubsistenceFishing
  "Interface to subsistence use bayesian network"
  (bayesian 'fisheries:SubsistenceFishing
            :import   "aries.marine::FisheriesSubsistenceUse.xdsl"
            :keep     ('fisheries:SubsistenceUse)
;; FIXME substitute below with distance to coast when it's correctly written and based on decent data
            :required ('fisheries:ProximityBuffer)
            :observed (subsistence-fishing-undiscretized)
            :context  (poverty population-density coastal-proximity subsistence-selector)))

;; --------------------------------------------------------------------------------------
;; Flow models
;; --------------------------------------------------------------------------------------

(defmodel paths 'infrastructure:Path
  (binary-coding 'infrastructure:Path))

(defmodel fish-flow-data 'fisheries:TempFishFlowData$
  (identification 'fisheries:TempFishFlowData
      :context (paths (count 'policytarget:PopulationDensity "/km^2"))))

(defmodel fisheries-subsistence-data 'fisheries:SubsistenceFishProvision
	(identification 'fisheries:SubsistenceFishProvision
		:context (total-pelagic-subsistence-harvest subsistence-fishing 
              fish-flow-data fish-habitat-quality)))

(defmodel fisheries-subsistence-flow 'fisheries:SubsistenceFishProvision
  (span 'fisheries:SubsistenceFishAccessibility
        'fisheries:TotalSubsistenceHarvest
        'fisheries:SubsistenceUse ;; SubsistenceFishing
        nil
        nil
        'fisheries:TempFishFlowData
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
        :save-file          "coastal-fisheries-data.clj"
        :context            (total-pelagic-subsistence-harvest subsistence-fishing fish-flow-data)
        :keep               ('fisheries:SubsistenceFishSupply
                             'fisheries:SubsistenceFishDemand
                             'fisheries:SubsistenceFishFlow
                             'fisheries:UtilizedSubsistenceFish
                             'fisheries:SatisfiedSubsistenceFishDemand
                             'fisheries:UnutilizedSubsistenceFish
                             'fisheries:UnsatisfiedSubsistenceFishDemand)))
