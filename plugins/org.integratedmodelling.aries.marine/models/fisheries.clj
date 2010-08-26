;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for subsistence fisheries
;; --------------------------------------------------------------------------------------------------

(ns marine.models.fisheries
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))
;; --------------------------------------------------------------------------------------
;; use models
;; --------------------------------------------------------------------------------------

(defmodel coastal-proximity 'fisheries:DistanceToCoast
	(classification (measurement 'fisheries:DistanceToCoast "km")
		1       'fisheries:HighCoastalProximity
    5       'fisheries:ModerateCoastalProximity
		[25 :>] 'fisheries:LowCoastalProximity))

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

;;Assume high subsistence use = per capita demand of 6.8 kg fish/yr, moderate use = 4.6 kg fish/yr
;; low use = 2.3 kg fish/yr.  This calculates total demand.
(defmodel subsistence-fishing-undiscretized 'fisheries:SubsistenceUse
  (classification 'fisheries:SubsistenceUse
    :units "kg/km^2*year" 
    :context ((count 'policytarget:PopulationDensity "/km^2" :as population-density-count))
    ;; This classification syntax is documented as working but isn't implemented!
    (* (:population-density-count self) 6.8)  'fisheries:HighSubsistenceUse
    (* (:population-density-count self) 4.6)  'fisheries:ModerateSubsistenceUse
    (* (:population-density-count self) 2.3)  'fisheries:LowSubsistenceUse
    0                                         'fisheries:NoSubsistenceUse))

(defmodel subsistence-fishing 'fisheries:SubsistenceFishing
  	"Interface to subsistence use bayesian network"
	  (bayesian 'fisheries:SubsistenceFishing 
	  	:import   "aries.marine::FisheriesSubsistenceUse.xdsl"
	  	:keep     ('fisheries:SubsistenceUse)
      :observed (subsistence-fishing-undiscretized) 
	 	 	:context  (poverty population-density coastal-proximity)))

;; --------------------------------------------------------------------------------------
;; source models
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
      :state    #(do (println "Slender Emperor:"      (:lethrinusborbonicusharvest %))
                     (println "Sky Emperor:"          (:lethrinusmahsenaharvest %))
                     (println "Mangrove Red Snapper:" (:lutjanusargentimaculatusharvest %))
                     (apply + (map (fn [fishval] (or fishval 0.0))
                                   [(:lethrinusborbonicusharvest      %)
                                    (:lethrinusmahsenaharvest          %)
                                    (:lutjanusargentimaculatusharvest %)])))))

;; KB, 8/11/10: Statements below are to link habitat change to fish change.  This is not
;;   part of the 1st generation flow models, and could be added to subsequent marine modeling work.
;; TODO almost all coral polygons in existing data do not report bleaching; I 
;; don't know what text should be in the categories to define other states, so
;; only HighBleaching is reported here if the field isn't empty.
;; TODO this is the same as for coastalProtection, it's a weird file - we should use a more
;; general category anyway if this is going to stay the only datafile for long.
(defmodel bleaching-fisheries 'fisheries:CoralBleaching
	(classification (categorization 'coastalProtection:CoralBleaching)
 		#{nil "None"}			  'fisheries:NoBleaching
 		#{"HIgh" "High"}	  'fisheries:HighBleaching
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
 		"Small"		 'fisheries:SmallEstuary
 		"Large"	   'fisheries:LargeEstuary
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
;; flow models
;; --------------------------------------------------------------------------------------

;;This SPAN statement has just been copied from flood_mark, but the "keep" 
;; list has been updated to correctly reflect the fisheries flow concepts.
(defmodel fisheries-subsistence-flow 'floodService:AvoidedDamageToFarms100
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
        :keep ('fisheries:SubsistenceFishSupply 'fisheries:SubsistenceFishDemand 'fisheries:SubsistenceFishFlow
              'fisheries:UtilizedSubsistenceFish 'fisheries:SatisfiedSubsistenceFishDemand 'fisheries:UnutilizedSubsistenceFish
              'fisheries:UnsatisfiedFishDemand) 
        ;;:save-file          (str (System/getProperty "user.home") "/flood_data_farmers100.clj")
        :context (subsistence-fishing total-pelagic-subsistence-harvest)))

(defmodel paths 'infrastructure:Path
  (binary-coding 'infrastructure:Path))

(defmodel fisheries-subsistence-data 'fisheries:SubsistenceFishProvision
	(identification 'fisheries:SubsistenceFishProvision 
		:context (total-pelagic-subsistence-harvest subsistence-fishing paths)))
	 	 	