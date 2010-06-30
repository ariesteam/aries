;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; fv Jan 10
;; --------------------------------------------------------------------------------------------------

(ns marine.models.fisheries
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization ranking numeric-coding binary-coding identification bayesian count))
  (:refer aries :only (span)))
;; --------------------------------------------------------------------------------------
;; use models
;; --------------------------------------------------------------------------------------

;;CHANGE DISCRETIZATION BELOW TO FIX...
(defmodel coastal-proximity 'fisheries:DistanceToCoast
	(classification (ranking 'fisheries:DistanceToCoast)
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

(defmodel subsistence-fishing 'fisheries:SubsistenceFishing
  	"Interface to subsistence use bayesian network"
	  (bayesian 'fisheries:SubsistenceFishing 
	  	:import   "aries.marine::FisheriesSubsistenceUse.xdsl"
	  	:keep     ('fisheries:SubsistenceUse)
	 	 	:context  (poverty population-density coastal-proximity)))
	 	 	
;; --------------------------------------------------------------------------------------
;; source models
;; --------------------------------------------------------------------------------------

;; TODO classify
(defmodel meagre-habitat 'fisheries:ArgyrosomusHololepidotusHabitat
	(ranking 'fisheries:ArgyrosomusHololepidotusHabitat))

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
 	
;; Converted to km^2 so should work now but need to get into xml via wfs2opal & test in Thinkcap  	  
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
 	  
;; FIXME the source model is a measurement in kg/ha.yr
;; FIXME the discretization in the BN is different than the excel - compacting high classes
(defmodel nitrogen 'fisheries:NitrogenRunoff
	(classification (ranking 'policytarget:NitrogenFromFertilizerAndManure)
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
;; all together now
;; --------------------------------------------------------------------------------------

(defmodel fisheries-subsistence-data 'fisheries:SubsistenceFishProvision
	(identification 'fisheries:SubsistenceFishProvision 
		:context (meagre-habitat fish-habitat-quality subsistence-fishing)))
	 	 	