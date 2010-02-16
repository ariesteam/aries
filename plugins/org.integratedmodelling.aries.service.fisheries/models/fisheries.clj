;; --------------------------------------------------------------------------------------------------
;; UNEP marine project
;; models for coastal protection
;; fv Jan 10
;; --------------------------------------------------------------------------------------------------

(ns aries/marine
	(:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian)))

;; --------------------------------------------------------------------------------------
;; use models
;; --------------------------------------------------------------------------------------

;; FIXME the MG distcoast layer is screwed up - this uses an incomplete
;; buffer layer that only encodes one buffer zone, therefore the moderate
;; class is never encoded.
(defmodel coastal-proximity 'fisheries:DistanceToCoast
	(classification (ranking 'fisheries:DistanceToCoast)
		1 'fisheries:HighCoastalProximity
		:otherwise 'fisheries:LowCoastalProximity))

(defmodel poverty 'fisheries:Poverty
	(classification (ranking 'policytarget:PovertyPercentage)
		[50 :>]   'fisheries:HighPoverty
		[25 50]   'fisheries:ModeratePoverty
		[:< 25]   'fisheries:LowPoverty))
		
;; the ranking model should really be a count with spatial ctx 
;; (data are persons/30 arc-second pixel)
(defmodel population-density 'fisheries:PopulationDensity
	(classification (ranking 'policytarget:PopulationDensity)
		[25000 :>]    'fisheries:VeryHighPopulationDensity
		[11000 25000] 'fisheries:HighPopulationDensity
		[3900 11000]  'fisheries:ModeratePopulationDensity
		[750 3900]    'fisheries:LowPopulationDensity
		[:< 750]      'fisheries:VeryLowPopulationDensity))

(defmodel subsistence-fishing 'fisheries:SubsistenceFishing
  	"Interface to subsistence use bayesian network"
	  (bayesian 'fisheries:SubsistenceFishing)
	  	:import   "aries.marine::FisheriesSubsistenceUse.xdsl"
	  	:keep     ('fisheries:SubsistenceUse)
	 	 	:context  (poverty population-density coastal-proximity))
	 	 	
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
 	
;; FIXME this is a measurement in degrees^2; can't get it into jscience  	  
(defmodel reef-area 'fisheries:CoralReefArea
	(classification (ranking 'fisheries:CoralReefArea)
		[0.02 :>]         'fisheries:HighReefArea
		[0.002 0.02]      'fisheries:ModerateReefArea
		[:exclusive 0 0.002]   'fisheries:LowReefArea
		nil               'fisheries:NoReefArea))
 	  
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
	  (bayesian 'fisheries:FishHabitat)
	  	:import   "aries.marine::FisheriesA_hololepidotus.xdsl"
	  	:keep     ('fisheries:ReefQuality 'fisheries:EstuaryQuality)
	 	 	:context  (bleaching-fisheries reef-area estuary-area nitrogen))		
		
;; --------------------------------------------------------------------------------------
;; all together now
;; --------------------------------------------------------------------------------------

(defmodel fisheries-subsistence-data 'fisheries:SubsistenceFishProvision
	(identification 'fisheries:SubsistenceFishProvision)
		:context (meagre-habitat fish-habitat-quality subsistence-fishing))
	 	 	