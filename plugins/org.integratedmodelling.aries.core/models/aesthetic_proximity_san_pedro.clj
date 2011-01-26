;;Proximity model for San Pedro
(ns core.models.aesthetic-proximity-san-pedro
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;Data on land cover types are for the U.S. only, using SWReGAP data.  Since we have no parcel/housing location
;; data for Mexico, plan to solely run model in the U.S. on U.S. LULC types.
;; Bare rock/dune, etc. are not included here
(defmodel forest 'sanPedro:ForestAndWoodland
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC) 
                  #{22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 41 45 46 63 64 91 92 95 101 102 103} 'sanPedro:ForestOrWoodlandPresent               
                  :otherwise                                                                                'sanPedro:ForestOrWoodlandAbsent))

(defmodel farmland 'aestheticService:Farmland
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  114          'aestheticService:FarmlandPresent                     
                  :otherwise   'aestheticService:FarmlandAbsent))

(defmodel grassland 'aestheticService:Grassland
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  #{65 68 73 74 75 76 90 93 106}  'aestheticService:GrasslandPresent                     
                  :otherwise                      'aestheticService:GrasslandAbsent))

(defmodel desert-scrub 'aestheticService:DesertScrub ;; includes chapparal and other shrubland
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  #{40 44 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 66 67 82 94 96 97 105 108}  'aestheticService:DesertScrubPresent                  
                  :otherwise                                                                          'aestheticService:DesertScrubAbsent))

;;This model assumes that all riparian areas that are not mapped within the SPRNCA are low quality.  This is a poor assumption -
;; moderate quality might also be appropriate and it would be better to run these as a simple BN for presence and quality like
;; the housing presence and value BNs, incoprorating priors for quality when we lack data.
(defmodel riparian-wetland-code 'sanPedro:RiparianAndWetlandCode
  (numeric-coding 'sanPedro:RiparianAndWetlandCode
                  :context ((numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC :as lulc)
                            (ranking 'sanPedro:RiparianConditionClass :as condition))
                  :state   #(if (contains? #{77.0 78.0 79.0 80.0 81.0 83.0 84.0 85.0 98.0 109.0 110.0 118.0} (:lulc %))
                                (let [condition (:condition %)]
                                  (if (or (nil? condition) (Double/isNaN condition))
                                    1
                                    condition))
                                0)))

(defmodel riparian-wetland 'sanPedro:RiparianAndWetland
  (classification riparian-wetland-code
                  3 'sanPedro:HighQualityRiparianOrWetlandPresent
                  2 'sanPedro:ModerateQualityRiparianOrWetlandPresent
                  1 'sanPedro:LowQualityRiparianOrWetlandPresent
                  0 'sanPedro:RiparianOrWetlandAbsent))

(defmodel park 'aestheticService:Park
    (classification (numeric-coding 'habitat:LandOwnership)
     #{8 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 
       117 118 119 120 121 122 123 124 125 126 127}   'aestheticService:ParkPresent
     :otherwise                                       'aestheticService:ParkAbsent)) 

(defmodel fire-threat 'aestheticService:FireThreat
  (classification (numeric-coding 'habitat:FireReturnInterval) 
                  #{1 2 3}        'aestheticService:HighFireThreat ;; includes high, moderate, variable fire frequency
                  #{4 5 6}        'aestheticService:LowFireThreat))

;;This uses the WDPA data - need to double check that the numbers correspond to protected/not protected (see what's getting output).
;; Might be worthwhile to replace with local data.
;; Should also change unprotected land to state & private land only: other BLM for instance isn't going to be developed.
(defmodel formal-protection 'aestheticService:FormalProtection
  (classification (binary-coding 'conservation:ProtectedStatus)
                  1            'aestheticService:Protected
                  :otherwise   'aestheticService:NotProtected)) 

;; Computes area of open space polygons as a GIS operation and stores this value in each pixel
(defmodel area 'aestheticService:OpenSpaceAreaClass
  (classification (measurement 'aestheticService:OpenSpaceArea "ha")
                  [40 :>] 'aestheticService:VeryLargeArea
                  [10 40] 'aestheticService:LargeArea
                  [2 10]  'aestheticService:SmallArea
                  [:< 2]  'aestheticService:VerySmallArea))

(defmodel theoretical-open-space 'aestheticService:TheoreticalProximitySource
  (classification 'aestheticService:TheoreticalProximitySource
                  [0   10] 'aestheticService:NoProximityPotential 
                  [10  40] 'aestheticService:LowProximityPotential 
                  [40  75] 'aestheticService:ModerateProximityPotential 
                  [75 100] 'aestheticService:HighProximityPotential))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticProximityProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticProximityProvision
            :import   "aries.core::ProximitySourceSanPedro.xdsl"
            :context  (forest farmland grassland desert-scrub park fire-threat formal-protection riparian-wetland area) 
            :observed (theoretical-open-space)
            :keep     ('aestheticService:TheoreticalProximitySource)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel sink 'aestheticService:ProximitySink
  (ranking 'aestheticService:ProximitySink
           :context ((binary-coding 'infrastructure:Highway :as highway))
           :state #(cond (== (:highway %) 1) 50   ;;50 units of proximity value are depleted by the sink if highways are present
                         :otherwise          0))) ;;Otherwise zero sink

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel housing 'aestheticService:PresenceOfHousing
  (classification (ranking 'economics:AppraisedPropertyValue)
        [1 :>]            'aestheticService:HousingPresent
        :otherwise        'aestheticService:HousingAbsent))
;;  (classification (numeric-coding 'nlcd:NLCDNumeric) ;;Using NLCD where parcel data are unavailable.
;;        [22 23 24]   'aestheticService:HousingPresent  ;;Assumes (incorrectly) that all developed land is housing.
;;        :otherwise   'aestheticService:HousingAbsent))

(defmodel property-value 'aestheticService:HousingValue  ;; value is in $/ac, which is not a legitimate unit in thinklab, so kept as a ranking for now.
  (classification (ranking 'economics:AppraisedPropertyValue)
                  [0         10000] 'aestheticService:VeryLowHousingValue
                  [10000    25000]  'aestheticService:LowHousingValue
                  [25000   50000]   'aestheticService:ModerateHousingValue
                  [50000  200000]   'aestheticService:HighHousingValue
                  [200000 :>]       'aestheticService:VeryHighHousingValue))

;;Urban proximity proxied by year 2000 population density for Arizona
(defmodel urban-proximity 'aestheticService:UrbanProximity
  (classification (count 'policytarget:PopulationDensity "/km^2")
                  [309 :>] 'aestheticService:Urban
                  [77 309] 'aestheticService:Suburban
                  [:< 77]  'aestheticService:Rural))

;;undiscretizer for proximty use
(defmodel proximity-use-undiscretizer 'aestheticService:HomeownerProximityUse
  (classification 'aestheticService:HomeownerProximityUse
                  [0 0.05]   'aestheticService:HomeownerProximityUseAbsent 
                  [0.05 1] 'aestheticService:HomeownerProximityUsePresent))

;; bayesian model
(defmodel homeowners 'aestheticService:ProximityUse
  "Property owners who can afford to pay for proximity to open space"
  (bayesian 'aestheticService:ProximityUse 
            :import  "aries.core::ProximityUseSanPedro.xdsl"
            :context (urban-proximity) ;;property-value housing
            :observed (proximity-use-undiscretizer) 
            :keep    ('aestheticService:HomeownerProximityUse)))						

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;;Gary: For the flow models, distance decay on proxmity should usually be steep (i.e., very steep after 0.5 mi, almost
;; nothing left after 1.0 mi.  However, this decay should be a little less steep for rivers in western regions (i.e.,
;; San Pedro) - perhaps a similar decay function stretched with an inflection point at 1.0 mi and decaying to very little
;; at 2.0 mi.

;; all data, for testing and storage
(defmodel data 'aestheticService:Proximity
  (identification 'aestheticService:Proximity
                  :context (source     :as source
                            homeowners :as use
                            sink       :as sink)))

(defmodel proximity 'aestheticService:AestheticProximity
  (span 'aestheticService:Proximity
        'aestheticService:TheoreticalProximitySource
        'aestheticService:HomeownerProximityUse
      	'aestheticService:ProximitySink
      	nil
        nil
        :source-threshold   5.0  ;;Initially set as the midpoint of the lowest bin
        :sink-threshold     0.0  
        :use-threshold      4.0   ;;Set just below the "no use" threshold in the use model; run sensitivity analysis on this
        :trans-threshold    4.0   ;;Set just below the "no use" threshold in the use model; run sensitivity analysis on this
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 2
        :rv-max-states      10
        :keep ('aestheticService:PotentialProximateOpenSpace 'aestheticService:PotentialProximitySink 'aestheticService:HomeownersWithOpenSpaceDemand
               'aestheticService:PossibleProximateOpenSpace 'aestheticService:AccessibleOpenSpace 'aestheticService:OpenSpaceProximateHomeowners
               'aestheticService:AccessibleProximity 'aestheticService:EnjoyedOpenSpace 'aestheticService:BlockingProximitySink
               'aestheticService:HomeownersWithProximateOpenSpace 'aestheticService:UnaccessedOpenSpace 'aestheticService:InaccessibleProximitySink
               'aestheticService:HomeownersWithoutProximateOpenSpace 'aestheticService:BlockedProximity 'aestheticService:BlockedOpenSpace
               'aestheticService:HomeownersWithBlockedProximity)
        :context (source homeowners sink)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios 

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario mesquite-management 'sanPedro:MesquiteManagement: change woodland to grassland in source model.

;;(defscenario cap-water-augmentation 'sanPedro:CAPWaterAugmentation: Change riparianandwetlandquality in source model.
      ;;sanPedro:CAPWaterAugmentationHalfMeterRise
      ;;sanPedro:CAPWaterAugmentationAllPerennial
      
;;(defscenario urban-growth 'sanPedro:UrbanGrowth: change open space type to developed in source model.  Add more users to the
;;  landscape.
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development 'sanPedro:BSRDevelopment: change open space type to developed in source model.  Add more users to the
;;  landscape.
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5
