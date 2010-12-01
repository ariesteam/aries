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

(defmodel riparian-wetland-code 'sanPedro:RiparianAndWetlandCode
  (numeric-coding 'sanPedro:RiparianAndWetlandCode
                  :context ((numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC :as lulc)
                            (ranking 'sanPedro:RiparianConditionClass :as condition))
                  :state   #(cond (and (== (:condition %) 1)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
                                  1 ;;'sanPedro:LowQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 2)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110 118} (:lulc %)))
                                  2 ;;'sanPedro:ModerateQualityRiparianOrWetlandPresent

                                  (and (== (:condition %) 3)
                                       (contains? #{77 78 79 80 81 83 84 85 98 109 110} (:lulc %)))
                                  3 ;;'sanPedro:HighQualityRiparianOrWetlandPresent

                                  :otherwise 0 ;;'sanPedro:RiparianOrWetlandAbsent
                                  )))
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
;; Might be worthwhile to replace with local data
(defmodel formal-protection 'aestheticService:FormalProtection
  (classification (binary-coding 'conservation:ProtectedStatus)
                  1            'aestheticService:Protected
                  :otherwise   'aestheticService:NotProtected)) 

;; Compute area of open space polygons as a GIS operation and store this value redundantly in each pixel in the 
;; polygon.  
;;(defmodel area 'aestheticService:OpenSpaceAreaClass
;;  (clasification (measurement 'aestheticService:OpenSpaceArea "ha")
;;                  [40 :>] 'aestheticService:VeryLargeArea
;;                  [10 40] 'aestheticService:LargeArea
;;                  [2 10]  'aestheticService:SmallArea
;;                  [2 :>]  'aestheticService:VerySmallArea)) 

(defmodel theoretical-open-space 'aestheticService:TheoreticalProximitySource
  (classification 'aestheticService:TheoreticalProximitySource
                  [0   10] 'aestheticService:NoProximityValue 
                  [10  40] 'aestheticService:LowProximityValue 
                  [40  75] 'aestheticService:ModerateProximityValue 
                  [75 100] 'aestheticService:HighProximityValue))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticProximityProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticProximityProvision
            :import   "aries.core::ProximitySourceSanPedro.xdsl"
            :context  (forest farmland grassland desert-scrub riparian-wetland park fire-threat formal-protection)
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
  "Classifies land use from property data."
  ;; specific to Puget region, will not be used if data unavailable
  (classification (categorization 'puget:ParcelUseCategoryKing)
                  #{"R" "K"}  'aestheticService:HousingPresent
                  :otherwise  'aestheticService:HousingAbsent))

(defmodel property-value 'aestheticService:HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  'economics:AppraisedPropertyValue)
                  [:<       100000] 'aestheticService:VeryLowHousingValue
                  [100000   200000] 'aestheticService:LowHousingValue
                  [200000   400000] 'aestheticService:ModerateHousingValue
                  [400000  1000000] 'aestheticService:HighHousingValue
                  [1000000 :>]      'aestheticService:VeryHighHousingValue))

;;Urban proximity proxied by year 2000 population density for Arizona
(defmodel urban-proximity 'aestheticService:UrbanProximity
  (classification (count 'policytarget:PopulationDensity "/km^2")
                  [309 :>] 'aestheticService:Urban
                  [77 309] 'aestheticService:Suburban
                  [77 :>]  'aestheticService:Rural))

;;undiscretizer for proximty use
(defmodel proximity-use-undiscretizer 'aestheticService:HomeownerProximityUse
  (classification 'aestheticService:HomeownerProximityUse
                  [0 5]   'aestheticService:HomeownerProximityUseAbsent 
                  [5 100] 'aestheticService:HomeownerProximityUsePresent))

;; bayesian model
(defmodel homeowners 'aestheticService:ProximityUse
  "Property owners who can afford to pay for proximity to open space"
  (bayesian 'aestheticService:ProximityUse 
            :import  "aries.core::ProximityUseSanPedro.xdsl"
            :context (property-value urban-proximity housing)
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
                  :context (source :as source
                                   homeowners :as use
                                   sink       :as sink)))

;; the real enchilada - need to be updated to the latest SPAN language
(defmodel proximity 'aestheticService:AestheticProximity
  (span 'aestheticService:Proximity
        'aestheticService:AestheticProximityProvision
        'aestheticService:ProximityUse
      	'aestheticService:ProximitySink
      	nil
        nil
        ;;:source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        ;;:sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        ;;:use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        ;;:trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 3
        :rv-max-states      10
        :keep ('aestheticService:PotentialProximateOpenSpace 'aestheticService:PotentialProximitySink 'aestheticService:HomeownersWithOpenSpaceDemand
               'aestheticService:PossibleProximateOpenSpace 'aestheticService:AccessibleOpenSpace 'aestheticService:OpenSpaceProximiateHomeowners
               'aestheticService:AccessibleProximity 'aestheticService:EnjoyedOpenSpace 'aestheticService:BlockingProximitySink
               'aestheticService:HomeownersWithProximiateOpenSpace 'aestheticService:UnaccessedOpenSpace 'aestheticService:InaccessibleProximitySink
               'aestheticService:HomeownersWithoutProximateOpenSpace 'aestheticService:BlockedProximity 'aestheticService:BlockedOpenSpace
               'aestheticService:HomeownersWithBlockedProximity)
        :context (source homeowners sink)))
