;;Proximity model for San Pedro
(ns core.models.aesthetic-proximity-san-pedro
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

;; ----------------------------------------------------------------------------------------------
;; source model
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

;;Figure out how to get quality represented here using riparian + condition class
(defmodel riparian-wetland 'sanPedro:RiparianAndWetland
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)
                  #{77 78 79 81 85 94 97 98 109 110}     'sanPedro:HighQualityRiparianOrWetlandPresent     ;; + condition class 3
                  #{77 78 79 81 85 94 97 98 109 110 118} 'sanPedro:ModerateQualityRiparianOrWetlandPresent ;; + condition class 2
                  #{77 78 79 81 85 94 97 98 109 110 118} 'sanPedro:LowQualityRiparianOrWetlandPresent      ;; + condition class 1             
                  :otherwise                             'sanPedro:RiparianOrWetlandAbsent))

;;No data yet for parks - though would likely only matter in cities anyway.
;;(defmodel park 'aestheticService:Park
;;  (classification (binary-coding 'aestheticService:Park)
;;                  0          'aestheticService:ParkAbsent
;;                  :otherwise 'aestheticService:ParkPresent))

(defmodel fire-threat 'aestheticService:FireThreat
  (classification (numeric-coding 'habitat:FireReturnInterval) 
                  #{1 2 3}        'aestheticService:HighFireThreat ;; includes high, moderate, variable fire frequency
                  #{4 5 6}        'aestheticService:LowFireThreat))

;;This uses the WDPA data - need to double check that the numbers correspond to protected/not protected (see what's getting output).
;; Might be worthwhile to replace with local data
(defmodel formal-protection 'aestheticService:FormalProtection
  (classification (binary-coding 'conservation:ProtectedStatus)
                  0            'aestheticService:Protected
                  :otherwise   'aestheticService:NotProtected)) 

;;CHECK WITH FERD ON HOW TO CALCULATE AREA
;;(defmodel area 'aestheticService:OpenSpaceArea...

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
            :context  (forest farmland grassland desert-scrub riparian-wetland fire-threat formal-protection)
            :observed (theoretical-open-space)
            :keep     ('aestheticService:TheoreticalProximitySource)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel highway 'aestheticService:Highways 
  (classification (binary-coding 'infrastructure:Highway)
                  0          'aestheticService:HighwaysAbsent
                  :otherwise 'aestheticService:HighwaysPresent))

;;Check with Gary that the below statement is correct; if so remove highways statement above.
(defmodel sink 'aestheticService:ProximitySink
  (numeric-coding 'aestheticService:ProximitySink
    :context ((binary-coding 'infrastructure:Highway :as highway))
    :state #(cond (== (:highway %) 1) 90   ;;90% of proximity value is depleted by the sink if highways are present
                  :otherwise          0))) ;;Otherwise zero sink

;; ----------------------------------------------------------------------------------------------
;; use model
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
            :import  "aries.core::ProximityUse.xdsl"
            :context (property-value urban-proximity housing)
            :observed (proximity-use-undiscretizer) 
            :keep    ('aestheticService:HomeownerProximityUse)))

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

;;REMOVE THIS, YES??
(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'aestheticService:Proximity
  (identification 'aestheticService:Proximity
                  :context (source :as source
                                   homeowners :as use
                                   sink       :as sink
                                   altitude   :as altitude)))  ;;Remove?

;; the real enchilada - need to be updated to the latest SPAN language
(defmodel proximity 'aestheticService:AestheticProximity
  (span 'aestheticService:Proximity
        'aestheticService:AestheticProximityProvision
        'aestheticService:ProximityUse
      	'aestheticService:ProximitySink
      	nil
        'geophysics:Altitude ;;DELETE THIS??
        ;;:source-threshold   100.0  ;;Initially set as the midpoint of the lowest bin
        ;;:sink-threshold     450.0  ;;Initially set as the midpoint of the lowest bin
        ;;:use-threshold      0.0    ;;Set at zero since output values for this are a 0/1
        ;;:trans-threshold    10.0   ;;Set at an initially arbitrary but low weight; eventually run sensitivity analysis on this
        ;;:source-type    :
        :sink-type        :relative
        :use-type         :relative
        :benefit-type     :non-rival
        :downscaling-factor 3
        :rv-max-states      10
        :keep ('aestheticService:PotentialProximateOpenSpace 'aestheticService:PotentialProximitySink 'aestheticService:HomeownersWithOpenSpaceDemand
               'aestheticService:PossibleProximateOpenSpace 'aestheticService:AccessibleOpenSpace 'aestheticService:OpenSpaceProximiateHomeowners
               'aestheticService:AccessibleProximity 'aestheticService:EnjoyedOpenSpace 'aestheticService:BlockingProximitySink
               'aestheticService:HomeownersWithProximiateOpenSpace 'aestheticService:UnaccessedOpenSpace 'aestheticService:InaccessibleProximitySink
               'aestheticService:HomeownersWithoutProximateOpenSpace 'aestheticService:BlockedProximity 'aestheticService:BlockedOpenSpace
               'aestheticService:HomeownersWithBlockedProximity)
        :context (source
                  homeowners
                  sink
                  altitude
                  (ranking 'eserv:SourceThreshold :value 50 :min 0 :max 100)
                  (ranking 'eserv:SinkThreshold :value 0.3 :min 0 :max 1)
                  (ranking 'eserv:UseThreshold :value 0.1 :min 0 :max 1)
                  (ranking 'eserv:TransitionThreshold :value 1.0))))
