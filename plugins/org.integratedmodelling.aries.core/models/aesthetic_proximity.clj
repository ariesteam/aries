;;Proximity model for Western Washington
(ns core.models.aesthetic-proximity
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel lake-front 'aestheticService:LakeFront
  (classification (binary-coding 'aestheticService:LakeFrontPresence)
                  0          'aestheticService:LakeFrontAbsent
                  :otherwise 'aestheticService:LakeFrontPresent))

(defmodel river-front 'aestheticService:RiverFront
  (classification (binary-coding 'aestheticService:RiverFrontPresence)
                  0          'aestheticService:RiverFrontAbsent
                  :otherwise 'aestheticService:RiverFrontPresent))

(defmodel beach 'aestheticService:Beach
  (classification (binary-coding 'aestheticService:BeachPresence)
                  0          'aestheticService:BeachAbsent
                  :otherwise 'aestheticService:BeachPresent))

(defmodel forest 'aestheticService:Forest
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{41 42 43}  'aestheticService:ForestPresent                  
                  :otherwise   'aestheticService:ForestAbsent))

(defmodel woody-wetland 'aestheticService:WoodyWetland
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  90           'aestheticService:WoodyWetlandPresent                  
                  :otherwise   'aestheticService:WoodyWetlandAbsent))

(defmodel emergent-wetland 'aestheticService:EmergentWetland
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  95           'aestheticService:EmergentWetlandPresent                  
                  :otherwise   'aestheticService:EmergentWetlandAbsent))

(defmodel farmland 'aestheticService:Farmland
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  #{81 82}     'aestheticService:FarmlandPresent                  
                  :otherwise   'aestheticService:FarmlandAbsent))

(defmodel park 'aestheticService:Park
  (classification (binary-coding 'aestheticService:ParkPresence)
                  0          'aestheticService:ParkAbsent
                  :otherwise 'aestheticService:ParkPresent))

(defmodel crime-potential 'aestheticService:CrimePotential
  (classification (categorization 'geofeatures:City)
                  #{"Seattle" "Tacoma"} 'aestheticService:HighCrimePotential
                  :otherwise            'aestheticService:LowCrimePotential))

(defmodel water-quality 'aestheticService:WaterQuality
  (classification (ranking 'aestheticService:WaterQualityAssessment)
                  1            'aestheticService:MeetsStandards
                  #{2 4 24}    'aestheticService:OfConcern
                  5            'aestheticService:RequiringTMDL
                  :otherwise   'aestheticService:NoSurfaceWater))

;;This is set as a Puget-specific concept since it's a binary coding rather than a ranking like the global
;; conservation status dataset.
(defmodel formal-protection 'aestheticService:FormalProtection
  (classification (binary-coding 'puget:ProtectedStatus)
                  1            'aestheticService:Protected
                  :otherwise   'aestheticService:NotProtected)) 

;; Compute area of open space polygons as a GIS operation and store this value redundantly in each pixel in the 
;; polygon.  Make sure all the appropriate ontology changes are made (including inheriting from Areas and all 
;; that jazz).
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
            :import   "aries.core::ProximitySource.xdsl"
            :context  (lake-front river-front beach forest woody-wetland emergent-wetland farmland park
                       crime-potential water-quality formal-protection)
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
  (classification (binary-coding 'aestheticService:PresenceOfHousing)
        "RESIDENTIAL" 'aestheticService:HousingPresent  ;;CHANGE TO "IF ZERO OR GREATER" housing present, otherwise not.
        :otherwise    'aestheticService:HousingNotPresent))

(defmodel property-value 'aestheticService:HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  'economics:AppraisedPropertyValue)
                  [:<       100000] 'aestheticService:VeryLowHousingValue
                  [100000   200000] 'aestheticService:LowHousingValue
                  [200000   400000] 'aestheticService:ModerateHousingValue
                  [400000  1000000] 'aestheticService:HighHousingValue
                  [1000000 :>]      'aestheticService:VeryHighHousingValue))

;;Urban proximity proxied by year 2007 population density for Washington
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

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data 'aestheticService:Proximity
  (identification 'aestheticService:Proximity
                  :context (source     :as source
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
        :source-type      :infinite
        :sink-type        :infinite
        :use-type         :infinite
        :benefit-type     :non-rival
        :downscaling-factor 3
        :rv-max-states      10
        :keep ('aestheticService:PotentialProximateOpenSpace
               'aestheticService:PotentialProximitySink
               'aestheticService:HomeownersWithOpenSpaceDemand
               'aestheticService:PossibleProximateOpenSpace
               'aestheticService:AccessibleOpenSpace
               'aestheticService:OpenSpaceProximiateHomeowners
               'aestheticService:AccessibleProximity
               'aestheticService:EnjoyedOpenSpace
               'aestheticService:BlockingProximitySink
               'aestheticService:HomeownersWithProximateOpenSpace
               'aestheticService:UnaccessedOpenSpace
               'aestheticService:InaccessibleProximitySink
               'aestheticService:HomeownersWithoutProximateOpenSpace
               'aestheticService:BlockedProximity
               'aestheticService:BlockedOpenSpace
               'aestheticService:HomeownersWithBlockedProximity)
        :context (source
                  homeowners
                  sink
                  (ranking 'eserv:SourceThreshold :value 50 :min 0 :max 100)
                  (ranking 'eserv:SinkThreshold :value 0.3 :min 0 :max 1)
                  (ranking 'eserv:UseThreshold :value 0.1 :min 0 :max 1)
                  (ranking 'eserv:TransitionThreshold :value 1.0))))
