;;Proximity model for Western Washington
(ns core.models.aesthetic-proximity
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization 
                           namespace-ontology ranking
                           probabilistic-measurement probabilistic-classification probabilistic-ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService)

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel lake-front LakeFront
  (classification (binary-coding LakeFrontPresence)
                  1          LakeFrontPresent
                  :otherwise LakeFrontAbsent))

(defmodel river-front RiverFront
  (classification (binary-coding RiverFrontPresence)
                  1          RiverFrontPresent
                  :otherwise RiverFrontAbsent))

(defmodel beach Beach
  (classification (binary-coding BeachPresence)
                  1          BeachPresent
                  :otherwise BeachAbsent))

(defmodel forest Forest
  (classification (numeric-coding nlcd:NLCDNumeric)
                  #{41 42 43}  ForestPresent                  
                  :otherwise   ForestAbsent))

(defmodel woody-wetland WoodyWetland
  (classification (numeric-coding nlcd:NLCDNumeric)
                  90           WoodyWetlandPresent                  
                  :otherwise   WoodyWetlandAbsent))

(defmodel emergent-wetland EmergentWetland
  (classification (numeric-coding nlcd:NLCDNumeric)
                  95           EmergentWetlandPresent                  
                  :otherwise   EmergentWetlandAbsent))

(defmodel farmland Farmland
  (classification (numeric-coding nlcd:NLCDNumeric)
                  #{81 82}     FarmlandPresent                  
                  :otherwise   FarmlandAbsent))

(defmodel park Park
  (classification (binary-coding ParkPresence)
                  1          ParkPresent
                  :otherwise ParkAbsent))

(defmodel crime-potential CrimePotential
  (classification (categorization geofeatures:City)
                  #{"Seattle" "Tacoma"} HighCrimePotential
                  :otherwise            LowCrimePotential))

(defmodel water-quality WaterQuality
  (classification (ranking WaterQualityAssessment)
                  1            MeetsStandards
                  #{2 4 24}    OfConcern
                  5            RequiringTMDL
                  :otherwise   NoSurfaceWater))

;;This is set as a Puget-specific concept since it's a binary coding rather than a ranking like the global
;; conservation status dataset.
(defmodel formal-protection FormalProtection
  (classification (binary-coding puget:ProtectedStatus)
                  1            Protected
                  :otherwise   NotProtected)) 

;; Computed area of open space polygons as a GIS operation and stored this value in each pixel
(defmodel area OpenSpaceAreaClass
  (classification (measurement OpenSpaceArea "ha")
                  [40 :>]           VeryLargeArea
                  [10 40]           LargeArea
                  [2 10]            SmallArea
                  [:exclusive 0 2]  VerySmallArea))

(defmodel theoretical-open-space TheoreticalProximitySource
  (probabilistic-ranking TheoreticalProximitySource
                  [0   10] NoProximityPotential 
                  [10  40] LowProximityPotential
                  [40  75] ModerateProximityPotential 
                  [75 100] HighProximityPotential))

;; source bayesian model	    		 
(defmodel source AestheticProximityProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian AestheticProximityProvision
            :import   "aries.core::ProximitySource.xdsl"
            :context  (lake-front river-front beach forest woody-wetland emergent-wetland farmland park
                       crime-potential water-quality formal-protection area)
            :result   theoretical-open-space
            :keep     (TheoreticalProximitySource)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel sink ProximitySink
  (ranking ProximitySink
           :context ((binary-coding infrastructure:Highway :as highway))
           :state #(cond (== (:highway %) 1) 50   ;;50 units of proximity value are depleted by the sink if highways are present
                         :otherwise          0))) ;;Otherwise zero sink

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel housing PresenceOfHousing
  "Classifies land use from property data."
  (classification (ranking PresenceOfHousing)
        [1 :>]                   HousingPresent  
        :otherwise               HousingAbsent))
;;  (classification (numeric-coding 'nlcd:NLCDNumeric) ;;Using NLCD where parcel data are unavailable.
;;        [22 23 24]   HousingPresent  ;;Assumes (incorrectly) that all developed land is housing.
;;        :otherwise   HousingAbsent))

(defmodel property-value HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  economics:AppraisedPropertyValue)
                  [:exclusive 0 50000]  VeryLowHousingValue
                  [50000       150000]  LowHousingValue
                  [150000       300000] ModerateHousingValue
                  [300000      500000]  HighHousingValue
                  [500000     :>]       VeryHighHousingValue))

;;Urban proximity proxied by year 2007 population density for Washington
(defmodel urban-proximity UrbanProximity
  (classification (count policytarget:PopulationDensity "/km^2")
                  [309 :>] Urban
                  [77 309] Suburban
                  [:< 77]  Rural))

;;undiscretizer for proximty use
(defmodel proximity-use-undiscretizer HomeownerProximityUse
  (probabilistic-ranking HomeownerProximityUse
                  [0 0.05]   HomeownerProximityUseAbsent 
                  [0.05 1]   HomeownerProximityUsePresent))

;; bayesian model
(defmodel homeowners ProximityUse
  "Property owners who can afford to pay for proximity to open space"
  (bayesian ProximityUse 
            :import  "aries.core::ProximityUse.xdsl"
            :context (property-value urban-proximity housing)
            :result  proximity-use-undiscretizer 
            :keep    (HomeownerProximityUse)))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data Proximity
  (identification Proximity
                  :context (source     :as source
                            homeowners :as use
                            sink       :as sink)))

(defmodel proximity AestheticProximity
  (span Proximity
        TheoreticalProximitySource
        HomeownerProximityUse
        ProximitySink
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
        :keep (PotentialProximateOpenSpace PotentialProximitySink HomeownersWithOpenSpaceDemand
               PossibleProximateOpenSpace AccessibleOpenSpace OpenSpaceProximateHomeowners
               AccessibleProximity EnjoyedOpenSpace BlockingProximitySink
               HomeownersWithProximateOpenSpace UnaccessedOpenSpace InaccessibleProximitySink
               HomeownersWithoutProximateOpenSpace BlockedProximity BlockedOpenSpace
               HomeownersWithBlockedProximity)
        :context (source homeowners sink)))
