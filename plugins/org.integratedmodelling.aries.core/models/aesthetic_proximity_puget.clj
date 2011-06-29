;;Proximity model for Western Washington
(ns core.models.aesthetic-proximity-puget
  (:refer-clojure :rename {count length})
  (:refer tl :only [is? conc])
  (:refer modelling :only [defscenario defmodel measurement classification categorization 
                           namespace-ontology ranking model
                           probabilistic-measurement probabilistic-classification probabilistic-ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService
  (thinklab-core:BooleanRanking
        (LandOrSea
            (OnLand) (NotOnLand))))

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
                  [0   5]  NoProximityPotential 
                  [5  25]  LowProximityPotential
                  [25  50] ModerateProximityPotential 
                  [50 100] HighProximityPotential))

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

(defmodel highway infrastructure:Highway
  (binary-coding infrastructure:Highway))

(defmodel sink ProximitySink
  (ranking ProximitySink
           :context (highway)
           :state #(cond (== (:highway %) 1) 50   ;;50 units of proximity value are depleted by the sink if highways are present
                         :otherwise          0))) ;;Otherwise zero sink

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))  

;; Used to mask out ocean (elevation = 0)
(defmodel land-selector LandOrSea
    (classification  (measurement geophysics:Altitude "m")
       [:exclusive 0 :>] OnLand))

(defmodel housing PresenceOfHousing
  (classification (ranking PresenceOfHousing)
        [1 :>]       HousingPresent  
        :otherwise   HousingAbsent)
  (classification (numeric-coding nlcd:NLCDNumeric) ;;Using NLCD where parcel data are unavailable.
        [22 23 24]   HousingPresent  ;;Assumes (incorrectly) that all developed land is housing.
        :otherwise   HousingAbsent))

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
            :context (property-value urban-proximity housing land-selector)
            :required (LandOrSea)
            :result  proximity-use-undiscretizer 
            :keep    (HomeownerProximityUse)))

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data Proximity
  (identification Proximity
                  :context (source homeowners sink)))

(defmodel proximity AestheticProximity
  (span Proximity
        AestheticProximityProvision
        ProximityUse
        ProximitySink
        nil
        nil
        :source-threshold   40.0  ;; Excludes LowProximityPotential
        :sink-threshold     0.0   ;; Deterministic as 0.0 or 50.0 based on presence of highways
        :use-threshold      0.2   ;; Excludes HomeownerProximityUseAbsent
        :trans-threshold    1.0
        :source-type        :infinite
        :sink-type          :infinite
        :use-type           :infinite
        :benefit-type       :non-rival
        :downscaling-factor 1
        :rv-max-states      10
        :animation?         false
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_proximity_puget_data.clj")
        :keep (PotentialProximateOpenSpace
               PotentialProximitySink
               HomeownersWithOpenSpaceDemand
               PossibleProximateOpenSpace
               AccessibleOpenSpace
               OpenSpaceProximateHomeowners
               AccessibleProximity
               EnjoyedOpenSpace
               BlockingProximitySink
               HomeownersWithProximateOpenSpace
               UnaccessedOpenSpace
               InaccessibleProximitySink
               HomeownersWithoutProximateOpenSpace
               BlockedProximity
               BlockedOpenSpace
               HomeownersWithBlockedProximity)
        :context (source homeowners sink)))

;; ----------------------------------------------------------------------------------------------
;; Scenarios 

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

(defmodel constrained-development-scenario puget:ConstrainedDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCConstrained2060) 
      4                                     puget:HighDensityDevelopedConstrained
      6                                     puget:ModerateDensityDevelopedConstrained
      5                                     puget:LowDensityDevelopedConstrained
      7                                     puget:UrbanOpenSpaceConstrained
      #{0 1 2 3 8 9 10 11 12 13 14 15 16}   puget:NotDevelopedConstrained))

(defmodel open-development-scenario puget:OpenDevelopment
  (classification (numeric-coding puget:ENVISIONUrbanGrowthLULCOpen2060) 
      4                                     puget:HighDensityDevelopedOpen
      6                                     puget:ModerateDensityDevelopedOpen
      5                                     puget:LowDensityDevelopedOpen
      7                                     puget:UrbanOpenSpaceOpen
      #{0 1 2 3 8 9 10 11 12 13 14 15 16}   puget:NotDevelopedOpen))

(defscenario open-development-proximity
  "Changes values in developed areas to no valuable open space type, moderate housing value present."
  (model Forest
    (classification Forest
        :context (open-development-scenario forest)
        :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:ForestAbsent)
                  (:forest %))))
  (model Farmland
    (classification Farmland
        :context (open-development-scenario farmland)
        :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:FarmlandAbsent) 
                  (:farmland %))))
  (model WoodyWetland
    (classification WoodyWetland
        :context (open-development-scenario woody-wetland)
        :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:WoodyWetlandAbsent)
                  (:woody-wetland %))))
  (model EmergentWetland
    (classification EmergentWetland
        :context (open-development-scenario emergent-wetland)
        :state #(if (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                        (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen)))
                  (conc 'aestheticService:EmergentWetlandAbsent)
                  (:emergent-wetland %))))
  (model Park
    (classification Park
        :context (open-development-scenario park)
        :state #(cond (or (is? (:open-development %) (conc 'puget:HighDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:ModerateDensityDevelopedOpen))
                          (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen)))
                      (conc 'aestheticService:ParkAbsent)
                      
                      (is? (:open-development %) (conc 'puget:UrbanOpenSpaceOpen))
                      (conc 'aestheticService:ParkPresent)

                      :otherwise (:park %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
        :context (open-development-scenario housing)
        :state #(if (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                  (conc 'aestheticService:HousingPresent)           
                  (:presence-of-housing %))))
  (model HousingValue
    (classification HousingValue
        :context (open-development-scenario property-value)
        :state #(if (is? (:open-development %) (conc 'puget:LowDensityDevelopedOpen))
                  (conc 'aestheticService:ModerateHousingValue)           
                  (:housing-value %)))))

(defscenario constrained-development-proximity
  "Changes values in developed areas to no valuable open space type, moderate housing value present."
  (model Forest
    (classification Forest
        :context (constrained-development-scenario forest)
        :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:ForestAbsent)
                  (:forest %))))
  (model Farmland
    (classification Farmland
        :context (constrained-development-scenario farmland)
        :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:FarmlandAbsent)
                  (:farmland %))))
  (model WoodyWetland
    (classification WoodyWetland
        :context (constrained-development-scenario woody-wetland)
        :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:WoodyWetlandAbsent)
                  (:woody-wetland %))))
  (model EmergentWetland
    (classification EmergentWetland
        :context (constrained-development-scenario emergent-wetland)
        :state #(if (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrainedConstrained))
                        (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                        (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained)))
                  (conc 'aestheticService:EmergentWetlandAbsent)
                  (:emergent-wetland %))))
  (model Park
    (classification Park
        :context (constrained-development-scenario park)
        :state #(cond (or (is? (:constrained-development %) (conc 'puget:HighDensityDevelopedConstrained))
                          (is? (:constrained-development %) (conc 'puget:ModerateDensityDevelopedConstrained))
                          (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained)))
                      (conc 'aestheticService:ParkAbsent)
                      
                      (is? (:constrained-development %) (conc 'puget:UrbanOpenSpaceConstrained))
                      (conc 'aestheticService:ParkPresent)

                  :otherwise (:park %))))
  (model PresenceOfHousing
    (classification PresenceOfHousing
        :context (constrained-development-scenario housing)
        :state #(if (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                  (conc 'aestheticService:HousingPresent)           
                  (:presence-of-housing %))))
  (model HousingValue
    (classification HousingValue
        :context (constrained-development-scenario property-value)
        :state #(if (is? (:constrained-development %) (conc 'puget:LowDensityDevelopedConstrained))
                  (conc 'aestheticService:ModerateHousingValue)           
                  (:housing-value %)))))