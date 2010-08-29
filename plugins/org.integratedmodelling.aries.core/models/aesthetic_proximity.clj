;;This is the proximity model for Western Washington; it's unchanged from the view model but contains
;; the correct flow concepts in the SPAN statement for proximity, so use as a starting point to build
;; the Western Washington proximity model.
(ns core.models.aesthetic-proximity
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario
                           defmodel
                           measurement
                           classification
                           categorization
                           ranking
                           numeric-coding
                           binary-coding
                           identification
                           bayesian
                           count])
  (:refer aries :only [span]))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

;;Remove ocean; add riparian corridor to source model
(defmodel lake 'aestheticService:Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (binary-coding 'geofeatures:Lake)
                  0          'aestheticService:LakeAbsent
                  :otherwise 'aestheticService:LakePresent))

(defmodel ocean 'aestheticService:Ocean
  "Just being there."
  (classification (binary-coding 'geofeatures:Ocean)
                  0          'aestheticService:OceanAbsent
                  :otherwise 'aestheticService:OceanPresent))

(defmodel mountain 'aestheticService:Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement 'geophysics:Altitude "m")
                  [1000 2750]  'aestheticService:SmallMountain ; 
                  [2750 8850]  'aestheticService:LargeMountain ; no higher than mount Everest!
                  :otherwise   'aestheticService:NoMountain)) ; will catch artifacts too

(defmodel theoretical-beauty 'aestheticService:TheoreticalNaturalBeauty
  (classification 'aestheticService:TheoreticalNaturalBeauty
                  [0   25] 'aestheticService:NoNaturalBeauty 
                  [25  50] 'aestheticService:LowNaturalBeauty 
                  [50  75] 'aestheticService:ModerateNaturalBeauty 
                  [75 100] 'aestheticService:HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticEnjoymentProvision 
            :import   "aries.core::ProximitySource.xdsl"
            :context  (mountain lake ocean)
            :observed (theoretical-beauty)
            :keep     ('aestheticService:TheoreticalNaturalBeauty)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel highway 'aestheticService:Highways 
  (classification (binary-coding 'infrastructure:Highway)
                  0          'aestheticService:HighwaysAbsent
                  :otherwise 'aestheticService:HighwaysPresent))

;;No BN needed here.  In the presence of highways, set the sink value to 90, otherwise set to 0 (ASK FERD).

(defmodel sink 'aestheticService:ViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian 'aestheticService:ViewSink 
            :import  "aries.core::ProximitySink.xdsl"
            :context (commercial-transportation highway)
            :keep    ('aestheticService:TotalVisualBlight)))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

(defmodel housing 'aestheticService:PresenceOfHousing
  "Classifies land use from property data."
  ;; specific to Puget region, will not be used if data unavailable
  (classification (categorization 'puget:ParcelUseCategoryKing)
                  #{"R" "K"}  'aestheticService:HousingPresent
                  :otherwise  'aestheticService:HousingAbsent))
;; TODO bring these back when the flow model runs at acceptable speeds.
;;  (classification (categorization 'puget:ParcelUseCategoryGraysHarbor)
;;		"RESIDENTIAL" 'aestheticService:HousingPresent
;;		:otherwise    'aestheticService:HousingAbsent)

(defmodel property-value 'aestheticService:HousingValue
  ;; TODO we need this to become an actual valuation with currency and date, so we can 
  ;; turn any values into these dollars
  (classification (ranking  'economics:AppraisedPropertyValue)
                  [:<       100000] 'aestheticService:VeryLowHousingValue
                  [100000   200000] 'aestheticService:LowHousingValue
                  [200000   400000] 'aestheticService:ModerateHousingValue
                  [400000  1000000] 'aestheticService:HighHousingValue
                  [1000000 :>]      'aestheticService:VeryHighHousingValue))

;;Defmodel statement for urban proximity: get Census population density into geoserver, wfs2opal, xml
;; ("PersonsKm2" field)
;;(defmodel urban-proximity 'aestheticService:UrbanProximity
;;  (classification (ranking 'concept)
;;                  [309 :>] 'aestheticService:Urban
;;                  [77 309] 'aestheticService:Suburban
;;                  [77 :>]  'aestheticService:Rural)) 

;;NEED AN UNDISCRETIZATION STATEMENT: ASK FERD

;; bayesian model
;;(defmodel homeowners 'aestheticService:ViewUse
;;  "Property owners who can afford to pay for proximity to open space"
;;  (bayesian 'aestheticService:ViewUse 
;;            :import  "aries.core::ProximityUse.xdsl"
;;            :context (property-value urban-proximity housing)
;;            :keep    ('aestheticService:HomeownerViewUse)))

;; ----------------------------------------------------------------------------------------------
;; dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude 'geophysics:Altitude
  (measurement 'geophysics:Altitude "m"))	 								

;; ---------------------------------------------------------------------------------------------------	 	 	
;; overall models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
;;(defmodel data 'aestheticService:AestheticEnjoyment 
;;  (identification 'aestheticService:AestheticEnjoyment
(defmodel data 'aestheticService:LineOfSight
  (identification 'aestheticService:LineOfSight
                  :context (source :as source
                                   homeowners :as use
                                   sink       :as sink
                                   altitude   :as altitude)))

;; the real enchilada - need to be updated to the latest SPAN language
(defmodel view 'aestheticService:AestheticView
  (span 'aestheticService:LineOfSight 
        'aestheticService:TheoreticalNaturalBeauty
        'aestheticService:HomeownerViewUse
      	'aestheticService:TotalVisualBlight
      	nil
        'geophysics:Altitude
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
