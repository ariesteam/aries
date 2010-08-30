(ns core.models.aesthetics
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
                  [1000 2750]  'aestheticService:SmallMountain  
                  [2750 8850]  'aestheticService:LargeMountain ;; no higher than Mt. Everest, catches artifacts
                  :otherwise   'aestheticService:NoMountain))  ;; catches low artifacts

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
            :import   "aries.core::ViewSource.xdsl"
            :context  (mountain lake ocean)
            :observed (theoretical-beauty)
            :keep     ('aestheticService:TheoreticalNaturalBeauty)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

;; TODO errors
(defmodel clearcut 'aestheticService:Clearcuts 
  (classification (binary-coding 'geofeatures:Clearcut)
                  0          'aestheticService:ClearcutsAbsent
                  :otherwise 'aestheticService:ClearcutsPresent))

; use NLCD layers to extract transportation infrastructure
(defmodel commercial-transportation 'aestheticService:CommercialIndustrialTransportation 
  (classification (numeric-coding 'nlcd:NLCDNumeric)
                  23         'aestheticService:TransportationInfrastructurePresent
                  :otherwise 'aestheticService:TransportationInfrastructureAbsent))

; presence/absence of highways
(defmodel highway 'aestheticService:Highways 
  (classification (binary-coding 'infrastructure:Highway)
                  0          'aestheticService:HighwaysAbsent
                  :otherwise 'aestheticService:HighwaysPresent))

(defmodel view-sink-undiscretizer 'aestheticService:VisualBlight
  (classification 'aestheticService:VisualBlight
                  [0    5]  'aestheticService:NoBlight 
                  [5   25]  'aestheticService:LowBlight 
                  [25  50]  'aestheticService:ModerateBlight 
                  [50 100]  'aestheticService:HighBlight))

(defmodel sink 'aestheticService:ViewSink
  "Landscape features that reduce the quality and enjoyment of scenic views"
  (bayesian 'aestheticService:ViewSink 
            :import  "aries.core::ViewSink.xdsl"
            :context (commercial-transportation highway)
            :observed (view-sink-undiscretizer)
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

;;undiscretizer for view use
(defmodel view-use-undiscretizer 'aestheticService:HomeownerViewUse
  (classification 'aestheticService:HomeownerViewUse
                  [0 5]   'aestheticService:HomeownerViewUseAbsent 
                  [5 100] 'aestheticService:HomeownerViewUsePresent))

;; bayesian model
(defmodel homeowners 'aestheticService:ViewUse
  "Property owners who can afford to pay for the view"
  (bayesian 'aestheticService:ViewUse 
            :import  "aries.core::ViewUse.xdsl"
            :context (property-value housing)
            :observed (view-use-undiscretizer) 
            :keep    ('aestheticService:HomeownerViewUse)))

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
        :keep ('aestheticService:PotentialViews 'aestheticService:PotentialVisualBlight 'aestheticService:HomeownersWithViewDemand
               'aestheticService:PossibleViews 'aestheticService:VisibleNaturalBeauty 'aestheticService:HomeownersWithPossibleViews
               'aestheticService:ActualViews 'aestheticService:EnjoyedViews 'aestheticService:RelevantVisualBlight
               'aestheticService:HomeownersWithViews 'aestheticService:UnseenViews 'aestheticService:InaccessibleVisualBlight
               'aestheticService:HomeownersWithoutViews 'aestheticService:BlockedViews 'aestheticService:InvisibleNaturalBeauty
               'aestheticService:HomeownersWithDegradedViews)
        :context (source
                  homeowners
                  sink
                  altitude
                  (ranking 'eserv:SourceThreshold :value 50 :min 0 :max 100)
                  (ranking 'eserv:SinkThreshold :value 0.3 :min 0 :max 1)
                  (ranking 'eserv:UseThreshold :value 0.1 :min 0 :max 1)
                  (ranking 'eserv:TransitionThreshold :value 1.0))))
