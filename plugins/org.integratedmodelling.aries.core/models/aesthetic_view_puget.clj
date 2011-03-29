(ns core.models.aesthetics
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario namespace-ontology
                           defmodel measurement classification categorization ranking numeric-coding
                           probabilistic-measurement probabilistic-classification probabilistic-ranking
                           binary-coding identification bayesian count])
  (:refer aries :only [span]))

(namespace-ontology aestheticService)

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

(defmodel lake Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (binary-coding geofeatures:Lake)
                  1          LakePresent
                  :otherwise LakeAbsent))

(defmodel ocean Ocean
  "Just being there."
  (classification (binary-coding geofeatures:Ocean)
                  1          OceanPresent
                  :otherwise OceanAbsent))

(defmodel mountain Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement geophysics:Altitude "m")
                  [1000 2500]  SmallMountain  
                  [2500 8850]  LargeMountain ;; no higher than Mt. Everest, catches artifacts
                  :otherwise   NoMountain))  ;; catches low artifacts

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
                  [0   5]  NoNaturalBeauty 
                  [5  25]  LowNaturalBeauty 
                  [25  50] ModerateNaturalBeauty 
                  [50 100] HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source AestheticViewProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian AestheticViewProvision 
            :import   "aries.core::ViewSource.xdsl"
            :context  (mountain lake ocean)
            :result   theoretical-beauty
            :keep     (TheoreticalNaturalBeauty)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel clearcut Clearcuts 
  (classification (categorization geofeatures:Clearcut)
                  #{"EVEN-AGE" "EVEN R/W" "EVEN/SALVAGE"}   ClearcutsPresent
                  :otherwise                                ClearcutsAbsent))

; NLCD 1992 for Commercial/Industrial/Transportation land use
(defmodel commercial-transportation CommercialIndustrialTransportation 
  (classification (numeric-coding nlcd:NLCD1992Typology)
                  23         TransportationInfrastructurePresent
                  :otherwise TransportationInfrastructureAbsent))

(defmodel highway Highways 
  (classification (binary-coding infrastructure:Highway)
                  1          HighwaysPresent
                  :otherwise HighwaysAbsent))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
                  [0    5]  NoBlight 
                  [5   25]  LowBlight 
                  [25  50]  ModerateBlight 
                  [50 100]  HighBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality and enjoyment of scenic views"
  (bayesian ViewSink 
            :import  "aries.core::ViewSink.xdsl"
            :context (commercial-transportation clearcut highway)
            :result  view-sink-undiscretizer
            :keep    (VisualBlight)))

;; ----------------------------------------------------------------------------------------------
;; Use models
;; ----------------------------------------------------------------------------------------------

(defmodel housing PresenceOfHousing
  "Classifies land use from property data."
  (classification (ranking PresenceOfHousing)
        [1 :>]               HousingPresent  
        :otherwise           HousingAbsent))
;;  (classification (numeric-coding nlcd:NLCDNumeric) ;;Using NLCD where parcel data are unavailable.
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

;;Training data for King County: actual housing with views.  
;; Do not use until BNs can be properly trained.
(defmodel view-use-king HomeownerViewUse
   (classification (binary-coding HomeownerViewUse)
                  [0 5]   HomeownerViewUsePresent  ;;CHANGE TO "IF ZERO OR GREATER" view use present, otherwise not.
                  [5 100] HomeownerViewUseAbsent)) 

;;undiscretizer for view use
(defmodel view-use-undiscretizer HomeownerViewUse
  (probabilistic-ranking HomeownerViewUse
                  [0 0.05]   HomeownerViewUseAbsent 
                  [0.05 1]   HomeownerViewUsePresent))

;; bayesian model
(defmodel homeowners ViewUse
  "Property owners who can afford to pay for the view"
  (bayesian ViewUse 
            :import  "aries.core::ViewUse.xdsl"
            :context (property-value housing)
            :result  view-use-undiscretizer 
            :keep    (HomeownerViewUse)))

;;Scenic highways as another beneficiary class - i.e., their drivers benefit from views along highways.
(defmodel scenic-highways ScenicDrivePresence
  (classification (binary-coding ScenicDrives)
                1             ScenicDrivesPresent
                :otherwise    ScenicDrivesAbsent))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))	 								

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data LineOfSight
  (identification LineOfSight
                  :context (source :as source
                                   homeowners :as use
                                   ;;scenic-highways :as use (once wfs is working)
                                   sink       :as sink
                                   altitude   :as altitude)))

(defmodel view AestheticView
  (span LineOfSight 
        TheoreticalNaturalBeauty
        HomeownerViewUse
        VisualBlight
        nil
        (geophysics:Altitude)
        :source-threshold   4.0  ;;Initially set within the lowest bin
        :sink-threshold     4.0  ;;Initially set within the lowest bin
        :use-threshold      4.0  ;;Initially set within the lowest bin
        :trans-threshold    4.0  ;;Set just below the "no use" threshold in the use model; run sensitivity analysis on this
        :source-type      :infinite
        :sink-type        :infinite
        :use-type         :infinite
        :benefit-type     :non-rival
        :downscaling-factor 2
        :rv-max-states      10
        :keep (PotentialViews PotentialVisualBlight HomeownersWithViewDemand
               PossibleViews VisibleNaturalBeauty HomeownersWithPossibleViews
               ActualViews EnjoyedViews RelevantVisualBlight
               HomeownersWithViews UnseenViews InaccessibleVisualBlight
               HomeownersWithoutViews BlockedViews DegradedNaturalBeauty
               HomeownersWithDegradedViews)
        :context (source homeowners sink altitude)))

;;Develop another one of these to account for scenic drives.
