(ns core.models.aesthetic-view-san-pedro
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defscenario defmodel measurement classification categorization ranking
                           numeric-coding binary-coding identification bayesian count])
  (:refer aries :only [span]))

;; ----------------------------------------------------------------------------------------------
;; source model
;; ----------------------------------------------------------------------------------------------

(defmodel mountain 'aestheticService:Mountain
  (classification (measurement 'geophysics:Altitude "m")
                  [1400 1800]  'aestheticService:SmallMountain  
                  [1800 8850]  'aestheticService:LargeMountain ;; no higher than Mt. Everest, catches artifacts
                  :otherwise   'aestheticService:NoMountain))  ;; catches low artifacts

(defmodel scenic-vegetation 'sanPedro:ScenicVegetationType
  [(categorization 'geofeatures:Country :as country)]
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC) 
                  :when #(= (:country %) "United States")
                  #{1 2 3 4 5 6 7 8 9 15 39 69 70 71 86 89}               'sanPedro:AlpineAndCliff
                  #{22 23 33 37 38 91}                                    'sanPedro:Forest
                  #{34 35 36 41 42 44 46 63 64 92 95 100 101 102 103 109} 'sanPedro:Woodland ;; includes pinon & juniper savannas
                  #{76 81 83 84 85 98 118}                                'sanPedro:RiparianAndWater
                  :otherwise                                              'sanPedro:Other)
  (classification (categorization 'mexico:CONABIOLULCCategory)
                  #{"Bosque de coniferas distintas a Pinus" "Bosque de encino" "Bosque de pino"} 'sanPedro:Forest
                  #{"Vegetacion de galeria"}                                                     'sanPedro:Woodland
                  #{"Cuerpos de agua"}                                                           'sanPedro:RiparianAndWater
                  :otherwise                                                                     'sanPedro:Other))

(defmodel theoretical-beauty 'aestheticService:TheoreticalNaturalBeauty
  (classification 'aestheticService:TheoreticalNaturalBeauty
                  [0    5] 'aestheticService:NoNaturalBeauty 
                  [5   25] 'aestheticService:LowNaturalBeauty 
                  [25  50] 'aestheticService:ModerateNaturalBeauty 
                  [50 100] 'aestheticService:HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source 'aestheticService:AestheticViewProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian 'aestheticService:AestheticViewProvision 
            :import   "aries.core::ViewSourceSanPedro.xdsl"
            :context  (mountain scenic-vegetation)
            :observed (theoretical-beauty)
            :keep     ('aestheticService:TheoreticalNaturalBeauty)))

;; ----------------------------------------------------------------------------------------------
;; sink model
;; ----------------------------------------------------------------------------------------------

(defmodel mine 'aestheticService:Mines                         
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)         
                  #{19 117}       'aestheticService:MinesPresent
                  :otherwise      'aestheticService:MinesAbsent))

;; Run model and figure this out. It seems backwards
(defmodel transmission-line 'aestheticService:TransmissionLines 
  (classification (binary-coding 'infrastructure:TransmissionLine)
                  0          'aestheticService:TransmissionLinesPresent
                  :otherwise 'aestheticService:TransmissionLinesAbsent))

(defmodel highway 'aestheticService:Highways 
  (classification (binary-coding 'infrastructure:Highway)
                  0          'aestheticService:HighwaysAbsent
                  :otherwise 'aestheticService:HighwaysPresent))

;; Insert correct concepts for Mexico
(defmodel developed-land 'aestheticService:DevelopedLand
  (classification (numeric-coding 'sanPedro:SouthwestRegionalGapAnalysisLULC)           
                  111        'aestheticService:LowDensityDevelopment
                  112        'aestheticService:HighDensityDevelopment
                  :otherwise 'aestheticService:NoDevelopment))

(defmodel view-sink-undiscretizer 'aestheticService:VisualBlight
  (classification 'aestheticService:VisualBlight
                  [0    5]  'aestheticService:NoBlight 
                  [5   25]  'aestheticService:LowBlight 
                  [25  50]  'aestheticService:ModerateBlight 
                  [50 100]  'aestheticService:HighBlight))

(defmodel sink 'aestheticService:ViewSink
  "Landscape features that reduce the quality and enjoyment of scenic views"
  (bayesian 'aestheticService:ViewSink 
            :import  "aries.core::ViewSinkSanPedro.xdsl"
            :context  (mine highway transmission-line developed-land)
            :observed (view-sink-undiscretizer) 
            :keep     ('aestheticService:TotalVisualBlight)))

;; ----------------------------------------------------------------------------------------------
;; use model
;; ----------------------------------------------------------------------------------------------

;;UPDATE THIS ONCE FULL PARCEL DATA IS AVAILABLE
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

;;Define conditions under which these are present - ideally get a layer, or could develop one by hand.  Relevant highways in the San
;; Pedro Valley include I-10 from Tucson to Benson, Arizona Highways 80 & 82.
;;(defmodel scenic-highways 'aestheticService:ScenicDrives
;;  (classification (binary-coding 'infrastructure:Highway)
;;                        'aestheticService:ScenicDrivesPresent
;;                        'aestheticService:ScenicDrivesAbsent))

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

;;Develop another one of these to account for scenic drives.
