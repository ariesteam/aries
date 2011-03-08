(ns core.models.aesthetic-view-san-pedro
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

(defmodel mountain Mountain
  (classification (measurement geophysics:Altitude "m")
                  [1400 1800]  SmallMountain  
                  [1800 8850]  LargeMountain ;; no higher than Mt. Everest, catches artifacts
                  :otherwise   NoMountain))  ;; catches low artifacts

(defmodel scenic-vegetation sanPedro:ScenicVegetationType
;;  [(categorization geofeatures:Country :as country)]
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC) 
;;                  :when #(= (:country %) "United States")
                  #{1 2 3 4 5 6 7 8 9 15 39 69 70 71 86 89}               sanPedro:AlpineAndCliff
                  #{22 23 33 37 38 91}                                    sanPedro:Forests
                  #{34 35 36 41 42 44 46 63 64 92 95 100 101 102 103 109} sanPedro:Woodland ;; includes pinon & juniper savannas
                  #{77 78 79 80 81 83 84 85 98 109 110 118}               sanPedro:RiparianAndWater
                  :otherwise                                              sanPedro:Other)
  (classification (categorization mexico:CONABIOLULCCategory)
                  #{"Bosque de coniferas distintas a Pinus" "Bosque de encino" "Bosque de pino"} sanPedro:Forests
                  #{"Vegetacion de galeria"}                                                     sanPedro:Woodland
                  #{"Cuerpos de agua"}                                                           sanPedro:RiparianAndWater
                  :otherwise                                                                     sanPedro:Other))

(defmodel theoretical-beauty TheoreticalNaturalBeauty
  (probabilistic-ranking TheoreticalNaturalBeauty
                  [0   10] NoNaturalBeauty 
                  [10  25] LowNaturalBeauty 
                  [25  50] ModerateNaturalBeauty 
                  [50 100] HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source AestheticViewProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian AestheticViewProvision 
            :import   "aries.core::ViewSourceSanPedro.xdsl"
            :context  (mountain scenic-vegetation)
            :result   theoretical-beauty
            :keep     (TheoreticalNaturalBeauty)))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------

(defmodel mine Mines                         
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)         
                  #{19 117}       MinesPresent
                  :otherwise      MinesAbsent))

(defmodel transmission-line TransmissionLines 
  (classification (binary-coding infrastructure:TransmissionLine)
                  1          TransmissionLinesPresent
                  :otherwise TransmissionLinesAbsent))

(defmodel highway Highways 
  (classification (binary-coding infrastructure:Highway)
                  1          HighwaysPresent
                  :otherwise HighwaysAbsent))

;; Insert correct concepts for Mexico
(defmodel developed-land DevelopedLand
  (classification (numeric-coding sanPedro:SouthwestRegionalGapAnalysisLULC)           
                  111        LowDensityDevelopment
                  112        HighDensityDevelopment
                  :otherwise NoDevelopment))

(defmodel view-sink-undiscretizer VisualBlight
  (probabilistic-ranking VisualBlight
                  [0    5]  NoBlight 
                  [5   25]  LowBlight 
                  [25  50]  ModerateBlight 
                  [50 100]  HighBlight))

(defmodel sink ViewSink
  "Landscape features that reduce the quality and enjoyment of scenic views"
  (bayesian ViewSink 
            :import  "aries.core::ViewSinkSanPedro.xdsl"
            :context  (mine highway transmission-line developed-land)
            :result   view-sink-undiscretizer
            :keep     (VisualBlight)))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------

(defmodel housing PresenceOfHousing
  (classification (ranking economics:AppraisedPropertyValue)
        [1 :>]       HousingPresent
        :otherwise   HousingAbsent)
  (classification (numeric-coding nlcd:NLCDNumeric) ;;Using NLCD where parcel data are unavailable.
        [22 23 24]   HousingPresent  ;;Assumes (incorrectly) that all developed land is housing.
        :otherwise   HousingAbsent))

(defmodel property-value HousingValue  ;; value is in $/ac, which is not a legitimate unit in thinklab, so kept as a ranking for now.
  (classification (ranking economics:AppraisedPropertyValue)
                  [0         10000] VeryLowHousingValue
                  [10000    25000]  LowHousingValue
                  [25000   50000]   ModerateHousingValue
                  [50000  200000]   HighHousingValue
                  [200000 :>]       VeryHighHousingValue))

;;Scenic highways as another beneficiary class - i.e., their drivers benefit from views along highways.
(defmodel scenic-highways ScenicDrivePresence
  (classification (binary-coding ScenicDrives)
                1             ScenicDrivesPresent
                :otherwise    ScenicDrivesAbsent))

;;undiscretizer for view use
;;  This needs to be a range (high-mod-low)
(defmodel view-use-undiscretizer HomeownerViewUse
  (probabilistic-ranking HomeownerViewUse
                  [0 0.05]   HomeownerViewUseAbsent 
                  [0.05 1]   HomeownerViewUsePresent))

;; bayesian model
(defmodel homeowners ViewUse
  "Property owners who can afford to pay for the view"
  (bayesian ViewUse 
            :import  "aries.core::ViewUseSanPedro.xdsl"
            :context (housing property-value) 
            :result  view-use-undiscretizer
            :keep    (HomeownerViewUse)))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------

(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))	 								

;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data-homeowners LineOfSight
  (identification LineOfSight
                  :context (source          :as source
                            homeowners      :as use 
                            sink            :as sink
                            altitude        :as altitude)))

(defmodel data-bsr LineOfSight
  (identification LineOfSight
                  :context (source          :as source
                            sink            :as sink)))

(defmodel data-scenic-highways LineOfSight
  (identification LineOfSight
                  :context (source          :as source
                            scenic-highways :as use  
                            sink            :as sink
                            altitude        :as altitude)))

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
        ;;:downscaling-factor 2
        :downscaling-factor 1
        :rv-max-states      10
        :keep (PotentialViews PotentialVisualBlight HomeownersWithViewDemand
               PossibleViews VisibleNaturalBeauty HomeownersWithPossibleViews
               ActualViews EnjoyedViews RelevantVisualBlight
               HomeownersWithViews UnseenViews InaccessibleVisualBlight
               HomeownersWithoutViews BlockedViews DegradedNaturalBeauty
               HomeownersWithDegradedViews)
        ;;:save-file          "/home/gjohnson/code/java/imt/identifications/aesthetic_view_san_pedro_data.clj"
        :context (source homeowners sink altitude)))

;;Develop another one of these to account for scenic drives.

;; ----------------------------------------------------------------------------------------------
;; Scenarios

;; Observations that are specifically tagged for a scenario will be picked up automatically
;; instead of the baseline ones.
;; ----------------------------------------------------------------------------------------------

;;(defscenario mesquite-management sanPedro:MesquiteManagement): change scenic-vegetation to sanPedro:Other within polygons in the source model.
      
;;(defscenario urban-growth sanPedro:UrbanGrowth: change scenic-vegetation to sanPedro:Other within polygons in the source model.  
;;  Add new developed-land as aestheticService:LowDensityDevelopment in the sink model.  Add more users in the developed areas in the use model.
      ;;sanPedro:UrbanGrowth2020Open
      ;;sanPedro:UrbanGrowth2020Constrained
      
;;(defscenario bsr-development sanPedro:BSRDevelopment: change scenic-vegetation to sanPedro:Other within polygons in the source model.
;;  Add new developed-land as aestheticService:LowDensityDevelopment in the sink model.  Add more users in the developed areas in the use model.
      ;;sanPedro:BSRDevelopmentSite1
      ;;sanPedro:BSRDevelopmentSite2
      ;;sanPedro:BSRDevelopmentSite3
      ;;sanPedro:BSRDevelopmentSite4
      ;;sanPedro:BSRDevelopmentSite5
