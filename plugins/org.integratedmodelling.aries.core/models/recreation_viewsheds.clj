(ns core.models.recreation-viewsheds
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defscenario defmodel measurement classification categorization 
                            namespace-ontology ranking numeric-coding binary-coding
                            probabilistic-measurement probabilistic-classification probabilistic-ranking 
                            identification bayesian count))
  (:refer aries :only (span)))

(namespace-ontology recreationService
  (representation:GenericObservable
    (Lake
      (LakeAbsent)
      (LakePresent))
    (Mountain
      (NoMountain)
      (SmallMountain)
      (LargeMountain))))

;; ----------------------------------------------------------------------------------------------
;; Source model
;; ----------------------------------------------------------------------------------------------

;;Need a new defmodel statement combining lake & river into "waterview," with rivers as low quality and
;; lakes as high.
(defmodel lake Lake
  "Just being a lake. We may want to reclass lake area instead"
  (classification (binary-coding geofeatures:Lake)
		  0          LakeAbsent
		  :otherwise LakePresent))

(defmodel river-stream RiverStream
  "Presence of a river or stream."
  (classification (binary-coding geofeatures:River)
		  0          RiverStreamAbsent
		  :otherwise RiverStreamPresent))

(defmodel mountain Mountain
  "Classifies an elevation model into three levels of provision of beautiful mountains"
  (classification (measurement geophysics:Altitude "m")
		  [457 914]  SmallMountain ; 
		  [914 8850] LargeMountain ;; no higher than Mt. Everest, catches artifacts
		  :otherwise NoMountain)) ; will catch artifacts too		  
		  
(defmodel open-space OpenSpaceClass
  "Classifies an area as open space according to NLCD 2001 data"
  (classification (numeric-coding nlcd:NLCDNumeric)
      #{81 82}       AgriculturalLand
      #{41 42 43}    ForestedLand
      #{31 90 95 52} OtherOpenLand
      :otherwise     NotOpenLand))

(defmodel theoretical-beauty aestheticService:TheoreticalNaturalBeauty
	(probabilistic-ranking aestheticService:TheoreticalNaturalBeauty
  		[0 25]   aestheticService:NoNaturalBeauty 
  		[25 50]  aestheticService:LowNaturalBeauty 
  		[50 75]  aestheticService:ModerateNaturalBeauty 
  		[75 100] aestheticService:HighNaturalBeauty))

;; source bayesian model	    		 
(defmodel source aestheticService:AestheticEnjoymentProvision
  "This one will harmonize the context, then retrieve and run the BN with the given
   evidence, and produce a new observation with distributions for the requested nodes."
  (bayesian aestheticService:AestheticEnjoymentProvision 
    :import   "aries.core::RecreationViewSource.xdsl"
    :keep     (aestheticService:TheoreticalNaturalBeauty)
    :context  (lake river-stream mountain open-space)
    :result   theoretical-beauty))

;; ----------------------------------------------------------------------------------------------
;; Sink model
;; ----------------------------------------------------------------------------------------------
;;development, clearcuts, roads, energy infrastructure
(defmodel development Development
  "Development as defined by the NLCD 2001"
  (classification (numeric-coding nlcd:NLCDNumeric)
      22          LowIntensityDevelopment
      23          MediumIntensityDevelopment
      24          HighIntensityDevelopment
      :otherwise  NotDeveloped)) 

(defmodel transportation-energy-infrastructure-code TransportationEnergyInfrastructureCode
   (binary-coding TransportationEnergyInfrastructureCode
        :context ((binary-coding infrastructure:Road                  :as road)
                  (binary-coding infrastructure:EnergyInfrastructure  :as energy-infrastructure)) 
        :state   #(if (or (= (:road %) 1)
                          (= (:energy-infrastructure %) 1))
                      1
                      0))) 
(defmodel transportation-energy-infrastructure TransportationEnergyInfrastructure
   (classification transportation-energy-infrastructure-code
             1  TransportationEnergyInfrastructurePresent 
             0  TransportationEnergyInfrastructureAbsent))                        

(defmodel visual-blight aestheticService:VisualBlight
  (probabilistic-ranking aestheticService:VisualBlight
      [0 10]   aestheticService:NoBlight
      [10 50]  aestheticService:LowBlight
      [50 90]  aestheticService:ModerateBlight
      [90 100] aestheticService:HighBlight))
      
(defmodel sink aestheticService:ViewSink
  "Whatever is ugly enough to absorb our enjoyment"
  (bayesian aestheticService:ViewSink 
    :import  "aries.core::RecreationViewSink.xdsl"
    :keep    (aestheticService:VisualBlight)
    :context (development transportation-energy-infrastructure)
    :result  visual-blight))

;; ----------------------------------------------------------------------------------------------
;; Use model
;; ----------------------------------------------------------------------------------------------
;; ViewPosition, TravelTime, PublicAccess, HikingDistance, HikingSlope

(defmodel view-position ViewPositionClass
  "Location of a view point, a function of elevation."
  (classification (measurement ViewPosition "m")
		  [0 457]   LowViewPosition
		  [457 914] MediumViewPosition
		  [914 :>]  HighViewPosition))
		  
(defmodel travel-time TravelTimeClass
	"Travel time to hiking resources"
	(classification (ranking TravelTime)
			1  ShortTravelTime
			2  ModerateTravelTime
			3  LongTravelTime))
			
(defmodel public-access PublicAccessClass
	"describes access constraints to a particular parcel"
	(classification (ranking PublicAccess)
		  0   PublicLand
		  1		PrivateLandWithAccess
		  2		NoPublicAccess)) 
	
(defmodel hiking-distance HikingDistanceClass
	"Refers to trail distance between the starting point and the view point"
	(classification (ranking HikingDistance)
			1   ShortHikingDistance
			2   ModerateHikingDistance
			3   LongHikingDistance))
	
(defmodel hiking-slope HikingSlopeClass
	"describes the steepness of the hiking trail"
	(classification (measurement HikingSlope "\u00b0")
			[:< 10] LowSlope
			[10 45] ModerateSlope
			[45 :>]	SteepSlope))
			
(defmodel viewer-enjoyment ViewerEnjoyment
	(probabilistic-ranking ViewerEnjoyment
  		[0 33]  LowViewerEnjoyment 
  		[33 67]  ModerateViewerEnjoyment 
  		[67 100] HighViewerEnjoyment))

;; bayesian model
(defmodel user ViewerEnjoyment
  "Views afforded to recreational users"
  (bayesian ViewerEnjoyment
    :import   "aries.core::RecreationViewUse.xdsl"
    :keep     (ViewerEnjoyment)
    :context  (view-position travel-time public-access hiking-distance hiking-slope)
    :result   viewer-enjoyment))

(defmodel population-density policytarget:PopulationDensity
  (count policytarget:PopulationDensity "/km^2"))

;; ----------------------------------------------------------------------------------------------
;; Dependencies for the flow model
;; ----------------------------------------------------------------------------------------------
 	 								
(defmodel altitude geophysics:Altitude
  (measurement geophysics:Altitude "m"))	 								
 
;; ---------------------------------------------------------------------------------------------------	 	 	
;; Top-level service models 
;; ---------------------------------------------------------------------------------------------------	 	 	

;; all data, for testing and storage
(defmodel data aestheticService:AestheticEnjoyment 
	(identification aestheticService:AestheticEnjoyment
		:context (
			source :as source
			user :as use
			sink :as sink
			altitude :as altitude)))

;; The "first stage" flow model calculates view quality from view points, and passes results to the recreation
;;   flow model, which moves people toward those points.	
;;(defmodel view aestheticService:AestheticView
;;  (span aestheticService:LineOfSight 
;;        aestheticService:AestheticViewProvision
;;        aestheticService:ViewUse
;;        aestheticService:ViewSink
;;        nil
;;        (geophysics:Altitude)
;;        :source-threshold   25.0  ;; Excludes LowNaturalBeauty
;;        :sink-threshold     25.0  ;; Excludes LowBlight
;;        :use-threshold       0.2  ;; Excludes HomeownerViewUseAbsent
;;        :trans-threshold     1.0
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 1
;;        :rv-max-states      10
;;        :animation?         true
        ;;:save-file          (str (System/getProperty "user.home") "/aesthetic_view_san_pedro_data.clj")
;;        :keep (aestheticService:ActualViews            aestheticService:EnjoyedViews
;;               aestheticService:RelevantVisualBlight   aestheticService:HomeownersWithViews)
;;        :context (source viewpoints sink altitude)))  ;;Need to create the viewpoints layer, showing mountain views.

;; The "second stage" flow model moves people to mountain summits with views.
;;(defmodel recreation-flow-mountain-view MountainViewUse
;;  (span MountainViewAccessAndUse
;;        MountainViewSourceValue
;;        MountainViewDemand        ;;Need to create this model
;;        nil
;;        nil
;;        nil                  ;;May need concepts here
;;        :source-threshold   10.0
;;        :sink-threshold     10.0
;;        :use-threshold      1.0
;;        :trans-threshold    nil
;;        :source-type        :infinite
;;        :sink-type          :infinite
;;        :use-type           :infinite
;;        :benefit-type       :non-rival
;;        :downscaling-factor 8
;;        :rv-max-states      10
;;        :animation?         true
;;        :save-file          (str (System/getProperty "user.home") "/recreation_san_pedro_data.clj")
;;        :keep (RecreationalAttractiveness                  PotentialRecreationalUsers
;;               RecreationalUserFlow                        RecreationalUse
;;               ActualRecreationalUsers                     TransportationRestrictedRecreationalUse
;;               TransportationRestrictedRecreationalUsers)
;;        :context (source-mountain-view population-density roads))) ;;replace with final use concept
