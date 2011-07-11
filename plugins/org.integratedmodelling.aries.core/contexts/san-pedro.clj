(ns core.contexts.san_pedro
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model transform])
  (:refer geospace :only [grid shape]))

;; --------------------------------------------------------------------------
;; Using a variable for the resolution, so you can change it here and it gets
;; changed in all contexts. 
;; ---------------------------------------------------------------------------
(def resolution 512)

;; -----------------------------------------------------------------------------
;; FV pre-defining the inline WKT-specified shapes as variables to keep the context
;; definition easier to edit and understand. 
;; This is optional and cosmetic. It allows to just use the vars
;; below instead of the (shape ...) forms and in the (grid ...) statements.
;; 
;; FV using inline specs even if the gazetteer works fine to avoid naming conflict
;; due to Ken using different templates when initializing the gazetteer, and to eliminate a variable in case of 
;; bugs.
;; -----------------------------------------------------------------------------
(def san-pedro-bsr (shape "EPSG:4326 POLYGON((-110.401 32.037, -110.193 32.037, -110.193 31.855, -110.401 31.855, -110.401 32.037))")) 

(def bsr_sc_1 (shape "EPSG:4326 MULTIPOLYGON (((-110.29769802088 31.9918941998369, -110.30705040833 31.9919443752441, -110.306948426194 32.003527338995, -110.312879896452 32.003678178882, -110.31485050943 32.0037945835448, -110.315436090978 32.0040600246044, -110.316015811356 32.004666670962, -110.316615799564 32.0063204594477, -110.317188812937 32.0076372432537, -110.3182098505 32.0086412801074, -110.312019038114 32.0127253202864, -110.306628493027 32.0128480896106, -110.306392361126 32.0122825346863, -110.306166201978 32.0117724437516, -110.305649273543 32.0113279432727, -110.305173152702 32.0107363642924, -110.305003688269 32.0097934543556, -110.30502049691 32.0091387772597, -110.304611045188 32.0081698426298, -110.303623044155 32.0071614757171, -110.302901992691 32.0065161253794, -110.303089036818 32.0054992089219, -110.302942220247 32.0044904341738, -110.302678497647 32.0038645666058, -110.302176518544 32.0030351329403, -110.301546178212 32.002754079272, -110.300301969243 32.0024239139718, -110.299611143473 32.0020867568214, -110.299307866034 32.0013808323259, -110.299270424729 32.000470663785, -110.299599608113 31.9992156820406, -110.300066939046 31.9980281284322, -110.300429815904 31.9976625129549, -110.300753101333 31.9972168495264, -110.30096064554 31.996828554728, -110.300899348787 31.9963469979217, -110.300304116381 31.9956997157101, -110.299564065024 31.9953688618191, -110.298551452469 31.9949239354167, -110.297906966618 31.9942829457086, -110.297487213253 31.9932089068502, -110.29769802088 31.9918941998369)))"))
(def bsr_sc_2 (shape "EPSG:4326 MULTIPOLYGON (((-110.377889632361 31.9757993429668, -110.361747536131 31.975846042225, -110.361612199488 31.9637936004556, -110.377757480771 31.9639227282788, -110.377889632361 31.9757993429668)))"))
(def bsr_sc_3 (shape "EPSG:4326 MULTIPOLYGON (((-110.313045801583 31.9526294642438, -110.312750187447 31.9602222087262, -110.311470352259 31.9612272234059, -110.309762468675 31.9611686337687, -110.309788783567 31.9598422829962, -110.303936873878 31.9597262544598, -110.302777460596 31.9591249190752, -110.302061815554 31.9583753335143, -110.304407182368 31.9561821385438, -110.303621370042 31.9548106136199, -110.301967696845 31.9536904476497, -110.301010024302 31.9536728915241, -110.299729118428 31.9543977556389, -110.299488326384 31.9558306582633, -110.297892742211 31.9575772280702, -110.2968576492 31.9585159303795, -110.296362259149 31.959455632048, -110.293887397158 31.9597724238231, -110.290682338626 31.9579706289727, -110.290633675515 31.9561735349667, -110.290203897266 31.9558123809562, -110.290040108848 31.9514406118394, -110.291557941906 31.9514599243228, -110.291532045679 31.94907641142, -110.295825662934 31.9490045979459, -110.299100700968 31.9470083063227, -110.302981183 31.9469751679097, -110.307085601343 31.9526581554385, -110.307159559196 31.9544941446366, -110.309567101729 31.9545017843643, -110.30954360172 31.9525343061251, -110.313045801583 31.9526294642438)))"))
(def bsr_sc_4 (shape "EPSG:4326 MULTIPOLYGON (((-110.366569861928 31.9193647796888, -110.350439103892 31.919409860843, -110.350305559464 31.907356191067, -110.366439477234 31.9074869550588, -110.366569861928 31.9193647796888)))"))
(def bsr_sc_5 (shape "EPSG:4326 MULTIPOLYGON (((-110.265784494403 31.8898768239616, -110.24965961121 31.8899074512999, -110.249541234801 31.8778530299868, -110.265669056878 31.8779982601507, -110.265784494403 31.8898768239616)))"))

(def san_pedro_sprnca (shape "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(def mesquite_sc_0 (shape "EPSG:4326 MULTIPOLYGON (((-110.115251587022 31.4414812540477, -110.124974867076 31.4415480387707, -110.12939332953 31.4585788198904, -110.123804032171 31.4649159391071, -110.116937300786 31.4650322363513, -110.112694547234 31.4498004271717, -110.115251587022 31.4414812540477)))"))
(def mesquite_sc_1 (shape "EPSG:4326 MULTIPOLYGON (((-110.115251587022 31.4414812540477, -110.124974867076 31.4415480387707, -110.12939332953 31.4585788198904, -110.123804032171 31.4649159391071, -110.116937300786 31.4650322363513, -110.112694547234 31.4498004271717, -110.115251587022 31.4414812540477)))"))
(def mesquite_sc_2 (shape "EPSG:4326 MULTIPOLYGON (((-110.150838301384 31.5123401764918, -110.148335693696 31.5147755447128, -110.13918317873 31.5140604240515, -110.135036758247 31.5084745931603, -110.135089440028 31.5027536123243, -110.130922581568 31.4994560448879, -110.130981541552 31.4930812352977, -110.142668478016 31.4876021665283, -110.150838301384 31.5123401764918)))"))
(def mesquite_sc_3 (shape "EPSG:4326 MULTIPOLYGON (((-110.102487263238 31.4404116550326, -110.101859186829 31.4462920887844, -110.098224099373 31.4475742891762, -110.100235466175 31.456579141049, -110.103166415381 31.4691866732593, -110.110537556189 31.4762671759603, -110.109715855361 31.4824732073751, -110.115014893644 31.486923557344, -110.119274873314 31.5005206800163, -110.117679608225 31.5078656816895, -110.120644956004 31.5172036749831, -110.125705382353 31.5270463285051, -110.130198856289 31.5363945266457, -110.128246350301 31.5411217407362, -110.116995152083 31.540063752607, -110.103439268867 31.50057418141, -110.098282751178 31.4812489158568, -110.091453059867 31.4576614078204, -110.0878072969 31.4403079658368, -110.102487263238 31.4404116550326)))"))

;; -------------------------------------------------------------------------------
;; Scenarios start here. The file contains the following scenarios:
;;   bsr-baseline
;;   bsr-development-1
;;   bsr-development-2
;;   bsr-development-3
;;   bsr-development-4
;;   bsr-development-5
;; 
;; one per each polygon in the development shapefile.
;; Modify the transformation as necessary. Vars for the mesquite scenarios are
;; defined above, so corresponding contexts can be defined similarly.
;; -----------------------------------------------------------------------

(defcontext bsr-baseline
  "San Pedro test area, resolution defined above, no transformations"
  (grid resolution san-pedro-bsr))

(defcontext bsr-development-1
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_1)  
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     bsr_sc_1)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:UrbanBarrenWater
     bsr_sc_1)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     bsr_sc_1)  
  (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
     bsr_sc_1)  
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_1)  
;; (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_1)  

;; Changes to aesthetic proximity models
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_1)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_1)  
  (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
     bsr_sc_1)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     bsr_sc_1)  
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
     bsr_sc_1)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     bsr_sc_1)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     bsr_sc_1)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     bsr_sc_1) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     bsr_sc_1)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_1))

(defcontext bsr-development-2
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_2)  
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     bsr_sc_2)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:UrbanBarrenWater
     bsr_sc_2)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     bsr_sc_2)  
  (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
     bsr_sc_2)  
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_2)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_2)  
;; Changes to aesthetic proximity models
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_2)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_2)  
  (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
     bsr_sc_2)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     bsr_sc_2)  
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
     bsr_sc_2)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     bsr_sc_2)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     bsr_sc_2)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     bsr_sc_2) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     bsr_sc_2) 
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_2))

(defcontext bsr-development-3
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_3)  
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     bsr_sc_3)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:UrbanBarrenWater
     bsr_sc_3)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     bsr_sc_3)  
  (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
     bsr_sc_3)  
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_3)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_3)  
;; Changes to aesthetic proximity models
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_3)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_3)  
  (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
     bsr_sc_3)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     bsr_sc_3)  
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
     bsr_sc_3)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     bsr_sc_3)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     bsr_sc_3)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     bsr_sc_3) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     bsr_sc_3)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_3))

(defcontext bsr-development-4
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_4)  
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     bsr_sc_4)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:UrbanBarrenWater
     bsr_sc_4)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     bsr_sc_4)  
  (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
     bsr_sc_4)  
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_4)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_4)  
;; Changes to aesthetic proximity models
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_4)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_4)  
  (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
     bsr_sc_4)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     bsr_sc_4)  
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
     bsr_sc_4)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     bsr_sc_4)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     bsr_sc_4)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     bsr_sc_4) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     bsr_sc_4)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_4))

(defcontext bsr-development-5
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_5)  
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     bsr_sc_5)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:UrbanBarrenWater
     bsr_sc_5)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     bsr_sc_5)  
  (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
     bsr_sc_5)  
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_5)  
;;  (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_5)  
;; Changes to aesthetic proximity models
;;  (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
;;     bsr_sc_5)  
;; (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
;;     bsr_sc_5)  
  (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
     bsr_sc_5)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     bsr_sc_5)  
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
     bsr_sc_5)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     bsr_sc_5)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     bsr_sc_5)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     bsr_sc_5) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     bsr_sc_5)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_5))

(defcontext mesquite
  "San Pedro mesquite removal"
  (grid resolution san_pedro_sprnca)
;; Changes to surface water model
  (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'sanPedro:EvapotranspirationVegetationType 'sanPedro:Grassland
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
;; Changes to aesthetic view models
  (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)
;; Changes to aesthetic proximity models
  (transform 'aestheticService:Grassland 'aestheticService:GrasslandPresent
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'aestheticService:Park 'aestheticService:ParkAbsent
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3) 
;; Changes to carbon models
  (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3))
  (transform 'sanPedro:CarbonVegetationType 'sanPedro:Grassland
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3))

(defcontext mesquite-base
   "Baseline to compare mesquite results at the same spatial extent and resolution"
   (grid resolution san_pedro_sprnca))

;;CAP water augmentation scenarios handeled entirely through defscenario statements.
