;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.

(ns core.contexts.san-pedro
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
(def san_pedro_us (shape "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

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
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     bsr_sc_1)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:UrbanBarrenWater
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     bsr_sc_1)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_1))

(defcontext bsr-development-2
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_2)  
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     bsr_sc_2)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:UrbanBarrenWater
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     bsr_sc_2) 
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_2))

(defcontext bsr-development-3
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_3)  
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     bsr_sc_3)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:UrbanBarrenWater
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     bsr_sc_3)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_3))

(defcontext bsr-development-4
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_4)  
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     bsr_sc_4)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:UrbanBarrenWater
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     bsr_sc_4)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_4))

(defcontext bsr-development-5
  "Just testing gazetteer shapes"
  (grid resolution san-pedro-bsr)
;; Changes to surface water model
  (transform 'waterSupplyService:MountainFront 'waterSupplyService:MountainFrontAbsent
     bsr_sc_5)  
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     bsr_sc_5)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:UrbanBarrenWater
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     bsr_sc_5)  
  (transform 'carbonService:FireFrequency 'carbonService:NoFireFrequency
     bsr_sc_5))

(defcontext mesquite
  "San Pedro mesquite removal"
  (grid resolution san_pedro_us) ;;Change back to san_pedro_sprnca for
  ;;non-hydrologic services
;; Changes to surface water model
  (transform 'waterSupplyService:PercentTreeCanopyCoverClass 'waterSupplyService:VeryLowCanopyCover
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)  
  (transform 'sanPedro:WaterSupplyVegetationType 'sanPedro:Grassland
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
  (transform 'carbonService:PercentTreeCanopyCoverClass 'carbonService:VeryLowCanopyCover
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3)
  (transform 'sanPedro:CarbonVegetationType 'sanPedro:Grassland
     mesquite_sc_1 mesquite_sc_2 mesquite_sc_3))

(defcontext mesquite-base
   "Baseline to compare mesquite results at the same spatial extent and resolution"
   (grid resolution san_pedro_sprnca))

;;CAP water augmentation scenarios handeled entirely through
;;defscenario statements.

(defcontext arizona512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-114.821 37, -109.05 37, -109.05 31.333, -114.821 31.333, -114.821 37))"))

(defcontext arizona2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-114.821 37, -109.05 37, -109.05 31.333, -114.821 31.333, -114.821 37))"))

(defcontext san_pedro256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 30.869, -111.012 30.869, -111.012 33.281))"))

(defcontext san_pedro512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 30.869, -111.012 30.869, -111.012 33.281))"))

(defcontext san_pedro1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-110.98 33.07, -109.86 33.07, -109.86 30.869, -110.98 30.869, -110.98 33.07))"))

(defcontext san_pedro_watershed1024_simple
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-110.49310706230432 31.28329486017922, -110.34581533520846 31.429942672410363, -110.64037950722019 31.574806044445126, -110.41172819658986 31.843266418659795, -110.51649870688502 32.25397752083476, -110.97575582919558 32.77538849749573, -110.81490831477673 32.97313626702431, -110.33320973767174 33.02570528420744, -110.03965596157498 32.697619696442445, -110.2980873256088 32.58277055722701, -109.89346023082143 31.80457173406103, -109.9354683125433 30.992492522487662, -110.36253022820469 30.98174936794606, -110.49310706230432 31.28329486017922))"))

(defcontext san_pedro_watershed1024_real
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-110.49310706230432 31.28329486017922, -110.48047902917494 31.283244415528976, -110.47096260834765 31.291339620121686, -110.45824048921659 31.307554627294028, -110.4486301849515 31.331011904552607, -110.44221706664557 31.34725211038042, -110.43474848484544 31.363487292403587, -110.42827047839627 31.38966782061902, -110.41770749870129 31.394139107348312, -110.40813475025652 31.408555464131897, -110.39961868372698 31.422072247611624, -110.38485866894997 31.42290600594162, -110.3732600642625 31.423753562359963, -110.35851806295702 31.421873105782417, -110.34797950898422 31.421819920810886, -110.34581533520846 31.429942672410363, -110.35634244414155 31.431803492761723, -110.36790578103037 31.436379829207244, -110.3762661296407 31.447265846943736, -110.39097155783725 31.455470388958826, -110.39093061533273 31.46179646145372, -110.38980010332102 31.473539844113603, -110.40030419126995 31.47991572852977, -110.40985155217226 31.470922580085936, -110.4330814472958 31.465605014280907, -110.45205471830754 31.466591362235594, -110.45209155904338 31.460265226100532, -110.4605415444683 31.45758982645749, -110.48265654799258 31.462199861875728, -110.4995018405769 31.466785601025347, -110.5121076065366 31.47587184122619, -110.51843917431849 31.474992053477248, -110.53845659894515 31.478680817363774, -110.53946242843845 31.488625788235385, -110.54890645036637 31.49860075728557, -110.56576158906984 31.503177846628635, -110.57623215827974 31.52038433192734, -110.58568327178983 31.530356601303065, -110.60145622486048 31.543059287612007, -110.59509347307421 31.550269324872648, -110.59504663051739 31.561114198984576, -110.6097967364988 31.567486330901428, -110.62668528272393 31.567536769316558, -110.64037950722019 31.574806044445126, -110.63612543534177 31.582927759192057, -110.62234289455169 31.59734774526154, -110.6074773114966 31.617185332714275, -110.60107520881589 31.63252911713954, -110.6009362366276 31.665063288460487, -110.59562958064603 31.67046899944752, -110.57548252962721 31.685767031261314, -110.55110944223058 31.69923954465096, -110.53933486979733 31.72902053237728, -110.51696171639367 31.76147166212429, -110.4978447819057 31.77585720441463, -110.48403722046551 31.785742868219536, -110.45868246968931 31.77931162729846, -110.4470347334523 31.781069008963538, -110.42152639369647 31.79993356736575, -110.41172819658986 31.843266418659795, -110.41800620762726 31.855043391119036, -110.4095245773035 31.8568116938632, -110.41258057685334 31.87580347994735, -110.42501905325602 31.919237621460738, -110.43861687159057 31.948216488121602, -110.43848491864746 31.969904360049696, -110.45842486606966 32.00433095249226, -110.46673081048478 32.0350912220077, -110.45813762977517 32.05312829019652, -110.455946011704 32.06486664777552, -110.46334262842991 32.07032021593191, -110.47498536922772 32.075791017726786, -110.48655371540576 32.09481551051139, -110.49186638738095 32.09393326315261, -110.52050873623766 32.09765960471171, -110.52149020192903 32.11302570917194, -110.52346341776307 32.14195049859639, -110.527623222736 32.15913570961405, -110.53279627493403 32.18626475309057, -110.54118826922654 32.20798327183159, -110.53260686108489 32.22331397732265, -110.51984574850623 32.22416980000479, -110.51649870688502 32.25397752083476, -110.53342508237749 32.27121048672147, -110.54297021584001 32.27666727146854, -110.55560627760792 32.30291809692395, -110.566241614041 32.30385873337079, -110.59178518597244 32.3021366287372, -110.60454093345385 32.3048881714524, -110.62037269018329 32.335660694798975, -110.64262115657691 32.36193128168196, -110.67238620892992 32.3746630762004, -110.70529293015562 32.40637161392264, -110.72552615131671 32.40822457806268, -110.74360674541694 32.41820265729253, -110.74993143386811 32.44261347953065, -110.7455342899625 32.49049697885287, -110.75611323981116 32.521241109564976, -110.76774064475805 32.561925939017094, -110.78582652487219 32.584548409398714, -110.79857350630982 32.611677816842636, -110.83270727624279 32.62799287120713, -110.86362472532039 32.66056005522409, -110.88605826288531 32.65967882307446, -110.90206449741432 32.67505358071746, -110.92127160074837 32.702175099041845, -110.93512421353776 32.75820585842311, -110.9479488292585 32.77176567807541, -110.96613096879479 32.771771913155774, -110.97575582919558 32.77538849749573, -110.95220910231052 32.805200046889105, -110.9158090809452 32.8286739776859, -110.90829897632943 32.84674026591006, -110.89007373213305 32.869314960518096, -110.89646645125879 32.89642756226021, -110.89643594722222 32.922631000993, -110.88570621209654 32.93527155684972, -110.85674369879303 32.95512007544817, -110.81490831477673 32.97313626702431, -110.77950922285926 32.98301790103377, -110.74949130975844 32.98296129027621, -110.71943060866293 32.99645065180732, -110.70870834250336 32.99642609538691, -110.69693239608767 32.99097675902181, -110.66906305617093 32.989098900889964, -110.64663107537025 32.968255752691924, -110.63061682507006 32.95284901434736, -110.6242860411523 32.92933768607436, -110.6050168080537 32.92566404495579, -110.5717002285161 32.94723961258457, -110.54591683131724 32.9597982715283, -110.52236238832573 32.955192686873744, -110.51379370796064 32.95425608361232, -110.48475323699523 32.97221059711386, -110.46971961044729 32.97666538638487, -110.46103967114458 32.99379550658144, -110.44602944900836 32.993729807970844, -110.42674234782027 32.9918356651529, -110.41922560470633 32.99360800104215, -110.38266567835961 33.00879265181162, -110.37080593713647 33.01776874316657, -110.33320973767174 33.02570528420744, -110.3182848733454 33.01387880221016, -110.31746296937662 32.98134725610676, -110.2962383707336 32.95412348544302, -110.31023297502495 32.94606971977903, -110.32739155682893 32.94435619412088, -110.32644330718054 32.928087388536014, -110.32013963312532 32.91178952567198, -110.31479784403406 32.90995327718254, -110.29563219801703 32.8953901011911, -110.2893075571832 32.882704717876805, -110.26794390071413 32.8762566966991, -110.22725742485396 32.87601194683336, -110.21446494461826 32.86960748918971, -110.21168456452311 32.82079872186035, -110.19789606349225 32.8071579530485, -110.14704356079959 32.75441624101752, -110.13855076312888 32.748033306745626, -110.10649499049374 32.74600177508068, -110.09684576121153 32.74864324654179, -110.06497026858554 32.72943508480438, -110.05217230988715 32.726628347449044, -110.03965596157498 32.697619696442445, -110.05297386155301 32.650736338603735, -110.07873443586817 32.63827825337346, -110.09830838263967 32.60318141587166, -110.12725089766008 32.59073586987059, -110.17966223207097 32.57843894368413, -110.23531433934217 32.55981466177073, -110.24491664114498 32.559872707519226, -110.26715313846594 32.580786787946835, -110.28737237042652 32.58813176184224, -110.2980873256088 32.58277055722701, -110.28119945211277 32.559181825544954, -110.28772946798664 32.54295436700043, -110.28598192185414 32.494150330755005, -110.2595722571524 32.464178967651414, -110.24254222503478 32.46136686350922, -110.19588183991699 32.43487319511967, -110.1756468269435 32.43383879267276, -110.14062530244935 32.419147212116556, -110.10885485884376 32.39995103524454, -110.08679902916079 32.369070928372174, -110.09112989795615 32.3618734370974, -110.08798159974167 32.357332715905315, -110.0606129691566 32.32731310172362, -110.07034716450829 32.31202396375749, -110.04210501038993 32.26572901443733, -110.05066875819982 32.26037202554447, -110.07508014814968 32.26507118279646, -110.08056118115714 32.24884618716545, -110.09122946612727 32.24530891695879, -110.07665557081383 32.21448033602429, -110.04506363371995 32.18623354572462, -110.02910718359534 32.18791928975874, -110.02064587909696 32.18423958953114, -110.0452427964876 32.169066138689125, -110.05073225856924 32.151938631833794, -110.03927668649456 32.13016521935372, -110.06705005281913 32.114107627943625, -110.12341348047012 32.104570352999616, -110.12348252713427 32.09734172497361, -110.10037794499996 32.071877864523394, -110.07302436866883 32.04818611349445, -110.04351669631596 32.02899050294698, -110.00980880992739 32.0070444795693, -109.99944029800604 31.985275791425202, -110.002786889061 31.969940226219197, -109.99129513673567 31.9544875022161, -109.98533353986298 31.91829418993888, -109.98871924507765 31.899344547910335, -109.96565015702281 31.87837494456372, -109.95108130720615 31.855664312277234, -109.89346023082143 31.80457173406103, -109.9013795350553 31.761264872932188, -109.93437103395311 31.743471858277847, -109.94422551180098 31.71373282745597, -109.9350839189037 31.6811246164373, -109.95789921476631 31.6252848439617, -109.94861843588806 31.605327699381863, -109.97539946672927 31.570301350389993, -109.97451215561189 31.554931414986406, -109.95993920617856 31.536739775508543, -109.96546344316249 31.51419210503906, -109.96129428444716 31.509639821267132, -109.94803802882068 31.468864933492164, -109.95037842448485 31.448098895325558, -109.94726683224134 31.44355482059854, -109.93354456783176 31.4452487228155, -109.91887159766549 31.43789608323282, -109.88113492395266 31.420401739126696, -109.87077110459511 31.405851606985667, -109.88163862108017 31.377932002113255, -109.88414011740828 31.344516504614813, -109.87375149574858 31.3326774883933, -109.88023901182264 31.31827481632736, -109.87614679902819 31.308298183603522, -109.88263216392622 31.293895288586008, -109.88058097247973 31.289358830255257, -109.89225078579217 31.28132650564629, -109.90608534982631 31.267889175084477, -109.93890027147455 31.25008970337462, -109.9557295971687 31.25022752839428, -109.96621844305758 31.25302352281465, -109.97783773237899 31.248597758974316, -109.99246736270796 31.25775065740088, -109.99984042632417 31.256904505355056, -109.99151150386922 31.248705763326242, -109.97690281911898 31.237745450949657, -109.9770780690663 31.221479506225702, -109.96564906642223 31.208735712268915, -109.95628589837447 31.19962266564993, -109.93852524097184 31.189536211676565, -109.91862522511573 31.183044345985852, -109.91343175660701 31.177578167698947, -109.93127882967752 31.179535081155382, -109.94207057023927 31.15431938309005, -109.94545296736202 31.133560913543064, -109.94556292204128 31.123620557216444, -109.94980447233651 31.12004019351379, -109.94796279368187 31.096527573603712, -109.93352335019105 31.072911287244292, -109.9294149904973 31.064743436764598, -109.9326652438187 31.05573279468702, -109.92858802156715 31.044853888096807, -109.92345208406192 31.034869781047778, -109.93513684423691 31.02231405372741, -109.94148271349607 31.017847427321666, -109.9363066340277 31.011478525761014, -109.9354683125433 30.992492522487662, -109.9429009516532 30.984419652016044, -109.95234111519632 30.984496566108295, -109.95228185817751 30.989918709452645, -109.95955597864649 30.996303910140636, -109.96481094934902 30.995442370693933, -109.97310638133642 31.00454631827225, -109.9772064324644 31.013616572031534, -109.9930210340477 31.006510713613473, -110.00770936375781 31.006624434594237, -110.02660366342886 31.005864484119122, -110.03812637701758 31.007758397113996, -110.04233218049673 31.00688588178323, -110.04028813463972 31.001448025260892, -110.05605173290284 30.998852875730403, -110.08021581876527 30.995412107818062, -110.09174703496812 30.996397508336816, -110.10741597821405 31.003737066532633, -110.13153055453829 31.005709091538105, -110.13783387234965 31.00484754319386, -110.14103005400595 30.99944609267877, -110.14732484275615 30.999487810659826, -110.1483257768623 31.00491717322257, -110.16192577457805 31.009525129844175, -110.18287993339537 31.01327462696628, -110.1849862110358 31.01238414554171, -110.19756249774672 31.014270518204988, -110.22490312501871 31.00720716354994, -110.25221867787079 31.002849233794098, -110.26063318021045 31.000186274111062, -110.27011013355556 30.99572115700188, -110.27959300630842 30.99035155751764, -110.29325754062089 30.98681181334452, -110.30692762513826 30.98236682986483, -110.31530015156753 30.98512291552952, -110.3247413140892 30.985172615179863, -110.32799603504445 30.969824748608453, -110.3321914461715 30.96984650050977, -110.341593919647 30.975317650471396, -110.35623629392515 30.981718155834006, -110.36253022820469 30.98174936794606, -110.376108309721 30.99085382791777, -110.38761369452484 30.99633179248019, -110.38650089005797 31.006268483955218, -110.38961988651144 31.010802340405373, -110.38956227932842 31.019840208311862, -110.38007076123483 31.027025473353653, -110.37372060548401 31.035129112903697, -110.37891569218706 31.04328850812167, -110.38408837148802 31.055062817870112, -110.38403014066637 31.06410061435236, -110.3891814195698 31.079489816276514, -110.3985010996939 31.100321126749215, -110.40467319565548 31.12113722581751, -110.4119934619024 31.12659364379177, -110.41295499968047 31.14105881891408, -110.41501210824985 31.148298562425452, -110.42020574443235 31.158263818275906, -110.43070975888688 31.159214484297387, -110.43909077002807 31.16377025365763, -110.45170284432031 31.163824580924352, -110.45903936101149 31.167470808320488, -110.45901881501318 31.171085913239967, -110.45791624504803 31.18011924373326, -110.4620852305648 31.18646330668874, -110.46730103547371 31.193715365149327, -110.47675813639577 31.194657962499473, -110.4841176097435 31.194687677484524, -110.48934525970112 31.20013130488319, -110.49034337166846 31.210077006625507, -110.49767061042876 31.216432386740188, -110.49551942156648 31.225461898407847, -110.49024638885429 31.22815248184489, -110.49022212949406 31.23267134266065, -110.49336781167004 31.234491357055187, -110.49228701471763 31.239909837662807, -110.50172051416806 31.246273275146972, -110.51013594539198 31.24630561684394, -110.5111692521519 31.249924711070587, -110.51217475008553 31.25896642508743, -110.51215150613376 31.263485279330894, -110.50582465708852 31.266172470224863, -110.5016018096126 31.268867523457946, -110.50368251063091 31.273394509231466, -110.50257810776876 31.283331879172803, -110.49310706230432 31.28329486017922))"))

(defcontext san_pedro_sprnca2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext san_pedro_bsr256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-110.401 32.037, -110.193 32.037, -110.193 31.855, -110.401 31.855, -110.401 32.037))"))

(defcontext san_pedro_us160
  ""
  (grid
   160
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us400
  ""
  (grid
   400
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us2048
  ""
  (grid
   2048
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_us4500
  ""
  (grid
   4500
   "EPSG:4326 POLYGON((-111.012 33.281, -109.845 33.281, -109.845 31.328, -111.012 31.328, -111.012 33.281))"))

(defcontext san_pedro_sprnca256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext san_pedro_sprnca512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-110.377 31.881, -109.987 31.881, -109.987 31.328, -110.377 31.328, -110.377 31.881))"))

(defcontext se_arizona_recreation
  ""
  (grid
   1024
   "EPSG:4326 POLYGON((-111.7 33.281, -109.05 33.281, -109.05 31.328, -111.7 31.328, -111.7 33.281))"))