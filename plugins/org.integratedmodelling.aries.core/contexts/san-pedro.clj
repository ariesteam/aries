(ns core.contexts.san_pedro
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model transform])
  (:refer geospace :only [grid shape]))

(defcontext bsr-development
   "BSR development scenarios" 
   (grid 256 'san_pedro_us)
;; Changes to surface water model
    (transform 'waterSupplyService:PercentImperviousCoverClass 'waterSupplyService:ModeratelyHighImperviousCover
      (shape 'san_pedro_us))  
    (transform 'waterSupplyService:PercentVegetationCoverClass 'waterSupplyService:VeryLowVegetationCover
      (shape 'san_pedro_us))  
;    (transform 'sanPedro:VegetationType 'sanPedro:UrbanBarrenWater
;      (shape 'san_pedro_us))  
;; Changes to aesthetic proximity & view models
;    (transform 'sanPedro:ScenicVegetationType 'sanPedro:Other
;      (shape 'san_pedro_us))  
;    (transform 'aestheticService:DevelopedLand 'aestheticService:LowDensityDevelopment
;      (shape 'san_pedro_us))  
    (transform 'aestheticService:PresenceOfHousing 'aestheticService:HousingPresent
      (shape 'san_pedro_us))  
    (transform 'aestheticService:HousingValue 'aestheticService:ModerateHousingValue
      (shape 'san_pedro_us))  
;    (transform 'sanPedro:ForestAndWoodland 'sanPedro:ForestOrWoodlandAbsent
;      (shape 'san_pedro_us))  
    (transform 'aestheticService:Farmland 'aestheticService:FarmlandAbsent
      (shape 'san_pedro_us))  
;    (transform 'aestheticService:Grassland 'aestheticService:GrasslandAbsent
;      (shape 'san_pedro_us))  
;     (transform 'aestheticService:DesertScrub 'aestheticService:DesertScrubAbsent
;      (shape 'san_pedro_us))  
;     (transform 'sanPedro:RiparianAndWetland 'sanPedro:RiparianOrWetlandAbsent
;      (shape 'san_pedro_us))  
    (transform 'aestheticService:Park 'aestheticService:ParkAbsent
      (shape 'san_pedro_us))  
;; Changes to surface carbon models
    (transform 'carbonService:PercentVegetationCover 'carbonService:VeryLowVegetationCover
      (shape 'san_pedro_us))  
) 