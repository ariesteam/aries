(ns aries.db.bv1  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

(defobject puget_pugetbeaches aestheticService:BeachPresence
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetbeaches")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 341432.39480104897)
        (geospace:hasLonLowerBound 602542.4124029671)
        (geospace:hasLatUpperBound 1366881.9205084355)
        (geospace:hasLonUpperBound 1233292.2512319256)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject puget_pugetcities geofeatures:City
  (observation:Categorization
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetcities")
        (geospace:hasValueAttribute "place_nm")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 64837.072613952645)
        (geospace:hasLonLowerBound 616557.6098079457)
        (geospace:hasLatUpperBound 1370631.5128883347)
        (geospace:hasLonUpperBound 2576704.1850025496)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject puget_pugetparks aestheticService:ParkPresence
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetparks")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -238866.43825608978)
        (geospace:hasLonLowerBound 639892.1412067211)
        (geospace:hasLatUpperBound 762810.3312307579)
        (geospace:hasLonUpperBound 1586270.971328112)
        (geospace:hasCoordinateReferenceSystem "EPSG:2926")))))


(defobject vermont_lye_brook_stands_wgs84_v2 habitat:StandCondition
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:lye_brook_stands_wgs84_v2")
        (geospace:hasValueAttribute "stand_cond")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 43.053)
        (geospace:hasLonLowerBound -73.096)
        (geospace:hasLatUpperBound 43.166)
        (geospace:hasLonUpperBound -72.94)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject vermont_lye_brook_stands_wgs84_v2 habitat:StandSizeDensity
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:lye_brook_stands_wgs84_v2")
        (geospace:hasValueAttribute "size_densi")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 43.053)
        (geospace:hasLonLowerBound -73.096)
        (geospace:hasLatUpperBound 43.166)
        (geospace:hasLonUpperBound -72.94)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject vermont_vt_soilcarbon carbonService:SoilCarbonStorage
  (measurement:Measurement
    (measurement:unit "g/m^2")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:vt_soilcarbon")
        (geospace:hasValueAttribute "wt_avg_rv")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24779.428113313774)
        (geospace:hasLonLowerBound 422354.0733692106)
        (geospace:hasLatUpperBound 280085.1865430563)
        (geospace:hasLonUpperBound 554628.3863183097)
        (geospace:hasCoordinateReferenceSystem "EPSG:32145")))))


(defobject NBCD_MZ65_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_65")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 11168)
        (geospace:hasYRangeMax 10493)
        (geospace:hasLatLowerBound 40.515)
        (geospace:hasLonLowerBound -74.156)
        (geospace:hasLatUpperBound 44.54)
        (geospace:hasLonUpperBound -69.872)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NBCD_MZ66_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_66")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16371)
        (geospace:hasYRangeMax 11559)
        (geospace:hasLatLowerBound 42.887)
        (geospace:hasLonLowerBound -73.409)
        (geospace:hasLatUpperBound 47.465)
        (geospace:hasLonUpperBound -66.925)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject agricultural_use_la waterSupplyService:AgriculturalSurfaceWaterUse
  (measurement:Measurement
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:agricultural_use_la")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 526)
        (geospace:hasYRangeMax 254)
        (geospace:hasLatLowerBound 2123568.508)
        (geospace:hasLonLowerBound 703009.19)
        (geospace:hasLatUpperBound 2161968.508)
        (geospace:hasLonUpperBound 782209.19)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject aquacultural_use_la waterSupplyService:AquaculturalSurfaceWaterUse
  (measurement:Measurement
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:aquacultural_use_la")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 246)
        (geospace:hasYRangeMax 139)
        (geospace:hasLatLowerBound 2138711.579)
        (geospace:hasLonLowerBound 699310.239)
        (geospace:hasLatUpperBound 2159861.579)
        (geospace:hasLonUpperBound 736510.239)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject industrial_use_la waterSupplyService:IndustrialSurfaceWaterUse
  (measurement:Measurement
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:industrial_use_la")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 78)
        (geospace:hasYRangeMax 231)
        (geospace:hasLatLowerBound 2124532.458)
        (geospace:hasLonLowerBound 708671.021)
        (geospace:hasLatUpperBound 2159482.458)
        (geospace:hasLonUpperBound 720671.021)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject residential_use_la waterSupplyService:ResidentialSurfaceWaterUse
  (measurement:Measurement
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:residential_use_la")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 594)
        (geospace:hasYRangeMax 307)
        (geospace:hasLatLowerBound 2119161.498)
        (geospace:hasLonLowerBound 684199.002)
        (geospace:hasLatUpperBound 2165511.498)
        (geospace:hasLonUpperBound 773599.002)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject surface_water_prox_la waterSupplyService:ProximityToSurfaceWater
  (measurement:Measurement
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:surface_water_prox_la")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3473)
        (geospace:hasYRangeMax 1555)
        (geospace:hasLatLowerBound 2118784.042)
        (geospace:hasLonLowerBound 681448.0)
        (geospace:hasLatUpperBound 2165494.042)
        (geospace:hasLonUpperBound 785698.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


