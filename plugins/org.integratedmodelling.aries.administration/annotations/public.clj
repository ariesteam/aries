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

(ns administration.annotations.public
  (:refer modelling :only [defobject]))

(defobject corine2000 corine:CORINENumeric

  "Data source: CORINE Land Cover"

  (measurement:NumericCoding
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.eea.europa.eu/themes/landuse/interactive/clc-download")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "europe:corine2000")
        (geospace:hasNodataValue 255)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 66998)
        (geospace:hasYRangeMax 57998)
        (geospace:hasLatLowerBound 700000.0)
        (geospace:hasLonLowerBound 800000.0)
        (geospace:hasLatUpperBound 6500000.0)
        (geospace:hasLonUpperBound 7500000.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:3035")))))


(defobject dem90m geophysics:Altitude

  "Data source: Shuttle Radar Topography Mission (SRTM)"

  (measurement:Measurement
    (metadata:hasURL "http://srtm.usgs.gov/index.php")
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:dem90m")
        (geospace:hasNodataValue -32768)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 431999)
        (geospace:hasYRangeMax 143999)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 60.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject slope90m geophysics:DegreeSlope

  "Data source: Derived from Shuttle Radar Topography Mission (SRTM) data"

  (measurement:Measurement
    (metadata:hasURL "http://srtm.usgs.gov/index.php")
    (measurement:unit "°")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:slope90m")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 431999)
        (geospace:hasYRangeMax 143999)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -175.0)
        (geospace:hasLatUpperBound 60.0)
        (geospace:hasLonUpperBound 185.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_pugetfloodplain geofeatures:Floodplain

  "Data source: FEMA Q3 flood data"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "FEMA_floodplain_extents")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetfloodplain")
        (geospace:hasValueAttribute "zone")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.85126051068143)
        (geospace:hasLonLowerBound -124.7500353279863)
        (geospace:hasLatUpperBound 49.0003072438)
        (geospace:hasLonUpperBound -121.02006408919881)
        (geospace:hasCoordinateReferenceSystem "EPSG:4267")))))


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

(defobject evapotrans habitat:ActualEvapotranspiration

  "Data source: University of Delaware Global Water Balance Archive"

  (measurement:Measurement
    (measurement:unit "mm")
    (metadata:hasPriority 10)
    (metadata:hasURL "http://climate.geog.udel.edu/~climate/html_pages/README.wb_ts2.html")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:aet_global")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Temp_Max_Yearly geophysics:AnnualMaximumGroundSurfaceTemperature

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:tmp_ann_max_us")
        (geospace:hasTransformation "self * 0.01")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7023)
        (geospace:hasYRangeMax 3103)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.937)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject tmp_ann_max_global geophysics:AnnualMaximumGroundSurfaceTemperature

  "Data source: WorldClim data"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.worldclim.org/bioclim")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tmp_ann_max_global")
        (geospace:hasTransformation "self * 0.1")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 17998)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0000078)
        (geospace:hasLonUpperBound 180.0000188)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject avgannrunoff habitat:AnnualRunoff

  "Data source: University of New Hampshire GRDC Composite Runoff Fields"

  (measurement:Measurement
    (measurement:unit "mm")
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.grdc.sr.unh.edu/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:avg_ann_runoff_global")
        (geospace:hasNodataValue -32768)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 275)
        (geospace:hasLatLowerBound -55.5)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_bridges infrastructure:Bridge

  "Data source: Washington State Department of Transportation"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.wsdot.wa.gov/mapsdata/geodatacatalog/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetbridges")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")
        (geospace:hasLatLowerBound 88246.4453125)
        (geospace:hasLonLowerBound 648284.375)
        (geospace:hasLatUpperBound 1344600.875)
        (geospace:hasLonUpperBound 2513722.0)))))


(defobject puget_clearcuts_puget_new geofeatures:Clearcut

  "Data source: Washington State DNR"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fortress.wa.gov/dnr/app1/dataweb/dmmatrix.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:clearcuts_puget_new")
        (geospace:hasValueAttribute "timharv_fp")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.587703704834)
        (geospace:hasLonLowerBound -124.663955688477)
        (geospace:hasLatUpperBound 49.0023994445801)
        (geospace:hasLonUpperBound -117.039825439453)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject CoastalWetlands soilRetentionService:CoastalWetlands

  "Data source: Derived from NLCD 2001"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:coastal_wetlands_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8634)
        (geospace:hasYRangeMax 10362)
        (geospace:hasLatLowerBound 45.017870620002014)
        (geospace:hasLonLowerBound -125.33149754079895)
        (geospace:hasLatUpperBound 49.099934066182016)
        (geospace:hasLonUpperBound -121.93004058197893)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject comm_ind_trans_puget nlcd:NLCD1992Typology

  "Data source: NLCD 1992"

  (measurement:NumericCoding
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.mrlc.gov/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:comm_ind_trans_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 23234)
        (geospace:hasYRangeMax 20021)
        (geospace:hasLatLowerBound 4958500.48)
        (geospace:hasLonLowerBound 335717.398)
        (geospace:hasLatUpperBound 5559190.48)
        (geospace:hasLonUpperBound 1032797.398)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject pfactor1990_30min soilRetentionService:ConservationPractice

  "Data source: Yang et al. (2003)"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:rusle_pfactor")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject cfactor1990_30min soilRetentionService:CoverManagement

  "Data source: Yang et al. (2003)"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:rusle_cfactor")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_dam_storage_puget floodService:DamStorage

  "Data source: National Atlas of the United States, Major Dams of the United States"

  (measurement:Measurement
    (measurement:unit "mm")
    (metadata:hasPriority 0)
    (metadata:hasURL "http://nationalatlas.gov/atlasftp.html?openChapters=chpwater")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:dam_storage_puget")
        (geospace:hasValueAttribute "depth_ft")
        (geospace:hasTransformation "self * 304.8")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.670997355311954)
        (geospace:hasLonLowerBound -124.27)
        (geospace:hasLatUpperBound 49.01399658496675)
        (geospace:hasLonUpperBound -120.98400439562957)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject california_dam_storage_socal floodService:DamStoragePresence

  "Data source: National Atlas of the United States, Major Dams of the United States"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://nationalatlas.gov/atlasftp.html?openChapters=chpwater")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:dam_storage_socal")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3702424.287036522)
        (geospace:hasLonLowerBound 379189.00360562425)
        (geospace:hasLatUpperBound 3790640.4773841826)
        (geospace:hasLonUpperBound 448528.5578083895)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject usa_meandaysprecipdec habitat:DecemberDaysOfPrecipitation

  "Data source: NOAA-National Climatic Data Center"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:meandaysprecipdec")
        (geospace:hasValueAttribute "gridcode")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24.463)
        (geospace:hasLonLowerBound -124.783)
        (geospace:hasLatUpperBound 49.427)
        (geospace:hasLonUpperBound -66.915)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Temp_Max_Dec geophysics:DecemberMaximumGroundSurfaceTemperature

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:tmp_dec_us")
        (geospace:hasTransformation "self * 0.01")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7023)
        (geospace:hasYRangeMax 3103)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.937)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject tmp_dec geophysics:DecemberMeanGroundSurfaceTemperature

  "Data source: University of East Anglia Climate Research Unit (New et al. 1999)"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.sage.wisc.edu/atlas/maps.php?catnum=3&type=Ecosystems")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tmp_dec_global")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2154)
        (geospace:hasYRangeMax 853)
        (geospace:hasLatLowerBound -59.1665)
        (geospace:hasLonLowerBound -180.0005)
        (geospace:hasLatUpperBound 83.6185)
        (geospace:hasLonUpperBound 180.05150000000003)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject DecPrecip habitat:DecemberPrecipitation

  "Data source: USDA-NRCS, derived from PRISM climate data"

  (measurement:Measurement
    (measurement:unit "mm")
    (metadata:hasPriority 5)
    (metadata:hasURL "http://datagateway.nrcs.usda.gov/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:precip_dec_puget")
        (geospace:hasTransformation "self * 25.4")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 792)
        (geospace:hasYRangeMax 571)
        (geospace:hasLatLowerBound 46.138)
        (geospace:hasLonLowerBound -124.763)
        (geospace:hasLatUpperBound 49.003)
        (geospace:hasLonUpperBound -120.793)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject mg_mgdeforestation carbonService:DeforestationRisk

  "Data source: Calculated by Wendland et al. (in press)"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:mgdeforestation")
        (geospace:hasValueAttribute "class")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -25.465)
        (geospace:hasLonLowerBound 40.272)
        (geospace:hasLatUpperBound -0.214)
        (geospace:hasLonUpperBound 50.035)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_detention infrastructure:DetentionBasin

  "Data source: Derived from King, Pierce, and San Juan county GIS point data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:detention_basins_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5628)
        (geospace:hasYRangeMax 6002)
        (geospace:hasLatLowerBound 46.3)
        (geospace:hasLonLowerBound -124.3)
        (geospace:hasLatUpperBound 48.703)
        (geospace:hasLonUpperBound -121.478)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject fire_freq_puget habitat:FireFrequency

  "Data source: Derived from Washington DNR and Oregon Department of Forestry wildfire point data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fortress.wa.gov/dnr/app1/dataweb/dmmatrix.html, http://www.oregon.gov/ODF/GIS/gisdata.shtml")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:fire_freq_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 624)
        (geospace:hasYRangeMax 793)
        (geospace:hasLatLowerBound 4648241.757)
        (geospace:hasLonLowerBound 370946.591)
        (geospace:hasLatUpperBound 5443241.757)
        (geospace:hasLonUpperBound 996946.591)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject ghg_emissions_us carbonService:GreenhouseGasEmissions

  "Data source: Purdue University Vulcan Project"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.purdue.edu/eas/carbon/vulcan/index.php")
    (measurement:unit "t/ha*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:ghg_emissions_us")
        (geospace:hasTransformation "self * 0.0001")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3759)
        (geospace:hasYRangeMax 1759)
        (geospace:hasLatLowerBound 22.0900498)
        (geospace:hasLonLowerBound -137.257054)
        (geospace:hasLatUpperBound 57.3100498)
        (geospace:hasLonUpperBound -62.037054)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject hardwood_softwood habitat:HardwoodSoftwoodRatio

  "Data source: Interagency Vegetation Mapping Project"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.blm.gov/or/gis/data-details.php?data=ds000103")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:hardwood_softwood")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14513)
        (geospace:hasYRangeMax 32020)
        (geospace:hasLatLowerBound 4635337.5)
        (geospace:hasLonLowerBound 365037.5)
        (geospace:hasLatUpperBound 5435887.5)
        (geospace:hasLonUpperBound 727912.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:26710")))))


(defobject HSG habitat:HydrologicSoilsGroup

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_hydrologic_soils_group")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:hsg_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject hsg habitat:HydrologicSoilsGroup

  "Data source: University of Vermont Spatial Analysis Lab, derived from FAO's Digital Soil Map of the World"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "global:hsg_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 40316)
        (geospace:hasYRangeMax 15733)
        (geospace:hasLatLowerBound -56.453)
        (geospace:hasLonLowerBound -179.872)
        (geospace:hasLatUpperBound 84.038)
        (geospace:hasLonUpperBound 180.11)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_meandaysprecipjan habitat:JanuaryDaysOfPrecipitation

  "Data source: NOAA-National Climatic Data Center"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:meandaysprecipjan")
        (geospace:hasValueAttribute "gridcode")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24.463)
        (geospace:hasLonLowerBound -124.802)
        (geospace:hasLatUpperBound 49.425)
        (geospace:hasLonUpperBound -66.915)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Temp_Max_Jan geophysics:JanuaryMaximumGroundSurfaceTemperature

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:tmp_jan_us")
        (geospace:hasTransformation "self * 0.01")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7023)
        (geospace:hasYRangeMax 3103)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.937)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject tmp_jan geophysics:JanuaryMeanGroundSurfaceTemperature

  "Data source: University of East Anglia Climate Research Unit (New et al. 1998)"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.sage.wisc.edu/atlas/maps.php?catnum=3&type=Ecosystems")
    (measurement:unit "°C")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tmp_jan_global")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2154)
        (geospace:hasYRangeMax 853)
        (geospace:hasLatLowerBound -59.166)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.618)
        (geospace:hasLonUpperBound 180.052)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_lakes geofeatures:Lake

  "Data source: Derived from NLCD 2001"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetlake")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")
        (geospace:hasLatLowerBound 102131.5)
        (geospace:hasLonLowerBound 618565.375)
        (geospace:hasLatUpperBound 1367281.5)
        (geospace:hasLonUpperBound 2591186.75)))))


(defobject puget_pugetlakefront aestheticService:LakeFrontPresence

  "Data source: Derived from NLCD 2001"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetlakefront")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 76129.6295966863)
        (geospace:hasLonLowerBound 550339.3848230975)
        (geospace:hasLatUpperBound 1395057.6838687493)
        (geospace:hasLonUpperBound 2655014.7856741175)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject puget_levees infrastructure:Levee

  "Data source: Derived from King, Lewis, and Pierce county GIS point data"

  (measurement:BinaryCoding
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetlevees")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")
        (geospace:hasLatLowerBound 397939.875)
        (geospace:hasLonLowerBound 925800.75)
        (geospace:hasLatUpperBound 894250.8125)
        (geospace:hasLonUpperBound 1430108.875)))))


(defobject puget_ocean geofeatures:Ocean

  "Data source: Derived from NLCD 2001"

  (measurement:RankingSetRemapper
    (measurement:hasDefaultValue 0)
    (measurement:hasMapping "1:1")
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetocean")
        (geospace:hasValueAttribute "water_cd")
        (geospace:hasValueType "thinklab-core:Number")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")
        (geospace:hasLatLowerBound -92706.281)
        (geospace:hasLonLowerBound 478794.375)
        (geospace:hasLatUpperBound 1383059.75)
        (geospace:hasLonUpperBound 1449837.125)))))


(defobject FloodplainCanopy habitat:PercentFloodplainVegetationCover

  "Data source: Derived from NLCD 2001 tree canopy and FEMA Q3 flood data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "FEMA-NLCD_01_floodplain_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 128)
        (geospace:hasCoverageId "puget:floodplain_veg_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9466)
        (geospace:hasYRangeMax 7992)
        (geospace:hasLatLowerBound 45.85076076530745)
        (geospace:hasLonLowerBound -124.75135826799607)
        (geospace:hasLatUpperBound 48.999947174753444)
        (geospace:hasLonUpperBound -121.02149875778406)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_canopy_cover habitat:PercentVegetationCover

  "NLCD 2001 tree canopy data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:canopy_1")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 32069)
        (geospace:hasYRangeMax 25363)
        (geospace:hasLatLowerBound 40.121768696724)
        (geospace:hasLonLowerBound -127.24289)
        (geospace:hasLatUpperBound 50.1137582)
        (geospace:hasLonUpperBound -114.60911364887001)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject AfricaTreeCover habitat:PercentVegetationCover

  "University of Maryland Global Land Cover Facility"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://glcf.umiacs.umd.edu/index.shtml")
    (metadata:belongsToDataset "UMD_GLCF_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tree_cover_africa")
        (geospace:hasNodataValue 254.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9953)
        (geospace:hasYRangeMax 9559)
        (geospace:hasLatLowerBound -41.5)
        (geospace:hasLonLowerBound -26.375)
        (geospace:hasLatUpperBound 38.175)
        (geospace:hasLonUpperBound 56.583)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_impervious_surfaces habitat:PercentImperviousness

  "NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 33757)
        (geospace:hasYRangeMax 37544)
        (geospace:hasLatLowerBound 4441450.408428492)
        (geospace:hasLonLowerBound 184671.999278421)
        (geospace:hasLatUpperBound 5567800.408428492)
        (geospace:hasLonUpperBound 1197411.999278421)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject puget_pugetformalprotection puget:ProtectedStatus

  "Derived from national, state, and local public lands layers"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetformalprotection")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -551303.1220662754)
        (geospace:hasLonLowerBound 615871.2439393924)
        (geospace:hasLatUpperBound 778663.8395426599)
        (geospace:hasLonUpperBound 2705138.7912269174)
        (geospace:hasCoordinateReferenceSystem "EPSG:2926")))))


(defobject rfactor_1980 soilRetentionService:RainfallRunoffErosivityIndex

  "Data source: Yang et al. (2003)"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:rusle_rfactor")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_reservoir_storage_puget geofeatures:Reservoir

  "Data source: Derived from National Atlas of the United States, Major Dams of the United States"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://nationalatlas.gov/atlasftp.html?openChapters=chpwater")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:dam_storage_puget")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.670997355311954)
        (geospace:hasLonLowerBound -124.27)
        (geospace:hasLatUpperBound 49.01399658496675)
        (geospace:hasLonUpperBound -120.98400439562957)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject puget_pugetriverfront aestheticService:RiverFrontPresence

  "Data source: Derived from Washington DNR hydrography data"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fortress.wa.gov/dnr/app1/dataweb/dmmatrix.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetriverfront")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.539)
        (geospace:hasLonLowerBound -124.708)
        (geospace:hasLatUpperBound 49.015)
        (geospace:hasLonUpperBound -116.916)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_mgrivers geofeatures:River

  "Data source: Foiben-Taosarintanini Madagasikara (FTM)"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:mgrivers")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -34.687)
        (geospace:hasLonLowerBound -16.869)
        (geospace:hasLatUpperBound 37.247)
        (geospace:hasLonUpperBound 57.497)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject slope_stability_puget3 habitat:SlopeStability

  "Data source: Washington DNR"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fortress.wa.gov/dnr/app1/dataweb/dmmatrix.html")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:slope_stability_puget3")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12399)
        (geospace:hasYRangeMax 10381)
        (geospace:hasLatLowerBound 45.4826389)
        (geospace:hasLonLowerBound -124.864005)
        (geospace:hasLatUpperBound 49.0299355)
        (geospace:hasLonUpperBound -120.6272695)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject lsfactor_30min soilRetentionService:SlopeSteepnessAndLengthFactor

  "Data source: Yang et al. (2003)"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:rusle_lsfactor")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_rain_on_snow puget:SnowPrecipitationCategory

  "Data source: Washington DNR"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fortress.wa.gov/dnr/app1/dataweb/dmmatrix.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetrainonsnow")
        (geospace:hasValueAttribute "precip_sno")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")
        (geospace:hasLatLowerBound 84480.0)
        (geospace:hasLonLowerBound 605611.25)
        (geospace:hasLatUpperBound 1356334.875)
        (geospace:hasLonUpperBound 2551495.75)))))


(defobject SoilCarbFinal carbonService:SoilCarbonStorage

  "Data source: FAO Digital Soil Map of the World"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:soil_carbon_storage")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 4318)
        (geospace:hasYRangeMax 1684)
        (geospace:hasLatLowerBound -56.49999999999994)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.99994380000001)
        (geospace:hasLonUpperBound 179.99985599999985)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_soil_CN_ratio habitat:SoilCNRatio

  "Data source: Derived from FAO Digital Soil Map of the World"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:soil_cn_global")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 4319)
        (geospace:hasYRangeMax 1685)
        (geospace:hasLatLowerBound -56.5)
        (geospace:hasLonLowerBound -180)
        (geospace:hasLatUpperBound 84)
        (geospace:hasLonUpperBound 180)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject kfactor_30min habitat:SoilErodibility

  "Data source: Yang et al. (2003)"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:rusle_kfactor")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject annual_loss1980 soilRetentionService:SedimentSourceValueAnnual

  "Data source: Yang et al. (2003)"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:rusle_soil_loss")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_soil_ph habitat:SoilPhDeep

  "Data source: FAO Digital Soil Map of the World"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:soil_pH_30_100_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 719)
        (geospace:hasYRangeMax 359)
        (geospace:hasLatLowerBound -90)
        (geospace:hasLonLowerBound -180)
        (geospace:hasLatUpperBound 90)
        (geospace:hasLonUpperBound 180)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject soil_ph1 habitat:SoilPhShallow

  "Data source: FAO Digital Soil Map of the World"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:soil_pH_0_30_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_globalsoiltexture habitat:SoilTexture

  "Data source: Derived from FAO Digital Soil Map of the World"

  (observation:Categorization
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:globalsoiltexture")
        (geospace:hasValueAttribute "class")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -55.953)
        (geospace:hasLonLowerBound -180.042)
        (geospace:hasLatUpperBound 83.487)
        (geospace:hasLonUpperBound 179.944)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject successional_stage_puget ecology:SuccessionalStage

  "Data source: Interagency Vegetation Mapping Project"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.blm.gov/or/gis/data-details.php?data=ds000103")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:successional_stage_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12028)
        (geospace:hasYRangeMax 32020)
        (geospace:hasLatLowerBound 4635337.5)
        (geospace:hasLonLowerBound 365037.5)
        (geospace:hasLatUpperBound 5435887.5)
        (geospace:hasLonUpperBound 665787.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:26710")))))


(defobject SumHiWintLow habitat:SummerHighWinterLow

  "Data source: Derived from WorldClim data"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.worldclim.org/bioclim")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:sum_hi_wint_lo_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 17998)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject US_summer_high_winter_low habitat:SummerHighWinterLow

  "Data source: Derived from PRISM climate data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:sum_hi_wint_lo_us")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7024)
        (geospace:hasYRangeMax 3104)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.937)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject npp_modis1km carbonService:VegetationAndSoilCarbonSequestration

  "Data source: University of Montana/NASA MODIS NPP"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.ntsg.umt.edu/modis/")
    (measurement:unit "t/ha*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:npp_modis")
        (geospace:hasTransformation "self * 0.001")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 42542)
        (geospace:hasYRangeMax 16543)
        (geospace:hasLatLowerBound -60.001944101171546)
        (geospace:hasLonLowerBound -179.99999999999991)
        (geospace:hasLatUpperBound 79.99999999999997)
        (geospace:hasLonUpperBound 180.0025814348892)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NBCD_MZ01_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_1")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 19517)
        (geospace:hasYRangeMax 19296)
        (geospace:hasLatLowerBound 44.588)
        (geospace:hasLonLowerBound -126.315)
        (geospace:hasLatUpperBound 49.938)
        (geospace:hasLonUpperBound -118.438)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_10 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_10")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 15302)
        (geospace:hasYRangeMax 14799)
        (geospace:hasLatLowerBound 43.1276658)
        (geospace:hasLonLowerBound -119.5285295)
        (geospace:hasLatUpperBound 48.9993503)
        (geospace:hasLonUpperBound -113.4573005)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_12 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_12")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16540)
        (geospace:hasYRangeMax 13920)
        (geospace:hasLatLowerBound 36.7719583)
        (geospace:hasLonLowerBound -120.7770254)
        (geospace:hasLatUpperBound 41.8994135)
        (geospace:hasLonUpperBound -114.6846275)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_13 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_13")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14607)
        (geospace:hasYRangeMax 14015)
        (geospace:hasLatLowerBound 32.6431476)
        (geospace:hasLonLowerBound -118.4293122)
        (geospace:hasLatUpperBound 37.5973817)
        (geospace:hasLonUpperBound -113.2658389)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_14 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_14")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12452)
        (geospace:hasYRangeMax 12615)
        (geospace:hasLatLowerBound 31.5250622)
        (geospace:hasLonLowerBound -114.8219599)
        (geospace:hasLatUpperBound 35.8860825)
        (geospace:hasLonUpperBound -110.5172799)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_16 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_16")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12395)
        (geospace:hasYRangeMax 15307)
        (geospace:hasLatLowerBound 37.1510659)
        (geospace:hasLonLowerBound -113.6144637)
        (geospace:hasLatUpperBound 42.6146638)
        (geospace:hasLonUpperBound -109.1901235)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_17 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_17")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10990)
        (geospace:hasYRangeMax 14002)
        (geospace:hasLatLowerBound 37.0757629)
        (geospace:hasLonLowerBound -115.5458755)
        (geospace:hasLatUpperBound 42.1578552)
        (geospace:hasLonUpperBound -111.5568468)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_18 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_18")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17682)
        (geospace:hasYRangeMax 9684)
        (geospace:hasLatLowerBound 41.0926224)
        (geospace:hasLonLowerBound -117.5982095)
        (geospace:hasLatUpperBound 44.6940488)
        (geospace:hasLonUpperBound -111.022985)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_19 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_19")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13296)
        (geospace:hasYRangeMax 14192)
        (geospace:hasLatLowerBound 43.5922497)
        (geospace:hasLonLowerBound -114.8631596)
        (geospace:hasLatUpperBound 48.9924791)
        (geospace:hasLonUpperBound -109.8038212)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_2 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_2")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5711)
        (geospace:hasYRangeMax 10558)
        (geospace:hasLatLowerBound 41.9877283)
        (geospace:hasLonLowerBound -124.6796725)
        (geospace:hasLatUpperBound 46.2988575)
        (geospace:hasLonUpperBound -122.3473352)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_20 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_20")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 19976)
        (geospace:hasYRangeMax 10079)
        (geospace:hasLatLowerBound 45.2654009)
        (geospace:hasLonLowerBound -113.2721254)
        (geospace:hasLatUpperBound 48.9985348)
        (geospace:hasLonUpperBound -105.8739953)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_21 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_21")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10062)
        (geospace:hasYRangeMax 10764)
        (geospace:hasLatLowerBound 41.9058916)
        (geospace:hasLonLowerBound -112.3392729)
        (geospace:hasLatUpperBound 45.857279)
        (geospace:hasLonUpperBound -108.6455368)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_22 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_22")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17677)
        (geospace:hasYRangeMax 13963)
        (geospace:hasLatLowerBound 40.5758792)
        (geospace:hasLonLowerBound -111.4510311)
        (geospace:hasLatUpperBound 45.5259745)
        (geospace:hasLonUpperBound -105.184455)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_23 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_23")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20125)
        (geospace:hasYRangeMax 12747)
        (geospace:hasLatLowerBound 36.5483604)
        (geospace:hasLonLowerBound -113.4913143)
        (geospace:hasLatUpperBound 41.006217)
        (geospace:hasLonUpperBound -106.4536423)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_24 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_24")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 15600)
        (geospace:hasYRangeMax 13806)
        (geospace:hasLatLowerBound 32.7701757)
        (geospace:hasLonLowerBound -112.1009215)
        (geospace:hasLatUpperBound 37.4321601)
        (geospace:hasLonUpperBound -106.8332303)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_26 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_26")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17149)
        (geospace:hasYRangeMax 15874)
        (geospace:hasLatLowerBound 28.97151)
        (geospace:hasLonLowerBound -106.164427)
        (geospace:hasLatUpperBound 33.9686925)
        (geospace:hasLonUpperBound -100.7659213)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_27 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_27")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13434)
        (geospace:hasYRangeMax 19503)
        (geospace:hasLatLowerBound 33.2124474)
        (geospace:hasLonLowerBound -106.5159107)
        (geospace:hasLatUpperBound 39.6486596)
        (geospace:hasLonUpperBound -102.0823322)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_28 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_28")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13747)
        (geospace:hasYRangeMax 18318)
        (geospace:hasLatLowerBound 35.4328682)
        (geospace:hasLonLowerBound -108.8826938)
        (geospace:hasLatUpperBound 41.7019149)
        (geospace:hasLonUpperBound -104.177829)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_29 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_29")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17223)
        (geospace:hasYRangeMax 17121)
        (geospace:hasLatLowerBound 41.1002757)
        (geospace:hasLonLowerBound -109.2268953)
        (geospace:hasLatUpperBound 47.1298514)
        (geospace:hasLonUpperBound -103.161402)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_3 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_3")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5719)
        (geospace:hasYRangeMax 11033)
        (geospace:hasLatLowerBound 37.4489608)
        (geospace:hasLonLowerBound -124.4597134)
        (geospace:hasLatUpperBound 42.0273627)
        (geospace:hasLonUpperBound -122.0860807)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_30 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_30")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 18628)
        (geospace:hasYRangeMax 13570)
        (geospace:hasLatLowerBound 44.2491164)
        (geospace:hasLonLowerBound -106.7862484)
        (geospace:hasLatUpperBound 49.0037246)
        (geospace:hasLonUpperBound -100.2596974)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_31 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_31")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 19983)
        (geospace:hasYRangeMax 12493)
        (geospace:hasLatLowerBound 40.6520852)
        (geospace:hasLonLowerBound -104.1747711)
        (geospace:hasLatUpperBound 44.7801668)
        (geospace:hasLonUpperBound -97.5721532)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_32 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_32")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16403)
        (geospace:hasYRangeMax 19886)
        (geospace:hasLatLowerBound 31.150383)
        (geospace:hasLonLowerBound -99.8127233)
        (geospace:hasLatUpperBound 37.1995325)
        (geospace:hasLonUpperBound -94.8229658)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_33 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_33")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16810)
        (geospace:hasYRangeMax 15676)
        (geospace:hasLatLowerBound 37.0058258)
        (geospace:hasLonLowerBound -105.2986901)
        (geospace:hasLatUpperBound 42.155558)
        (geospace:hasLonUpperBound -99.7764743)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_34 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_34")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16665)
        (geospace:hasYRangeMax 17847)
        (geospace:hasLatLowerBound 31.8620083)
        (geospace:hasLonLowerBound -103.9904521)
        (geospace:hasLatUpperBound 37.4767288)
        (geospace:hasLonUpperBound -98.7475507)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_35 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_35")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 15131)
        (geospace:hasYRangeMax 15638)
        (geospace:hasLatLowerBound 28.9029537)
        (geospace:hasLonLowerBound -101.3527782)
        (geospace:hasLatUpperBound 33.6183954)
        (geospace:hasLonUpperBound -96.7901965)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_36 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_36")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20654)
        (geospace:hasYRangeMax 19099)
        (geospace:hasLatLowerBound 25.8372052)
        (geospace:hasLonLowerBound -100.7988932)
        (geospace:hasLatUpperBound 31.4892981)
        (geospace:hasLonUpperBound -94.6866671)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_37a carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_37a")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16380)
        (geospace:hasYRangeMax 17988)
        (geospace:hasLatLowerBound 29.3222777)
        (geospace:hasLonLowerBound -96.4217808)
        (geospace:hasLatUpperBound 34.7309668)
        (geospace:hasLonUpperBound -91.4965365)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_37b carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_37b")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13069)
        (geospace:hasYRangeMax 8586)
        (geospace:hasLatLowerBound 28.8793334)
        (geospace:hasLonLowerBound -92.7683872)
        (geospace:hasLatUpperBound 31.5138851)
        (geospace:hasLonUpperBound -88.7585798)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_38 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_38")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 18041)
        (geospace:hasYRangeMax 20263)
        (geospace:hasLatLowerBound 36.4552155)
        (geospace:hasLonLowerBound -100.6793377)
        (geospace:hasLatUpperBound 42.8691707)
        (geospace:hasLonUpperBound -94.9686546)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_39 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_39")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20669)
        (geospace:hasYRangeMax 12881)
        (geospace:hasLatLowerBound 42.4758393)
        (geospace:hasLonLowerBound -100.7256511)
        (geospace:hasLatUpperBound 46.6956169)
        (geospace:hasLonUpperBound -93.9549437)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_40 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_40")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 24256)
        (geospace:hasYRangeMax 8554)
        (geospace:hasLatLowerBound 46.0803882)
        (geospace:hasLonLowerBound -103.6625399)
        (geospace:hasLatUpperBound 49.0087661)
        (geospace:hasLonUpperBound -95.3599928)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_41 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_41")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 25251)
        (geospace:hasYRangeMax 16305)
        (geospace:hasLatLowerBound 43.8682383)
        (geospace:hasLonLowerBound -96.3385864)
        (geospace:hasLatUpperBound 49.3888488)
        (geospace:hasLonUpperBound -87.7893759)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_42 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_42")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20371)
        (geospace:hasYRangeMax 15425)
        (geospace:hasLatLowerBound 39.822832)
        (geospace:hasLonLowerBound -96.4489828)
        (geospace:hasLatUpperBound 44.8114612)
        (geospace:hasLonUpperBound -89.8609656)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_43 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_43")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 18567)
        (geospace:hasYRangeMax 17484)
        (geospace:hasLatLowerBound 36.4787292)
        (geospace:hasLonLowerBound -96.8169382)
        (geospace:hasLatUpperBound 41.9857276)
        (geospace:hasLonUpperBound -90.9688624)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_44 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_44")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 22070)
        (geospace:hasYRangeMax 20367)
        (geospace:hasLatLowerBound 33.3522532)
        (geospace:hasLonLowerBound -96.0661771)
        (geospace:hasLatUpperBound 39.6975411)
        (geospace:hasLonUpperBound -89.190376)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_45 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_45")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10444)
        (geospace:hasYRangeMax 19675)
        (geospace:hasLatLowerBound 31.1066201)
        (geospace:hasLonLowerBound -92.4426123)
        (geospace:hasLatUpperBound 37.4048822)
        (geospace:hasLonUpperBound -89.0990312)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_46 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_46")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 23587)
        (geospace:hasYRangeMax 19873)
        (geospace:hasLatLowerBound 29.5815123)
        (geospace:hasLonLowerBound -91.6501091)
        (geospace:hasLatUpperBound 35.9031398)
        (geospace:hasLonUpperBound -84.1471723)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_47 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_47")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20981)
        (geospace:hasYRangeMax 15111)
        (geospace:hasLatLowerBound 35.0481773)
        (geospace:hasLonLowerBound -89.8585436)
        (geospace:hasLatUpperBound 40.0653996)
        (geospace:hasLonUpperBound -82.8925954)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_48 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_48")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13600)
        (geospace:hasYRangeMax 11759)
        (geospace:hasLatLowerBound 32.6999067)
        (geospace:hasLonLowerBound -88.3947023)
        (geospace:hasLatUpperBound 36.540624)
        (geospace:hasLonUpperBound -83.9527809)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_49 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_49")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16854)
        (geospace:hasYRangeMax 18877)
        (geospace:hasLatLowerBound 36.8058701)
        (geospace:hasLonLowerBound -91.7345444)
        (geospace:hasLatUpperBound 43.077317)
        (geospace:hasLonUpperBound -86.1351213)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_5 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_5")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10292)
        (geospace:hasYRangeMax 13796)
        (geospace:hasLatLowerBound 34.9694382)
        (geospace:hasLonLowerBound -122.832336)
        (geospace:hasLatUpperBound 40.6415538)
        (geospace:hasLonUpperBound -118.6006533)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_50 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_50")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20466)
        (geospace:hasYRangeMax 14435)
        (geospace:hasLatLowerBound 41.8357078)
        (geospace:hasLonLowerBound -92.9878299)
        (geospace:hasLatUpperBound 46.7346189)
        (geospace:hasLonUpperBound -86.0424179)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_51 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_51")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 28098)
        (geospace:hasYRangeMax 20474)
        (geospace:hasLatLowerBound 41.1442394)
        (geospace:hasLonLowerBound -91.0367452)
        (geospace:hasLatUpperBound 48.3298937)
        (geospace:hasLonUpperBound -81.1755964)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_52 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_52")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 15726)
        (geospace:hasYRangeMax 10105)
        (geospace:hasLatLowerBound 38.9221042)
        (geospace:hasLonLowerBound -87.5713765)
        (geospace:hasLatUpperBound 42.4003813)
        (geospace:hasLonUpperBound -82.1586584)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_53 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_53")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16401)
        (geospace:hasYRangeMax 13446)
        (geospace:hasLatLowerBound 35.1620397)
        (geospace:hasLonLowerBound -86.0717556)
        (geospace:hasLatUpperBound 39.7397836)
        (geospace:hasLonUpperBound -80.4881198)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_54 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_54")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 19815)
        (geospace:hasYRangeMax 12246)
        (geospace:hasLatLowerBound 31.9413937)
        (geospace:hasLonLowerBound -86.9486512)
        (geospace:hasLatUpperBound 36.0379774)
        (geospace:hasLonUpperBound -80.3204671)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_55 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_55")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17187)
        (geospace:hasYRangeMax 16066)
        (geospace:hasLatLowerBound 28.5639303)
        (geospace:hasLonLowerBound -85.5297685)
        (geospace:hasLatUpperBound 33.8652941)
        (geospace:hasLonUpperBound -79.8585498)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_56 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_56")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12839)
        (geospace:hasYRangeMax 16361)
        (geospace:hasLatLowerBound 24.0737982)
        (geospace:hasLonLowerBound -83.5748126)
        (geospace:hasLatUpperBound 29.4754331)
        (geospace:hasLonUpperBound -79.3358349)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_57 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_57")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 15485)
        (geospace:hasYRangeMax 13507)
        (geospace:hasLatLowerBound 33.5563999)
        (geospace:hasLonLowerBound -84.9148355)
        (geospace:hasLatUpperBound 38.1471853)
        (geospace:hasLonUpperBound -79.6518632)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_58 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_58")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20497)
        (geospace:hasYRangeMax 16635)
        (geospace:hasLatLowerBound 31.2836704)
        (geospace:hasLonLowerBound -82.2718665)
        (geospace:hasLatUpperBound 37.0270589)
        (geospace:hasLonUpperBound -75.1952468)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_59 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_59")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14650)
        (geospace:hasYRangeMax 13128)
        (geospace:hasLatLowerBound 34.0819793)
        (geospace:hasLonLowerBound -82.2524644)
        (geospace:hasLatUpperBound 38.6527196)
        (geospace:hasLonUpperBound -77.1518942)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_6 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_6")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9983)
        (geospace:hasYRangeMax 14741)
        (geospace:hasLatLowerBound 34.8740225)
        (geospace:hasLonLowerBound -121.9996387)
        (geospace:hasLatUpperBound 40.9803575)
        (geospace:hasLonUpperBound -117.8639977)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_60 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_60")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 13122)
        (geospace:hasYRangeMax 15290)
        (geospace:hasLatLowerBound 35.5362103)
        (geospace:hasLonLowerBound -78.6371181)
        (geospace:hasLatUpperBound 41.2181809)
        (geospace:hasLonUpperBound -73.7607003)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_61 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_61")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 18263)
        (geospace:hasYRangeMax 15744)
        (geospace:hasLatLowerBound 36.3413602)
        (geospace:hasLonLowerBound -81.5844591)
        (geospace:hasLatUpperBound 41.9832615)
        (geospace:hasLonUpperBound -75.0399826)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_62 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_62")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14300)
        (geospace:hasYRangeMax 13278)
        (geospace:hasLatLowerBound 37.8786887)
        (geospace:hasLonLowerBound -83.5954114)
        (geospace:hasLatUpperBound 42.5909012)
        (geospace:hasLonUpperBound -78.5205573)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_63 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_63")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16570)
        (geospace:hasYRangeMax 11046)
        (geospace:hasLatLowerBound 40.5694545)
        (geospace:hasLonLowerBound -80.8253407)
        (geospace:hasLatUpperBound 44.6427814)
        (geospace:hasLonUpperBound -74.7153503)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_64 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_64")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12573)
        (geospace:hasYRangeMax 13385)
        (geospace:hasLatLowerBound 40.3086799)
        (geospace:hasLonLowerBound -77.3735384)
        (geospace:hasLatUpperBound 45.4937073)
        (geospace:hasLonUpperBound -72.5030134)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_7 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_7")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8583)
        (geospace:hasYRangeMax 12521)
        (geospace:hasLatLowerBound 40.631527)
        (geospace:hasLonLowerBound -123.3721999)
        (geospace:hasLatUpperBound 45.7236484)
        (geospace:hasLonUpperBound -119.8813541)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_8 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_8")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12277)
        (geospace:hasYRangeMax 10314)
        (geospace:hasLatLowerBound 44.9161824)
        (geospace:hasLonLowerBound -121.227661)
        (geospace:hasLatUpperBound 49.0022563)
        (geospace:hasLonUpperBound -116.3640605)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject biomass_9 carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_9")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14731)
        (geospace:hasYRangeMax 13983)
        (geospace:hasLatLowerBound 41.1661439)
        (geospace:hasLonLowerBound -121.5081676)
        (geospace:hasLatUpperBound 46.5174198)
        (geospace:hasLonUpperBound -115.8706741)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject c_1km_recl carbonService:VegetationCarbonStorage

  "Data source: ORNL-CDIAC Global Biomass Carbon Map for the Year 2000"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://cdiac.ornl.gov/epubs/ndp/global_carbon/carbon_documentation.html")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:biomass_carbon_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 40319)
        (geospace:hasYRangeMax 20159)
        (geospace:hasLatLowerBound -90.00000000000003)
        (geospace:hasLonLowerBound -180.0044642857)
        (geospace:hasLatUpperBound 90.00892799539997)
        (geospace:hasLonUpperBound 180.00446313370003)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject vegHt_Project habitat:VegetationHeight

  "Data source: Derived from Puget Sound LIDAR Consortium data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://pugetsoundlidar.ess.washington.edu/")
    (measurement:unit "ft")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:veg_ht_puget")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2915)
        (geospace:hasYRangeMax 447)
        (geospace:hasLatLowerBound 46.608)
        (geospace:hasLonLowerBound -124.3)
        (geospace:hasLatUpperBound 47.6)
        (geospace:hasLonUpperBound -121.996)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_pugetwaterquality aestheticService:WaterQualityAssessment

  "Data source: Washington Department of Ecology 303(d) Water Quality Assessment"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ecy.wa.gov/services/gis/index.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetwaterquality")
        (geospace:hasValueAttribute "quality")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 54692.530961369725)
        (geospace:hasLonLowerBound 545336.7573808122)
        (geospace:hasLatUpperBound 1380301.6967513857)
        (geospace:hasLonUpperBound 2599982.619737681)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject wetlands habitat:Wetland

  "Data source: Derived from Foiben-Taosarintanini Madagasikara (FTM) land cover map"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:wetlands_mg")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 29719)
        (geospace:hasYRangeMax 54068)
        (geospace:hasLatLowerBound 7135327.75)
        (geospace:hasLonLowerBound 266392.5)
        (geospace:hasLatUpperBound 8703357.75)
        (geospace:hasLonUpperBound 1128301.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:32738")))))

(defobject SumHiWintLowHadleyA2 habitat:SummerHighWinterLow

  "Data source: NCAR GIS Climate Change Scenarios"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.gisclimatechange.org/")
    (observation:hasScenario
      (carbonService:IPCCHadleyA2))
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:sum_hi_wint_lo_hadley_A2")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14998)
        (geospace:hasYRangeMax 7930)
        (geospace:hasLatLowerBound 13.9)
        (geospace:hasLonLowerBound -177.0)
        (geospace:hasLatUpperBound 80.0)
        (geospace:hasLonUpperBound -52.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject SumHiWintLowHadleyB2 habitat:SummerHighWinterLow

  "Data source: NCAR GIS Climate Change Scenarios"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.gisclimatechange.org/")
    (observation:hasScenario
      (carbonService:IPCCHadleyB2))
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:sum_hi_wint_lo_hadley_B2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14998)
        (geospace:hasYRangeMax 7930)
        (geospace:hasLatLowerBound 13.9)
        (geospace:hasLonLowerBound -177.0)
        (geospace:hasLatUpperBound 80.0)
        (geospace:hasLonUpperBound -52.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))

(defobject usa_airports infrastructure:Airport

  "Data source: National Transportation Atlas Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:airports")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 18.004)
        (geospace:hasLonLowerBound -171.744)
        (geospace:hasLatUpperBound 71.289)
        (geospace:hasLonUpperBound -52.723)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_amphibian_rich habitat:AmphibianRichness

  "Data source: University of Arizona-USGS Southern Arizona Data Services Program"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://sdrsnet.srnr.arizona.edu/index.php?page=datamenu")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:amphibian_rich")
        (geospace:hasValueAttribute "grid_code")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414974.363269456)
        (geospace:hasLonLowerBound 114877.97304113029)
        (geospace:hasLatUpperBound 4106788.5249591796)
        (geospace:hasLonUpperBound 699436.3793787669)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject usa_meandaysprecipannual habitat:AnnualDaysOfPrecipitation

  "Data source: NOAA-National Climatic Data Center"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:meandaysprecipannual")
        (geospace:hasValueAttribute "gridcode")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24.463)
        (geospace:hasLonLowerBound -124.802)
        (geospace:hasLatUpperBound 49.425)
        (geospace:hasLonUpperBound -66.935)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject california_infiltration_wch habitat:AnnualInfiltration

  "Data source: Mean annual infiltration, 1951-2002, modeled
  using Los Angeles Basin Groundwater Augmentation Model Version 4.1.10"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:infiltration_wch")
        (geospace:hasValueAttribute "totavgann")
        (geospace:hasTransformation "self * 25.4")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3751022.4715446113)
        (geospace:hasLonLowerBound 409664.2808525585)
        (geospace:hasLatUpperBound 3752274.098726226)
        (geospace:hasLonUpperBound 413096.65288435936)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject PrecipAvgYearly habitat:AnnualPrecipitation

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:annual_precip_us")
        (geospace:hasTransformation "self * 0.01")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7023)
        (geospace:hasYRangeMax 3103)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.937)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject bio_12 habitat:AnnualPrecipitation

  "Data source: WorldClim"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.worldclim.org/bioclim")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:annual_precip_global")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 17998)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject 2002_wgs84 habitat:AnnualPrecipitation2002

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:precip_2002")
        (geospace:hasTransformation "self * 0.01")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 1403)
        (geospace:hasYRangeMax 619)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.938)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject 2007_wgs84 habitat:AnnualPrecipitation2007

  "Data source: PRISM climate data"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.prism.oregonstate.edu/products/")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:precip_2007")
        (geospace:hasTransformation "self * 0.01")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 1403)
        (geospace:hasYRangeMax 619)
        (geospace:hasLatLowerBound 24.062)
        (geospace:hasLonLowerBound -125.021)
        (geospace:hasLatUpperBound 49.938)
        (geospace:hasLonUpperBound -66.479)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject groundwater_recharge habitat:AnnualRecharge

  "Data source: USGS WRD NDSI"

  (measurement:Measurement
    (measurement:unit "mm")
    (metadata:hasPriority 5)
    (metadata:hasURL "http://water.usgs.gov/lookup/getgislist")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:gw_recharge_us")
        (geospace:hasNodataValue -32768.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5398)
        (geospace:hasYRangeMax 2481)
        (geospace:hasLatLowerBound 22.851)
        (geospace:hasLonLowerBound -127.888)
        (geospace:hasLatUpperBound 51.609)
        (geospace:hasLonUpperBound -65.346)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject california_percolation_wch habitat:AnnualRecharge

  "Data source: Mean annual deep percolation, 1951-2002, modeled
  using Los Angeles Basin Groundwater Augmentation Model Version 4.1.10"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:percolation_wch")
        (geospace:hasValueAttribute "totavgann")
        (geospace:hasTransformation "self * 25.4")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3751022.4715446113)
        (geospace:hasLonLowerBound 409664.2808525585)
        (geospace:hasLatUpperBound 3752274.098726226)
        (geospace:hasLonUpperBound 413096.65288435936)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject california_runoff_wch habitat:AnnualRunoff

  "Data source: Mean annual runoff, 1951-2002, modeled
  using Los Angeles Basin Groundwater Augmentation Model Version 4.1.10"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:runoff_wch")
        (geospace:hasValueAttribute "totavgann")
        (geospace:hasTransformation "self * 25.4")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3751022.4715446113)
        (geospace:hasLonLowerBound 409664.2808525585)
        (geospace:hasLatUpperBound 3752274.098726226)
        (geospace:hasLonUpperBound 413096.65288435936)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject snowmelt_annual habitat:AnnualSnowmelt

  "Data source: University of Delaware Global Water Balance Archive"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://climate.geog.udel.edu/~climate/html_pages/README.wb_ts2.html")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:snowmelt_annual")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 520)
        (geospace:hasYRangeMax 250)
        (geospace:hasLatLowerBound -90.095)
        (geospace:hasLonLowerBound -180.095)
        (geospace:hasLatUpperBound 83.785)
        (geospace:hasLonUpperBound 180.085)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject housing_value_san_pedro3 economics:AppraisedPropertyValue

  "Data source: Derived from Pima and Pinal Co., AZ parcel data. Calculated property value/ac."

  (measurement:Ranking
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:housing_value_san_pedro3")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9867)
        (geospace:hasYRangeMax 6938)
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -113.371172)
        (geospace:hasLatUpperBound 33.4672157)
        (geospace:hasLonUpperBound -109.872)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject housing_value_puget3 economics:AppraisedPropertyValue

  "Data source: County assessors' offices"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:housing_value_puget3")
        (geospace:hasNodataValue 2.147483647E9)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8819)
        (geospace:hasYRangeMax 4633)
        (geospace:hasLatLowerBound 46.7618805)
        (geospace:hasLonLowerBound -124.243488)
        (geospace:hasLatUpperBound 48.2992335)
        (geospace:hasLonUpperBound -121.3177078)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_bird_rich habitat:AvianRichness

  "Data source: University of Arizona-USGS Southern Arizona Data Services Program"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://sdrsnet.srnr.arizona.edu/index.php?page=datamenu")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:bird_rich")
        (geospace:hasValueAttribute "grid_code")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414974.349525294)
        (geospace:hasLonLowerBound 114973.68290087063)
        (geospace:hasLatUpperBound 4106562.7324075284)
        (geospace:hasLonUpperBound 699340.7518807739)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject sanPedro_bsr_development_sites2 sanPedro:BSRDevelopment

  "Data source: Developed by Entrix for 2010 BSR roundtable on ecosystem services"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:bsr_development_sites2")
        (geospace:hasValueAttribute "site")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -111.012)
        (geospace:hasLatUpperBound 33.281)
        (geospace:hasLonUpperBound -109.7945359)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject buffalo waterSupplyService:BuffaloPopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:buffalo")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3469)
        (geospace:hasLatLowerBound -89.9)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.65)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject cattle waterSupplyService:CattlePopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:cattle")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3469)
        (geospace:hasLatLowerBound -89.9)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.65)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mexico_mex_lulc mexico:CONABIOLULCCategory

  "Data source: Comision Nacional Para el Conocimiento y Uso de la Biodiversidad (CONABIO)"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.conabio.gob.mx/informacion/gis/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mexico:mex_lulc")
        (geospace:hasValueAttribute "grupo_fina")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 14.531)
        (geospace:hasLonLowerBound -117.302)
        (geospace:hasLatUpperBound 33.285)
        (geospace:hasLonUpperBound -86.71)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mexico_lalulc veracruz-lulc:VeracruzLULCCategory
  (observation:Categorization
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mexico:lalulc")
        (geospace:hasValueAttribute "tipo")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 2099767.0934606227)
        (geospace:hasLonLowerBound 673608.6731989896)
        (geospace:hasLatUpperBound 2187949.8171524717)
        (geospace:hasLonUpperBound 817055.4370278353)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject usa_pugeterosionmetrics soilRetentionService:ConservationPractice

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_p")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject global_nationalboundaries geofeatures:Country
  (observation:Categorization
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:nationalboundaries")
        (geospace:hasValueAttribute "cntry_name")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.624)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_pugeterosionmetrics soilRetentionService:CoverManagement

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_c")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject snowmelt_dec habitat:DecemberSnowmelt

  "Data source: University of Delaware Global Water Balance Archive"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://climate.geog.udel.edu/~climate/html_pages/README.wb_ts2.html")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:snowmelt_dec")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 520)
        (geospace:hasYRangeMax 250)
        (geospace:hasLatLowerBound -90.095)
        (geospace:hasLonLowerBound -180.095)
        (geospace:hasLatUpperBound 83.785)
        (geospace:hasLonUpperBound 180.085)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject DepthToWaterSanPedro waterSupplyService:DepthToWaterTable

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_depth_to_water_table")
    (measurement:unit "cm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:depth_to_water_san_pedro")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8712)
        (geospace:hasYRangeMax 9186)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 424094.614)
        (geospace:hasLatUpperBound 3742078.5)
        (geospace:hasLonUpperBound 685514.614)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject DepthToWater_puget waterSupplyService:DepthToWaterTable

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_depth_to_water_table")
    (measurement:unit "cm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:depth_to_water_puget")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject DR_lulc domlulc:DOMLULCNumeric
  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "dominican:LULC_dr")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 4370)
        (geospace:hasYRangeMax 3978)
        (geospace:hasLatLowerBound 2086100.724)
        (geospace:hasLonLowerBound 213798.438)
        (geospace:hasLatUpperBound 2205500.724)
        (geospace:hasLonUpperBound 344958.438)
        (geospace:hasCoordinateReferenceSystem "EPSG:32619")))))


(defobject dominicanRepublic_coffee_farms soilRetentionService:FarmlandCode
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "dominican:coffee_farms")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 2101683.8885231065)
        (geospace:hasLonLowerBound 304735.55190355214)
        (geospace:hasLatUpperBound 2125545.5312832817)
        (geospace:hasLonUpperBound 334007.45222430024)
        (geospace:hasCoordinateReferenceSystem "EPSG:32619")))))


(defobject Reclass_fire_Clip habitat:FireReturnInterval

  "Data source: Derived from SWReGAP vegetation type and TNC historic fire return interval data"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 128.0)
        (geospace:hasCoverageId "sanPedro:fire_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 51935)
        (geospace:hasYRangeMax 30893)
        (geospace:hasLatLowerBound 30.868)
        (geospace:hasLonLowerBound -120.016)
        (geospace:hasLatUpperBound 42.022)
        (geospace:hasLonUpperBound -102.017)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Reclass_fthr1 habitat:FireThreat

  "Data source: California Fire and Resource Assessment Program"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://frap.cdf.ca.gov/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 128.0)
        (geospace:hasCoverageId "california:fire_threat_ca")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 11887)
        (geospace:hasYRangeMax 9570)
        (geospace:hasLatLowerBound 32.324)
        (geospace:hasLonLowerBound -125.408)
        (geospace:hasLatUpperBound 42.403)
        (geospace:hasLonUpperBound -112.889)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject california_californiafloodplain geofeatures:Floodplain

  "Data source: FEMA Q3 flood data"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "FEMA_floodplain_extents")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:ca_floodplain")
        (geospace:hasValueAttribute "zone_")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 32.61800083909326)
        (geospace:hasLonLowerBound -124.40900313774756)
        (geospace:hasLatUpperBound 42.00899872854834)
        (geospace:hasLonUpperBound -114.13100536780148)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject FloodplainWidth habitat:FloodplainWidth

  "Data source: Derived from FEMA Q3 flood data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "Calculated_FEMA_floodplain_width")
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:floodplain_width_puget")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 1836)
        (geospace:hasYRangeMax 1573)
        (geospace:hasLatLowerBound 45.851)
        (geospace:hasLonLowerBound -124.696)
        (geospace:hasLatUpperBound 49.001)
        (geospace:hasLonUpperBound -121.02)
        (geospace:hasCoordinateReferenceSystem "EPSG:4267")))))


(defobject floodplain_width habitat:FloodplainWidth

  "Data source: Derived from FEMA Q3 flood data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "Calculated_FEMA_floodplain_width")
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "california:floodplain_width_ca")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3310)
        (geospace:hasYRangeMax 3125)
        (geospace:hasLatLowerBound 32.627)
        (geospace:hasLonLowerBound -124.403)
        (geospace:hasLatUpperBound 42.008)
        (geospace:hasLonUpperBound -114.467)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject global_globalfloodplains geofeatures:Floodplain

  "Data source: Dartmouth Flood Observatory"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.dartmouth.edu/~floods/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:globalfloodplains")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -25.607)
        (geospace:hasLonLowerBound -84.928)
        (geospace:hasLatUpperBound 23.331)
        (geospace:hasLonUpperBound 50.484)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject gambels_240 sanPedro:GambelsQuailHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:gambels")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject landcover_glc2000 glc:GLCNumeric

  "Data source: GLC 2000 Project"

  (measurement:NumericCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://bioval.jrc.ec.europa.eu/products/glc2000/glc2000.php")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:landcover_glc2000")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 40318)
        (geospace:hasYRangeMax 20159)
        (geospace:hasLatLowerBound -90.004)
        (geospace:hasLonLowerBound -179.996)
        (geospace:hasLatUpperBound 90.004)
        (geospace:hasLonUpperBound 180.004)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject goats waterSupplyService:GoatsPopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:goats")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3469)
        (geospace:hasLatLowerBound -89.9)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.65)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_mex_highway infrastructure:Highway

  "Data source: U.S. EPA San Pedro Data Browser"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/san_pedro/")
    (metadata:belongsToDataset "highway-presence")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:mex_highway")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.534577496)
        (geospace:hasLonLowerBound 498087.4793240902)
        (geospace:hasLatUpperBound 3683452.407758169)
        (geospace:hasLonUpperBound 608027.6973406251)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject usa_highways infrastructure:Highway

  "Data source: TIGER/Line 2000 Streets"

  (measurement:BinaryCoding
    (metadata:belongsToDataset "highway-presence")
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:highways")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 19.058)
        (geospace:hasLonLowerBound -166.557)
        (geospace:hasLatUpperBound 71.296)
        (geospace:hasLonUpperBound -66.98)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject housing_views_puget aestheticService:HomeownerViewUse

  "Data source: King County assessors' offices"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:housing_views_puget")
        (geospace:hasNodataValue 65535.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 423)
        (geospace:hasYRangeMax 244)
        (geospace:hasLatLowerBound 47.139)
        (geospace:hasLonLowerBound -122.528)
        (geospace:hasLatUpperBound 47.778)
        (geospace:hasLonUpperBound -121.423)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_hydroreservoirs2 soilRetentionService:HydroelectricUseLevel

  "Data source: National Atlas of the United States, Major Dams of the United States"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://nationalatlas.gov/atlasftp.html?openChapters=chpwater")
    (measurement:unit "m^3")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:hydroreservoirs2")
        (geospace:hasValueAttribute "normal_s_1")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.95599729013428)
        (geospace:hasLonLowerBound -123.60800381860865)
        (geospace:hasLatUpperBound 49.01399658496675)
        (geospace:hasLonUpperBound -120.98400441810807)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject HydroDams soilRetentionService:HydroelectricUseLevel

  "Data source: FAO AQUASTAT"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.fao.org/nr/water/aquastat/gis/index2.stm")
    (measurement:unit "m^3")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:hydro_dams_africa")
        (geospace:hasNodataValue -3.4028234663852886E38)
        (geospace:hasNodataValue -999.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7079)
        (geospace:hasYRangeMax 7099)
        (geospace:hasLatLowerBound -34.222)
        (geospace:hasLonLowerBound -13.205)
        (geospace:hasLatUpperBound 36.788)
        (geospace:hasLonUpperBound 57.605)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject oc_hsg habitat:HydrologicSoilsGroup

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_hydrologic_soils_group")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "california:hsg_ca")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2531)
        (geospace:hasYRangeMax 2074)
        (geospace:hasLatLowerBound 3694351.02)
        (geospace:hasLonLowerBound 386509.0)
        (geospace:hasLatUpperBound 3769127.0)
        (geospace:hasLonUpperBound 472571.821)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject HSG4 habitat:HydrologicSoilsGroup

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:HSG4")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 57786)
        (geospace:hasYRangeMax 24837)
        (geospace:hasLatLowerBound 24.546)
        (geospace:hasLonLowerBound -124.737)
        (geospace:hasLatUpperBound 49.385)
        (geospace:hasLonUpperBound -66.949)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject san_pedro_hsg habitat:HydrologicSoilsGroup

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_hydrologic_soils_group")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:hsg_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8712)
        (geospace:hasYRangeMax 9186)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 424094.614)
        (geospace:hasLatUpperBound 3742078.5)
        (geospace:hasLonUpperBound 685514.614)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject snowmelt_jan habitat:JanuarySnowmelt

  "Data source: University of Delaware Global Water Balance Archive"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://climate.geog.udel.edu/~climate/html_pages/README.wb_ts2.html")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:snowmelt_jan")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 520)
        (geospace:hasYRangeMax 250)
        (geospace:hasLatLowerBound -90.095)
        (geospace:hasLonLowerBound -180.095)
        (geospace:hasLatUpperBound 83.785)
        (geospace:hasLonUpperBound 180.085)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject javelina_240 sanPedro:JavelinaHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:javelina")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_mammal_rich habitat:MammalRichness

  "Data source: University of Arizona-USGS Southern Arizona Data Services Program"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://sdrsnet.srnr.arizona.edu/index.php?page=datamenu")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:mammal_rich")
        (geospace:hasValueAttribute "grid_code")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414974.363269456)
        (geospace:hasLonLowerBound 114877.97304113029)
        (geospace:hasLatUpperBound 4106788.5249591796)
        (geospace:hasLonUpperBound 699436.3793787669)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject mearns_240 sanPedro:MearnsQuailHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:mearns")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_mesquite_management2 sanPedro:MesquiteManagement

  "Data source: Hypothetical mesquite management polygons within the SPRNCA"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:mesquite_management2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -111.012)
        (geospace:hasLatUpperBound 33.281)
        (geospace:hasLonUpperBound -109.7945359)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject os3 mglulc:MGLULCNumeric

  "Data source: Foiben-Taosarintanini Madagasikara (FTM)"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:lulc_mg")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2497)
        (geospace:hasYRangeMax 4630)
        (geospace:hasLatLowerBound -27.65)
        (geospace:hasLonLowerBound 41.35)
        (geospace:hasLatUpperBound -10.488)
        (geospace:hasLonUpperBound 52.78)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mountainfront_recharge geofeatures:MountainFront

  "Data source: Derived from AGIC geology layer"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/data/metadata/html/geology.htm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:mountainfront_recharge")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5333)
        (geospace:hasYRangeMax 6361)
        (geospace:hasLatLowerBound 3464504.4)
        (geospace:hasLonLowerBound 153487.996)
        (geospace:hasLatUpperBound 4100804.4)
        (geospace:hasLonUpperBound 686987.996)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))


(defobject mourning_240 sanPedro:MourningDoveHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:mourning")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mule_240 sanPedro:MuleDeerHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:mule")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Non_Rival_Use waterSupplyService:NonRivalWaterUseCode
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mexico:non_rival_use_la")
        (geospace:hasNodataValue 2.147483647E9)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 22)
        (geospace:hasYRangeMax 6)
        (geospace:hasLatLowerBound 19.142240226562222)
        (geospace:hasLonLowerBound -97.2746372601786)
        (geospace:hasLatUpperBound 19.576122241899572)
        (geospace:hasLonUpperBound -96.27930746109703)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject open_space_area_san_pedro aestheticService:OpenSpaceArea

  "Data source: Derived from NLCD 2001, calculating area of all open space"

  (measurement:Measurement
    (measurement:unit "ha")
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:open_space_area_san_pedro")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9439)
        (geospace:hasYRangeMax 7260)
        (geospace:hasLatLowerBound 3466405.58)
        (geospace:hasLonLowerBound 330743.611)
        (geospace:hasLatUpperBound 3684265.58)
        (geospace:hasLonUpperBound 613973.611)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject open_space_area_puget aestheticService:OpenSpaceArea

  "Data source: Derived from NLCD 2001, calculating area of all open space"

  (measurement:Measurement
    (measurement:unit "ha")
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:open_space_area_puget")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10269)
        (geospace:hasYRangeMax 10147)
        (geospace:hasLatLowerBound 5125512.88)
        (geospace:hasLonLowerBound 368260.429)
        (geospace:hasLatUpperBound 5429982.88)
        (geospace:hasLonUpperBound 676390.429)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject sanPedro_trails_sprnca2 infrastructure:Path

  "Data source: Tucson Field Office, Arizona Bureau of Land Management"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:trails_sprnca2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3490616.2259507948)
        (geospace:hasLonLowerBound 572743.1614215748)
        (geospace:hasLatUpperBound 3523745.548341618)
        (geospace:hasLonUpperBound 582134.5934271315)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject FloodplainVegMg habitat:PercentFloodplainVegetationCover

  "Data source: Derived from Dartmouth Flood Observatory flood extents and UMD-GLCF tree canopy data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "DFO-UMD_GLCF_floodplain_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:floodplain_veg_mg")
        (geospace:hasNodataValue 65535.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 871)
        (geospace:hasYRangeMax 1638)
        (geospace:hasLatLowerBound -25.608)
        (geospace:hasLonLowerBound 43.217)
        (geospace:hasLatUpperBound -11.942)
        (geospace:hasLonUpperBound 50.492)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject dr_fp_veg habitat:PercentFloodplainVegetationCover

  "Data source: Derived from Dartmouth Flood Observatory flood extents and UMD-GLCF tree canopy data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:belongsToDataset "DFO-UMD_GLCF_floodplain_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "dominican:floodplain_veg_dr")
        (geospace:hasNodataValue 65535.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2440)
        (geospace:hasYRangeMax 711)
        (geospace:hasLatLowerBound 17.392)
        (geospace:hasLonLowerBound -84.933)
        (geospace:hasLatUpperBound 23.333)
        (geospace:hasLonUpperBound -64.583)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject imperviousGlobal habitat:PercentImperviousness

  "Data source: NOAA-NGDC"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.ngdc.noaa.gov/dmsp/download_global_isa.html")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:impervious_global")
        (geospace:hasNodataValue -1.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 16798)
        (geospace:hasLatLowerBound -64.996)
        (geospace:hasLonLowerBound -180.004)
        (geospace:hasLatUpperBound 75.004)
        (geospace:hasLonUpperBound 179.996)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious2_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_2")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 34603)
        (geospace:hasYRangeMax 33376)
        (geospace:hasLatLowerBound 31.406)
        (geospace:hasLonLowerBound -125.1)
        (geospace:hasLatUpperBound 43.736)
        (geospace:hasLonUpperBound -112.317)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious5_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_5")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 33070)
        (geospace:hasYRangeMax 22359)
        (geospace:hasLatLowerBound 30.348)
        (geospace:hasLonLowerBound -115.876)
        (geospace:hasLatUpperBound 37.918)
        (geospace:hasLonUpperBound -104.68)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious10_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_10")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 34640)
        (geospace:hasYRangeMax 37790)
        (geospace:hasLatLowerBound 25.72)
        (geospace:hasLonLowerBound -101.846)
        (geospace:hasLatUpperBound 37.247)
        (geospace:hasLonUpperBound -91.28)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious11_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_11")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 39264)
        (geospace:hasYRangeMax 25224)
        (geospace:hasLatLowerBound 34.632)
        (geospace:hasLonLowerBound -91.816)
        (geospace:hasLatUpperBound 43.174)
        (geospace:hasLonUpperBound -78.52)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious12_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_12")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 28123)
        (geospace:hasYRangeMax 28057)
        (geospace:hasLatLowerBound 28.479)
        (geospace:hasLonLowerBound -92.817)
        (geospace:hasLatUpperBound 37.391)
        (geospace:hasLonUpperBound -83.885)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious13_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_13")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 41790)
        (geospace:hasYRangeMax 38166)
        (geospace:hasLatLowerBound 34.641)
        (geospace:hasLonLowerBound -81.75)
        (geospace:hasLatUpperBound 49.123)
        (geospace:hasLonUpperBound -65.894)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious14_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_14")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 38425)
        (geospace:hasYRangeMax 45574)
        (geospace:hasLatLowerBound 23.713)
        (geospace:hasLonLowerBound -87.735)
        (geospace:hasLatUpperBound 39.111)
        (geospace:hasLonUpperBound -74.752)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious3_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_3")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 39337)
        (geospace:hasYRangeMax 26150)
        (geospace:hasLatLowerBound 40.844)
        (geospace:hasLonLowerBound -119.737)
        (geospace:hasLatUpperBound 50.586)
        (geospace:hasLonUpperBound -105.081)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious4_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_4")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 36939)
        (geospace:hasYRangeMax 32629)
        (geospace:hasLatLowerBound 34.361)
        (geospace:hasLonLowerBound -116.988)
        (geospace:hasLatUpperBound 45.851)
        (geospace:hasLonUpperBound -103.981)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious6_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_6")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 47330)
        (geospace:hasYRangeMax 27392)
        (geospace:hasLatLowerBound 40.067)
        (geospace:hasLonLowerBound -109.919)
        (geospace:hasLatUpperBound 49.428)
        (geospace:hasLonUpperBound -93.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious7_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_7")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 27954)
        (geospace:hasYRangeMax 41713)
        (geospace:hasLatLowerBound 28.762)
        (geospace:hasLonLowerBound -107.642)
        (geospace:hasLatUpperBound 42.373)
        (geospace:hasLonUpperBound -98.521)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious8_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_8")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 44707)
        (geospace:hasYRangeMax 23898)
        (geospace:hasLatLowerBound 41.13)
        (geospace:hasLonLowerBound -96.446)
        (geospace:hasLatUpperBound 49.389)
        (geospace:hasLonUpperBound -80.997)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject impervious9_Project habitat:PercentImperviousness

  "Data source: NLCD 2001 impervious surface data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_impervious")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:impervious_9")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 38523)
        (geospace:hasYRangeMax 35660)
        (geospace:hasLatLowerBound 33.352)
        (geospace:hasLonLowerBound -101.053)
        (geospace:hasLatUpperBound 44.811)
        (geospace:hasLonUpperBound -88.674)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject AsiaPacificTreeCover habitat:PercentVegetationCover

  "University of Maryland Global Land Cover Facility"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://glcf.umiacs.umd.edu/index.shtml")
    (metadata:belongsToDataset "UMD_GLCF_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tree_cover_asia_pacific")
        (geospace:hasNodataValue 254.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10438)
        (geospace:hasYRangeMax 7436)
        (geospace:hasLatLowerBound -55.983)
        (geospace:hasLonLowerBound 93.0)
        (geospace:hasLatUpperBound 6.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject EuropeAsiaTreeCover habitat:PercentVegetationCover

  "University of Maryland Global Land Cover Facility"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://glcf.umiacs.umd.edu/index.shtml")
    (metadata:belongsToDataset "UMD_GLCF_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tree_cover_europe_asia")
        (geospace:hasNodataValue 254.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 22956)
        (geospace:hasYRangeMax 10689)
        (geospace:hasLatLowerBound 0.908)
        (geospace:hasLonLowerBound -11.317)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NorthAmericaTreeCover habitat:PercentVegetationCover

  "University of Maryland Global Land Cover Facility"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://glcf.umiacs.umd.edu/index.shtml")
    (metadata:belongsToDataset "UMD_GLCF_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tree_cover_north_america")
        (geospace:hasNodataValue 254.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 20679)
        (geospace:hasYRangeMax 10284)
        (geospace:hasLatLowerBound 4.283)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound -7.658)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject SouthAmericaTreeCover habitat:PercentVegetationCover

  "University of Maryland Global Land Cover Facility"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://glcf.umiacs.umd.edu/index.shtml")
    (metadata:belongsToDataset "UMD_GLCF_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:tree_cover_south_america")
        (geospace:hasNodataValue 254.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8650)
        (geospace:hasYRangeMax 10284)
        (geospace:hasLatLowerBound -71.867)
        (geospace:hasLonLowerBound -93.425)
        (geospace:hasLatUpperBound 13.85)
        (geospace:hasLonUpperBound -21.325)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy2_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_2")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 34603)
        (geospace:hasYRangeMax 33376)
        (geospace:hasLatLowerBound 31.406)
        (geospace:hasLonLowerBound -125.1)
        (geospace:hasLatUpperBound 43.736)
        (geospace:hasLonUpperBound -112.317)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy5_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_5")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 33070)
        (geospace:hasYRangeMax 22359)
        (geospace:hasLatLowerBound 30.348)
        (geospace:hasLonLowerBound -115.876)
        (geospace:hasLatUpperBound 37.918)
        (geospace:hasLonUpperBound -104.68)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy10_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_10")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 34640)
        (geospace:hasYRangeMax 37790)
        (geospace:hasLatLowerBound 25.72)
        (geospace:hasLonLowerBound -101.846)
        (geospace:hasLatUpperBound 37.247)
        (geospace:hasLonUpperBound -91.28)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy11_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_11")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 39264)
        (geospace:hasYRangeMax 25224)
        (geospace:hasLatLowerBound 34.632)
        (geospace:hasLonLowerBound -91.816)
        (geospace:hasLatUpperBound 43.174)
        (geospace:hasLonUpperBound -78.52)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy12_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_12")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 28123)
        (geospace:hasYRangeMax 28057)
        (geospace:hasLatLowerBound 28.479)
        (geospace:hasLonLowerBound -92.817)
        (geospace:hasLatUpperBound 37.391)
        (geospace:hasLonUpperBound -83.885)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy13_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_13")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 41790)
        (geospace:hasYRangeMax 38166)
        (geospace:hasLatLowerBound 34.641)
        (geospace:hasLonLowerBound -81.75)
        (geospace:hasLatUpperBound 49.123)
        (geospace:hasLonUpperBound -65.894)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy14_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_14")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 38425)
        (geospace:hasYRangeMax 45574)
        (geospace:hasLatLowerBound 23.713)
        (geospace:hasLonLowerBound -87.735)
        (geospace:hasLatUpperBound 39.111)
        (geospace:hasLonUpperBound -74.752)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy3_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_3")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 39337)
        (geospace:hasYRangeMax 26150)
        (geospace:hasLatLowerBound 40.844)
        (geospace:hasLonLowerBound -119.737)
        (geospace:hasLatUpperBound 50.586)
        (geospace:hasLonUpperBound -105.081)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy4_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_4")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 36939)
        (geospace:hasYRangeMax 32629)
        (geospace:hasLatLowerBound 34.361)
        (geospace:hasLonLowerBound -116.988)
        (geospace:hasLatUpperBound 45.851)
        (geospace:hasLonUpperBound -103.981)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy6_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_6")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 47330)
        (geospace:hasYRangeMax 27392)
        (geospace:hasLatLowerBound 40.067)
        (geospace:hasLonLowerBound -109.919)
        (geospace:hasLatUpperBound 49.428)
        (geospace:hasLonUpperBound -93.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy7_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_7")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 27954)
        (geospace:hasYRangeMax 41713)
        (geospace:hasLatLowerBound 28.762)
        (geospace:hasLonLowerBound -107.642)
        (geospace:hasLatUpperBound 42.373)
        (geospace:hasLonUpperBound -98.521)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy8_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_8")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 44707)
        (geospace:hasYRangeMax 23898)
        (geospace:hasLatLowerBound 41.13)
        (geospace:hasLonLowerBound -96.446)
        (geospace:hasLatUpperBound 49.389)
        (geospace:hasLonUpperBound -80.997)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject canopy9_Project habitat:PercentVegetationCover

  "Data source: NLCD 2001 canopy density data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001_canopy_cover")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127.0)
        (geospace:hasNodataValue 255.0)
        (geospace:hasCoverageId "usa:canopy_9")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 38523)
        (geospace:hasYRangeMax 35660)
        (geospace:hasLatLowerBound 33.352)
        (geospace:hasLonLowerBound -101.053)
        (geospace:hasLatUpperBound 44.811)
        (geospace:hasLonUpperBound -88.674)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject pigs waterSupplyService:PigsPopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:pigs")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3469)
        (geospace:hasLatLowerBound -89.9)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.65)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_az_popdens policytarget:PopulationDensity

  "Data source: U.S. Census Bureau 2000 population estimates and TIGER block group files"

  (measurement:Count
    (metadata:hasPriority 0)
    (measurement:unit "/km^2")
    (metadata:belongsToDataset "Census_population_density")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:az_popdens")
        (geospace:hasValueAttribute "popdens00")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 30.868001600279342)
        (geospace:hasLonLowerBound -114.81700548244825)
        (geospace:hasLatUpperBound 37.01100029892272)
        (geospace:hasLonUpperBound -109.04500647198498)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject puget_pugetpopulationdensity policytarget:PopulationDensity

  "Data source: Washington State Office of Financial Management 2007 Census Block Group data"

  (measurement:Count
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ofm.wa.gov/pop/smallarea/default.asp")
    (metadata:belongsToDataset "Census_population_density")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:pugetpopulationdensity")
        (geospace:hasValueAttribute "personskm2")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 54007.72199547973)
        (geospace:hasLonLowerBound 508412.22498618934)
        (geospace:hasLatUpperBound 1385120.9910318155)
        (geospace:hasLonUpperBound 2612573.0955876815)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject vermont_population_density_vt policytarget:PopulationDensity

  "Data source: U.S. Census Bureau 2000 population estimates and TIGER block group files"

  (measurement:Count
    (metadata:hasPriority 0)
    (measurement:unit "/km^2")
    (metadata:belongsToDataset "Census_population_density")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:population_density_vt")
        (geospace:hasValueAttribute "pop_dens")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 42.7268600463867)
        (geospace:hasLonLowerBound -73.4305191040039)
        (geospace:hasLatUpperBound 45.0166702270508)
        (geospace:hasLonUpperBound -71.4645538330078)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject pet habitat:PotentialEvapotranspiration

  "Data source: CGIAR Global Aridity/Global PET database"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://csi.cgiar.org/aridity/index.asp")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:pet_global")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 17998)
        (geospace:hasLatLowerBound -60.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject poultry waterSupplyService:PoultryPopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:poultry")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3469)
        (geospace:hasLatLowerBound -89.9)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.65)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject parcels_puget2 aestheticService:PresenceOfHousing

  "Data source: County assessors' offices"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:parcels_puget2")
        (geospace:hasNodataValue 65535.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9991)
        (geospace:hasYRangeMax 4808)
        (geospace:hasLatLowerBound 46.7619066)
        (geospace:hasLonLowerBound -124.615382)
        (geospace:hasLatUpperBound 48.3492066)
        (geospace:hasLonUpperBound -121.317692)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_railroads infrastructure:Railway
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:railroads")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 17.952)
        (geospace:hasLonLowerBound -165.262)
        (geospace:hasLatUpperBound 65.0)
        (geospace:hasLonUpperBound -52.709)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_pugeterosionmetrics soilRetentionService:RainfallRunoffErosivityIndex

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_r")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject rare_charismatic_bird_richness2 habitat:RareBirdHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:rare_charismatic_bird_richness2")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10660)
        (geospace:hasYRangeMax 7620)
        (geospace:hasLatLowerBound 28.9392764)
        (geospace:hasLonLowerBound -122.068598)
        (geospace:hasLatUpperBound 44.1832764)
        (geospace:hasLonUpperBound -100.744598)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_reptile_rich habitat:ReptileRichness

  "Data source: University of Arizona-USGS Southern Arizona Data Services Program"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://sdrsnet.srnr.arizona.edu/index.php?page=datamenu")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:reptile_rich")
        (geospace:hasValueAttribute "grid_code")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414974.349525294)
        (geospace:hasLonLowerBound 114973.68290087063)
        (geospace:hasLatUpperBound 4106562.7324075284)
        (geospace:hasLonUpperBound 699340.7518807739)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject dominican_drreservoirs2 geofeatures:Reservoir
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "dominican:drreservoirs2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 18.391)
        (geospace:hasLonLowerBound -71.7)
        (geospace:hasLatUpperBound 19.9)
        (geospace:hasLonUpperBound -70.186)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Africa_dams3 geofeatures:Reservoir

  "Data source: FAO AQUASTAT"

  (measurement:BinaryCoding
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.fao.org/nr/water/aquastat/gis/index2.stm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:dams_africa")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7398)
        (geospace:hasYRangeMax 7160)
        (geospace:hasLatLowerBound -34.405)
        (geospace:hasLonLowerBound -16.38)
        (geospace:hasLatUpperBound 37.215)
        (geospace:hasLonUpperBound 57.62)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_sprnca_condition_class sanPedro:RiparianConditionClass

  "Data source: Stromberg et al. (2006)"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:sprnca_condition_class")
        (geospace:hasValueAttribute "condition")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLonLowerBound 607835.3653628668)
        (geospace:hasLatLowerBound 3682439.528696386)
        (geospace:hasLonUpperBound 498882.00222237466)
        (geospace:hasLatUpperBound 3415628.8645155914)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject sanPedro_sprnca_condition_class_hmr sanPedro:RiparianConditionClassHalfMeterRise

  "Data source: Brookshire et al. (2010)"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:sprnca_condition_class")
        (geospace:hasValueAttribute "cc_gw__0_5__10")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.534577496)
        (geospace:hasLonLowerBound 498087.4793240902)
        (geospace:hasLatUpperBound 3683452.407758169)
        (geospace:hasLonUpperBound 608027.6973406251)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject sanPedro_sprnca_condition_class_wet sanPedro:RiparianConditionClassAllWet

  "Data source: Brookshire et al. (2010)"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:sprnca_condition_class")
        (geospace:hasValueAttribute "scen9")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.534577496)
        (geospace:hasLonLowerBound 498087.4793240902)
        (geospace:hasLatUpperBound 3683452.407758169)
        (geospace:hasLonUpperBound 608027.6973406251)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject hydrography_puget_mid_res geofeatures:River

  "Data source: Washington DOT hydrography data"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.wsdot.wa.gov/mapsdata/geodatacatalog/default.htm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:hydrography_puget_mid_res")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 4071)
        (geospace:hasYRangeMax 3147)
        (geospace:hasLatLowerBound 45.8540155)
        (geospace:hasLonLowerBound -124.762584)
        (geospace:hasLatUpperBound 49.0030155)
        (geospace:hasLonUpperBound -120.689584)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mexico_lastreams geofeatures:River
  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mexico:lastreams")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 2117473.982178886)
        (geospace:hasLonLowerBound 680996.7578990177)
        (geospace:hasLatUpperBound 2166836.2477506534)
        (geospace:hasLonUpperBound 786410.4929716699)
        (geospace:hasCoordinateReferenceSystem "EPSG:32614")))))


(defobject california_hydro geofeatures:River

  "Data source: Caltrans, Teale Data Center"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.dot.ca.gov/hq/tsip/gis/datalibrary/gisdatalibrary.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:hydro")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 32.534000861575905)
        (geospace:hasLonLowerBound -124.41500313603933)
        (geospace:hasLatUpperBound 42.009998728236134)
        (geospace:hasLonUpperBound -114.13200536255405)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject global_hydrography_europe geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_europe")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 12.6046094894409)
        (geospace:hasLonLowerBound -21.7620868682861)
        (geospace:hasLatUpperBound 79.7840194702148)
        (geospace:hasLonUpperBound 66.7019119262695)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_namerica geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_namerica")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 7.85668897628784)
        (geospace:hasLonLowerBound -166.383560180664)
        (geospace:hasLatUpperBound 83.1898727416992)
        (geospace:hasLonUpperBound -53.9646644592285)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_africa geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_africa")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -34.7510871887207)
        (geospace:hasLonLowerBound -17.0239601135254)
        (geospace:hasLatUpperBound 37.227466583252)
        (geospace:hasLonUpperBound 51.189136505127)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_australia geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_australia")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -46.5708618164062)
        (geospace:hasLonLowerBound 95.3060760498047)
        (geospace:hasLatUpperBound 18.4474067687988)
        (geospace:hasLonUpperBound 178.571899414062)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_asia geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_asia")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 1.62845921516418)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 79.7836303710938)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_samerica geofeatures:River

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_samerica")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -54.5574569702148)
        (geospace:hasLonLowerBound -81.288948059082)
        (geospace:hasLatUpperBound 11.5741453170776)
        (geospace:hasLonUpperBound -34.8422622680664)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_highways recreationService:RoadTravelCapacity

  "Data source: TIGER/Line 2000 Streets"

  (measurement:NumericCoding
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:highways")
        (geospace:hasValueAttribute "class")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 19.058)
        (geospace:hasLonLowerBound -166.557)
        (geospace:hasLatUpperBound 71.296)
        (geospace:hasLonUpperBound -66.98)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject scaled_240 sanPedro:ScaledQuailHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:scaled")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_highways_scenic aestheticService:ScenicDrives

  "Data source: TIGER/Line 2000 Streets and Rand McNally Atlas scenic drives"

  (measurement:BinaryCoding
    (metadata:hasPriority 5)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:highways_scenic")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -124.548)
        (geospace:hasLatUpperBound 48.928)
        (geospace:hasLonUpperBound -71.16)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_pugeterosionmetrics soilRetentionService:SedimentSourceValueAnnual

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Measurement
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (measurement:unit "kg/ha")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_a_kh")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject sheep waterSupplyService:SheepPopulation

  "Data source: FAO Gridded Livestock of the World"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.fao.org/Ag/againfo/////resources/en/glw/GLW_dens.html")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:sheep")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7198)
        (geospace:hasYRangeMax 3598)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject usa_pugeterosionmetrics soilRetentionService:SlopeSteepnessAndLengthFactor

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_ls")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject puget_pugeterosionmetrics habitat:SoilErodibility

  "Data source: U.S. EPA EMAP-West Metric Browser"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://www.epa.gov/nerlesd1/land-sci/emap_west_browser/pages/wemap_mm_sl_table.htm#mapnav")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:pugeterosionmetrics")
        (geospace:hasValueAttribute "rusle_k")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -5933788.227137955)
        (geospace:hasLonLowerBound 58564.58686897659)
        (geospace:hasLatUpperBound 3260467.140311843)
        (geospace:hasLonUpperBound 1.0537895527671775E7)
        (geospace:hasCoordinateReferenceSystem "EPSG:2927")))))


(defobject SoilErodibility2 habitat:SoilErodibility

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:SoilErodibility2")
        (geospace:hasTransformation "self * 0.1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject erodibility4 habitat:SoilErodibility

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:erodibility4")
        (geospace:hasTransformation "self * 0.1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 57786)
        (geospace:hasYRangeMax 24837)
        (geospace:hasLatLowerBound 24.546)
        (geospace:hasLonLowerBound -124.737)
        (geospace:hasLatUpperBound 49.385)
        (geospace:hasLonUpperBound -66.949)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject SoilpH_0_30 habitat:SoilPhShallow

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "california:soil_pH_0_30_ca")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2531)
        (geospace:hasYRangeMax 2074)
        (geospace:hasLatLowerBound 3694351.02)
        (geospace:hasLonLowerBound 386509.0)
        (geospace:hasLatUpperBound 3769127.0)
        (geospace:hasLonUpperBound 472571.821)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject SoilpH_30_100 habitat:SoilPhDeep

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_deep")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "california:soil_pH_30_100_ca")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2531)
        (geospace:hasYRangeMax 2074)
        (geospace:hasLatLowerBound 3694351.02)
        (geospace:hasLonLowerBound 386509.0)
        (geospace:hasLatUpperBound 3769127.0)
        (geospace:hasLonUpperBound 472571.821)
        (geospace:hasCoordinateReferenceSystem "EPSG:26911")))))


(defobject SoilpH_0_30_puget habitat:SoilPhShallow

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:soil_pH_0_30_puget")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject SoilpH_30_100_puget habitat:SoilPhDeep

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_deep")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:soil_pH_30_100_puget")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject SoilpH_0_30_san_pedro habitat:SoilPhShallow

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:soil_pH_0_30_san_pedro")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8712)
        (geospace:hasYRangeMax 9186)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 424094.614)
        (geospace:hasLatUpperBound 3742078.5)
        (geospace:hasLonUpperBound 685514.614)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject SoilpH_30_100_san_pedro habitat:SoilPhDeep

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (metadata:belongsToDataset "SSURGO_soil_pH_deep")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:soil_pH_30_100_san_pedro")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8712)
        (geospace:hasYRangeMax 9186)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 424094.614)
        (geospace:hasLatUpperBound 3742078.5)
        (geospace:hasLonUpperBound 685514.614)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject pH_0_30_a2 habitat:SoilPhShallow

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:pH_0_30_a2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 9995)
        (geospace:hasYRangeMax 12937)
        (geospace:hasLatLowerBound 27.218)
        (geospace:hasLonLowerBound -108.097)
        (geospace:hasLatUpperBound 40.157)
        (geospace:hasLonUpperBound -98.1)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject ph_0_30_b2 habitat:SoilPhShallow

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:ph_0_30_b2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 16702)
        (geospace:hasYRangeMax 8821)
        (geospace:hasLatLowerBound 31.332)
        (geospace:hasLonLowerBound -123.962)
        (geospace:hasLatUpperBound 40.155)
        (geospace:hasLonUpperBound -107.258)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject pH_0_30_c2 habitat:SoilPhShallow

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:pH_0_30_c2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 27412)
        (geospace:hasYRangeMax 12068)
        (geospace:hasLatLowerBound 36.934)
        (geospace:hasLonLowerBound -124.737)
        (geospace:hasLatUpperBound 49.004)
        (geospace:hasLonUpperBound -97.323)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject pH_0_30_d2 habitat:SoilPhShallow

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_shallow")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:pH_0_30_d2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 32565)
        (geospace:hasYRangeMax 24837)
        (geospace:hasLatLowerBound 24.546)
        (geospace:hasLonLowerBound -99.517)
        (geospace:hasLatUpperBound 49.385)
        (geospace:hasLonUpperBound -66.95)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject pH_30_100a2 habitat:SoilPhDeep

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_deep")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:pH_30_100a2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 27412)
        (geospace:hasYRangeMax 21785)
        (geospace:hasLatLowerBound 27.218)
        (geospace:hasLonLowerBound -124.737)
        (geospace:hasLatUpperBound 49.005)
        (geospace:hasLonUpperBound -97.323)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject pH_30_100b2 habitat:SoilPhDeep

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (metadata:belongsToDataset "STATSGO_soil_pH_deep")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:pH_30_100b2")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 32565)
        (geospace:hasYRangeMax 24837)
        (geospace:hasLatLowerBound 24.546)
        (geospace:hasLonLowerBound -99.517)
        (geospace:hasLatUpperBound 49.385)
        (geospace:hasLonUpperBound -66.95)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject SoilTexture habitat:SoilTexture

  "Data source: USDA-NRCS SSURGO soils data"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/ssurgo/description.html")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:soil_texture_puget")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 14406)
        (geospace:hasYRangeMax 11943)
        (geospace:hasLatLowerBound 5077470.87)
        (geospace:hasLonLowerBound 374335.998)
        (geospace:hasLatUpperBound 5435820.87)
        (geospace:hasLonUpperBound 806575.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject texture3 habitat:SoilTexture

  "Data source: USDA-NRCS STATSGO soils data"

  (measurement:Ranking
    (metadata:hasPriority 5)
    (metadata:hasURL "http://soils.usda.gov/survey/geography/statsgo")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:texture3")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 57786)
        (geospace:hasYRangeMax 24837)
        (geospace:hasLatLowerBound 24.546)
        (geospace:hasLonLowerBound -124.737)
        (geospace:hasLatUpperBound 49.385)
        (geospace:hasLonUpperBound -66.949)
        (geospace:hasCoordinateReferenceSystem "EPSG:4269")))))


(defobject swregap_landcover_Project sanPedro:SouthwestRegionalGapAnalysisLULC

  "Data source: Southwest Regional Gap Analysis Project"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:swregap_lulc")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 71174)
        (geospace:hasYRangeMax 46292)
        (geospace:hasLatLowerBound 29.6)
        (geospace:hasLonLowerBound -122.477)
        (geospace:hasLatUpperBound 44.486)
        (geospace:hasLonUpperBound -99.591)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject springs3 waterSupplyService:Springs

  "Data source: Arizona Geographic Information Council"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/main.do")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:springs_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 1536)
        (geospace:hasYRangeMax 1419)
        (geospace:hasLatLowerBound 30.868)
        (geospace:hasLonLowerBound -115.093)
        (geospace:hasLatUpperBound 37.022)
        (geospace:hasLonUpperBound -108.871)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject urban_growth_baseline_san_pedro sanPedro:Steinitz30ClassUrbanGrowthLULCBase

  "Data source: Steinitz et al. (2003)"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:urban_growth_baseline_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3156)
        (geospace:hasYRangeMax 5405)
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -111.012)
        (geospace:hasLatUpperBound 33.281)
        (geospace:hasLonUpperBound -109.7945359)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject urban_growth_constrained_san_pedro sanPedro:Steinitz30ClassUrbanGrowthLULCConstrained

  "Data source: Steinitz et al. (2003)"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:urban_growth_constrained_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3156)
        (geospace:hasYRangeMax 5405)
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -111.012)
        (geospace:hasLatUpperBound 33.281)
        (geospace:hasLonUpperBound -109.7945359)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject urban_growth_open_san_pedro sanPedro:Steinitz30ClassUrbanGrowthLULCOpen

  "Data source: Steinitz et al. (2003)"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:urban_growth_open_san_pedro")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3156)
        (geospace:hasYRangeMax 5405)
        (geospace:hasLatLowerBound 30.869)
        (geospace:hasLonLowerBound -111.012)
        (geospace:hasLatUpperBound 33.281)
        (geospace:hasLonUpperBound -109.7945359)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject str_gradient habitat:StreamGradient

  "Data source: Derived from Digital Chart of the World hydrography and SRTM slope layers"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://data.geocomm.com/catalog/DR/group105.html")
    (measurement:unit "°")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "dominican:stream_gradient_dr")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5159)
        (geospace:hasYRangeMax 4465)
        (geospace:hasLatLowerBound 18.847)
        (geospace:hasLonLowerBound -71.734)
        (geospace:hasLatUpperBound 19.94)
        (geospace:hasLonUpperBound -70.472)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject streamGradientMg habitat:StreamGradient

  "Data source: Derived from FTM hydrography and SRTM slope layers"

  (measurement:Measurement
    (measurement:unit "°")
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:stream_gradient_mg")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8683)
        (geospace:hasYRangeMax 16369)
        (geospace:hasLatLowerBound -25.603)
        (geospace:hasLonLowerBound 43.24)
        (geospace:hasLatUpperBound -11.96)
        (geospace:hasLonUpperBound 50.477)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject pugetStreamGradient habitat:StreamGradient

  "Data source: Derived from DNR hydrography and SRTM slope layers"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "°")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "puget:stream_gradient_puget")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 12619)
        (geospace:hasYRangeMax 11256)
        (geospace:hasLatLowerBound 5094045.61)
        (geospace:hasLonLowerBound 345136.427)
        (geospace:hasLatUpperBound 5431785.61)
        (geospace:hasLonUpperBound 723766.427)
        (geospace:hasCoordinateReferenceSystem "EPSG:26910")))))


(defobject global_hydrography_europe habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_europe")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 12.6046094894409)
        (geospace:hasLonLowerBound -21.7620868682861)
        (geospace:hasLatUpperBound 79.7840194702148)
        (geospace:hasLonUpperBound 66.7019119262695)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_africa habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_africa")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -34.7510871887207)
        (geospace:hasLonLowerBound -17.0239601135254)
        (geospace:hasLatUpperBound 37.227466583252)
        (geospace:hasLonUpperBound 51.189136505127)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_australia habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_australia")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -46.5708618164062)
        (geospace:hasLonLowerBound 95.3060760498047)
        (geospace:hasLatUpperBound 18.4474067687988)
        (geospace:hasLonUpperBound 178.571899414062)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_namerica habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_namerica")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 7.85668897628784)
        (geospace:hasLonLowerBound -166.383560180664)
        (geospace:hasLatUpperBound 83.1898727416992)
        (geospace:hasLonUpperBound -53.9646644592285)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_asia habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_asia")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 1.62845921516418)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 79.7836303710938)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_hydrography_samerica habitat:StreamGradientRatio

  "Data source: USGS HYDRO1k Elevation Derivative Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro")
    (metadata:belongsToDataset "USGS HYDRO1k global stream gradient")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:hydrography_samerica")
        (geospace:hasValueAttribute "gradient")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -54.5574569702148)
        (geospace:hasLonLowerBound -81.288948059082)
        (geospace:hasLatUpperBound 11.5741453170776)
        (geospace:hasLonUpperBound -34.8422622680664)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject Diversions3 waterSupplyService:SurfaceDiversionCapacity

  "Data source: Digitized locations of St. David and Pomerene diversions"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:Diversions3")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 107)
        (geospace:hasYRangeMax 197)
        (geospace:hasLatLowerBound 30.98)
        (geospace:hasLonLowerBound -110.974)
        (geospace:hasLatUpperBound 32.97)
        (geospace:hasLonUpperBound -109.884)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_terrestrialecoregions ecology:TerrestrialEcoregion

  "Data source: WWF Terrestrial Ecoregions Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.worldwildlife.org/science/data/item1875.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:terrestrialecoregions")
        (geospace:hasValueAttribute "eco_id")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -89.892)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 83.623)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject sanPedro_transmission_az infrastructure:TransmissionLine

  "Data source: Arizona Land Resources Information System (ALRIS)"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.land.state.az.us/alris/layers.html")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:transmission_az")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 498853.85500464373)
        (geospace:hasLonLowerBound 134933.7738281882)
        (geospace:hasLatUpperBound 4105081.8290773137)
        (geospace:hasLonUpperBound 697575.8714159467)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))


(defobject NBCD_MZ04_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_4")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 17782)
        (geospace:hasYRangeMax 15309)
        (geospace:hasLatLowerBound 32.491)
        (geospace:hasLonLowerBound -122.543)
        (geospace:hasLatUpperBound 38.125)
        (geospace:hasLonUpperBound -115.999)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NBCD_MZ15_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_15")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 19982)
        (geospace:hasYRangeMax 17060)
        (geospace:hasLatLowerBound 31.783)
        (geospace:hasLonLowerBound -114.342)
        (geospace:hasLatUpperBound 37.591)
        (geospace:hasLonUpperBound -107.539)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NBCD_MZ25_FIA_ALD_biomass_final carbonService:VegetationCarbonStorage

  "Data source: National_Biomass_and_Carbon_Dataset"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.whrc.org/mapping/nbcd/index.html")
    (metadata:belongsToDataset "National Biomass and Carbon Dataset")
    (measurement:unit "t/ha")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "usa:biomass_25")
        (geospace:hasTransformation "self * 0.5")
        (geospace:hasNodataValue 32767.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 23611)
        (geospace:hasYRangeMax 17316)
        (geospace:hasLatLowerBound 30.788)
        (geospace:hasLonLowerBound -112.533)
        (geospace:hasLatUpperBound 36.546)
        (geospace:hasLonUpperBound -104.681)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject cdl_awifs_r_vt_2009_utm18 carbonService:VegType
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:crop_data_vt")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2738)
        (geospace:hasYRangeMax 4586)
        (geospace:hasLatLowerBound 4733616.0)
        (geospace:hasLonLowerBound 625158.0)
        (geospace:hasLatUpperBound 4990544.0)
        (geospace:hasLonUpperBound 778598.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:32618")))))


(defobject usa_watersheds_us geofeatures:Watershed

  "Data source: USGS Water Resources of the United States"

  (observation:Categorization
    (metadata:hasPriority 5)
    (metadata:hasURL "http://water.usgs.gov/GIS/metadata/usgswrd/XML/huc250k.xml")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "usa:watersheds_us")
        (geospace:hasValueAttribute "huc_name")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24.5233192443848)
        (geospace:hasLonLowerBound -124.756942749023)
        (geospace:hasLatUpperBound 49.3838691711426)
        (geospace:hasLonUpperBound -66.9395599365234)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject puget_wria_watersheds geofeatures:Watershed

  "Data source: Washington Department of Ecology"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ecy.wa.gov/services/gis/maps/wria/wria.htm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "puget:wria_watersheds")
        (geospace:hasValueAttribute "wria_nr")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 45.5437240600586)
        (geospace:hasLonLowerBound -124.860824584961)
        (geospace:hasLatUpperBound 49.0024337768555)
        (geospace:hasLonUpperBound -116.915229797363)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject wells_sonora waterSupplyService:Wells
  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:wells_sonora")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 743)
        (geospace:hasYRangeMax 1148)
        (geospace:hasLatLowerBound 30.868)
        (geospace:hasLonLowerBound -111.02)
        (geospace:hasLatUpperBound 33.285)
        (geospace:hasLonUpperBound -109.87)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject whitetail_240 sanPedro:WhiteTailDeerHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:whitetail")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject whitewing_240 sanPedro:WhiteWingedDoveHabitat

  "Data source: Southwest Regional GAP Analysis Project Animal Habitat Models"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://fws-nmcfwru.nmsu.edu/swregap/habitatreview/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:whitewing")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7732)
        (geospace:hasYRangeMax 5527)
        (geospace:hasLatLowerBound 28.938)
        (geospace:hasLonLowerBound -122.069)
        (geospace:hasLatUpperBound 44.183)
        (geospace:hasLonUpperBound -100.744)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject wd_det_1km_Project habitat:WoodyDebris

  "Data source: NASA-CASA CQUEST Project"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "http://sgeaims.arc.nasa.gov/website/cquest/meta1.html")
    (measurement:unit "g/m^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue -9999.0)
        (geospace:hasCoverageId "usa:woody_debris_us")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 5680)
        (geospace:hasYRangeMax 2679)
        (geospace:hasLatLowerBound 21.522)
        (geospace:hasLonLowerBound -130.77)
        (geospace:hasLatUpperBound 52.928)
        (geospace:hasLonUpperBound -64.209)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))

(defobject A_hololepidotus_Mg_abun fisheries:ArgyrosomusHololepidotusAbundanceMg

  "Data source: The Sea Around Us Project"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (measurement:unit "kg/km^2*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:A_hololepidotus_mg_abund")
        (geospace:hasNodataValue -3.4028234663852886E38)
        (geospace:hasTransformation "self * 0.0003432")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 248)
        (geospace:hasYRangeMax 354)
        (geospace:hasLatLowerBound -29.0)
        (geospace:hasLonLowerBound 40.5)
        (geospace:hasLatUpperBound -9.4)
        (geospace:hasLonUpperBound 53.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject A_hololepidotus fisheries:ArgyrosomusHololepidotusHabitat

  "Data source: The Sea Around Us Project"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:A_hololepidotus")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject gdcycgdp policytarget:AssetsAtRiskStorm

  "Data source: CIESIN - Columbia University World Data Center for Human Interactions in the Environment"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://sedac.ciesin.columbia.edu/wdc/hazards.jsp")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:ts_economic_risk")
        (geospace:hasTransformation "self * 0.1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8631)
        (geospace:hasYRangeMax 3427)
        (geospace:hasLatLowerBound -58.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 84.989)
        (geospace:hasLonUpperBound 179.996)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_storm_tracks2_mg geophysics:AtmosphericPressure

  "Data source: UNEP Division of Early Warning and Assessment Global Resource Information Database"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.grid.unep.ch/data/gnv199.php")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:storm_tracks2_mg")
        (geospace:hasValueAttribute "pr")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -40.2000007629395)
        (geospace:hasLonLowerBound 18.6999988555908)
        (geospace:hasLatUpperBound -7.5)
        (geospace:hasLonUpperBound 96.9000015258789)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject slope_mg geophysics:BathymetricSlope

  "Data source: Derived from NASA ETOPO1 Global Relief Model"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.ngdc.noaa.gov/mgg/global/global.html/")
    (measurement:unit "°")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:slope_mg")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 899)
        (geospace:hasYRangeMax 1259)
        (geospace:hasLatLowerBound -30.0083333)
        (geospace:hasLonLowerBound 39.9916667)
        (geospace:hasLatUpperBound -8.9916625)
        (geospace:hasLonUpperBound 55.0083364)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_bath1-171_ProjectRaster1 geophysics:Bathymetry

  "Data source: NASA ETOPO1 Global Relief Model"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.ngdc.noaa.gov/mgg/global/global.html/")
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:bathymetry_mg")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 899)
        (geospace:hasYRangeMax 1259)
        (geospace:hasLatLowerBound -30.008)
        (geospace:hasLonLowerBound 39.992)
        (geospace:hasLatUpperBound -8.992)
        (geospace:hasLonUpperBound 55.008)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_buffer_mg_100km coastalProtection:BufferMg100km

  "Data source: Derived from FTM coastline data"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:buffer_mg_100km")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -27.7)
        (geospace:hasLonLowerBound 41.3)
        (geospace:hasLatUpperBound -10.4)
        (geospace:hasLonUpperBound 52.8)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject marine_coral coastalProtection:CoralBleaching

  "Data source: Derived from UNEP-WCMC World Atlas of Coral Reefs"

  (observation:Categorization
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.unep-wcmc.org/marine/coralatlas/introduction.htm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:coral")
        (geospace:hasValueAttribute "bleaching")
        (geospace:hasValueDefault "None")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -34.3)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 32.492)
        (geospace:hasLonUpperBound 179.983)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject marine_coral_area2 fisheries:CoralReefArea

  "Data source: UNEP-WCMC World Atlas of Coral Reefs"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.unep-wcmc.org/marine/coralatlas/introduction.htm")
    (measurement:unit "km^2")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:coral_area2")
        (geospace:hasValueAttribute "area_km2")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -34.3)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 32.492)
        (geospace:hasLonUpperBound 179.983)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject distance_to_coast fisheries:DistanceToCoast

  "Data source: Derived from FTM coastline data"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (measurement:unit "km")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "distance_to_coast")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 1538)
        (geospace:hasYRangeMax 3015)
        (geospace:hasLatLowerBound 57716.0)
        (geospace:hasLonLowerBound 64874.0)
        (geospace:hasLatUpperBound 1566418.0)
        (geospace:hasLonUpperBound 834849.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:29700")))))


(defobject mg_dunes_mg geofeatures:Dune

  "Data source: Foiben-Taosarintanini Madagasikara (FTM)"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:dunes_mg")
        (geospace:hasValueAttribute "nature")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -27.7)
        (geospace:hasLonLowerBound 41.3)
        (geospace:hasLatUpperBound -10.4)
        (geospace:hasLonUpperBound 52.8)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_estuaryarea fisheries:EstuaryArea

  "Data source: Derived from FTM hydrography data"

  (observation:Categorization
    (metadata:hasPriority 0)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:estuaryarea")
        (geospace:hasValueAttribute "estuary")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -25.607)
        (geospace:hasLonLowerBound 43.235)
        (geospace:hasLatUpperBound -11.956)
        (geospace:hasLonUpperBound 50.489)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject L_borbonicus_Mg_abun fisheries:LethrinusBorbonicusAbundanceMg

  "Data source: The Sea Around Us Project"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (measurement:unit "kg/km^2*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:L_borbonicus_mg_abund")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 248)
        (geospace:hasYRangeMax 354)
        (geospace:hasLatLowerBound -29.0)
        (geospace:hasLonLowerBound 40.5)
        (geospace:hasLatUpperBound -9.4)
        (geospace:hasLonUpperBound 53.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject L_borbonicus fisheries:LethrinusBorbonicusHabitat

  "Data source: The Sea Around Us Project"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:L_borbonicus")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject L_mahsena_Mg_abun fisheries:LethrinusMahsenaAbundanceMg

  "Data source: The Sea Around Us Project"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (measurement:unit "kg/km^2*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:L_mahsena_mg_abund")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 248)
        (geospace:hasYRangeMax 354)
        (geospace:hasLatLowerBound -29.0)
        (geospace:hasLonLowerBound 40.5)
        (geospace:hasLatUpperBound -9.4)
        (geospace:hasLonUpperBound 53.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject gdcycmrt policytarget:LivesAtRiskStorm

  "Data source: CIESIN - Columbia University World Data Center for Human Interactions in the Environment"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://sedac.ciesin.columbia.edu/wdc/hazards.jsp")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:ts_lives_at_risk")
        (geospace:hasTransformation "self * 0.1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8631)
        (geospace:hasYRangeMax 3428)
        (geospace:hasLatLowerBound -58.015)
        (geospace:hasLonLowerBound -179.998)
        (geospace:hasLatUpperBound 85.016)
        (geospace:hasLonUpperBound 179.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject L_argentimaculatus_Mg_abun fisheries:LutjanusArgentimaculatusAbundanceMg

  "Data source: The Sea Around Us Project"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (measurement:unit "kg/km^2*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:L_argentimaculatus_mg_abund")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 248)
        (geospace:hasYRangeMax 354)
        (geospace:hasLatLowerBound -29.0)
        (geospace:hasLonLowerBound 40.5)
        (geospace:hasLatUpperBound -9.4)
        (geospace:hasLonUpperBound 53.5)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject L_argentimaculatus fisheries:LutjanusArgentimaculatusHabitat

  "Data source: The Sea Around Us Project"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.seaaroundus.org/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:L_argentimaculatus")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject global_mangrove_width2 coastalProtection:MangrovePresence

  "Data source: UNEP-WCMC"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:mangrove_width2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -38.909309387207)
        (geospace:hasLonLowerBound -175.337432861328)
        (geospace:hasLatUpperBound 32.3501739501953)
        (geospace:hasLonUpperBound 179.903244018555)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject NfertPlusManure policytarget:NitrogenFromFertilizerAndManure

  "Data source: Potter et al. (in press)"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.geog.mcgill.ca/~nramankutty/Datasets/Datasets.html")
    (measurement:unit "kg/ha*year")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:N_fertilizer_manure")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 718)
        (geospace:hasYRangeMax 358)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 90.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_paths2 infrastructure:Path

  "Data source: Foiben-Taosarintanini Madagasikara (FTM)"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:paths2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -27.65)
        (geospace:hasLonLowerBound 41.35)
        (geospace:hasLatUpperBound -10.488)
        (geospace:hasLonUpperBound 52.78)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject popDens2006 policytarget:PopulationDensity

  "Data source: Oak Ridge National Laboratory LandScan Dataset"

  (measurement:Count
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.ornl.gov/sci/landscan/")
    (measurement:unit "/km^2")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:popdens_2006")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43198)
        (geospace:hasYRangeMax 20878)
        (geospace:hasLatLowerBound -90.0)
        (geospace:hasLonLowerBound -180.0)
        (geospace:hasLatUpperBound 84.0)
        (geospace:hasLonUpperBound 180.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))
    (metadata:belongsToDataset "Census_population_density")))


(defobject port_mg infrastructure:Port

  "Data source: Foiben-Taosarintanini Madagasikara (FTM)"

  (measurement:Ranking
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.ftm.mg/")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "mg:port_mg")
        (geospace:hasNodataValue 2.147483647E9)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 2429)
        (geospace:hasYRangeMax 4447)
        (geospace:hasLatLowerBound -27.7)
        (geospace:hasLonLowerBound 41.3)
        (geospace:hasLatUpperBound -10.4)
        (geospace:hasLonUpperBound 52.8)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject poverty_pct_1 policytarget:PovertyPercentage

  "Data source: SEDAC-CIESIN Poverty Mapping Project"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://sedac.ciesin.columbia.edu/povmap/ds_global.jsp")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:poverty_pct")
        (geospace:hasNodataValue 7.546573162078857)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 43199)
        (geospace:hasYRangeMax 15599)
        (geospace:hasLatLowerBound -65.004)
        (geospace:hasLonLowerBound -180.004)
        (geospace:hasLatUpperBound 65.004)
        (geospace:hasLonUpperBound 180.004)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject marine_seagrass coastalProtection:SeagrassPresence

  "Data source: UNEP-WCMC World Atlas of Seagrasses"

  (measurement:BinaryCoding
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.unep-wcmc.org/marine/seagrassatlas/introduction.htm")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:seagrass")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -43.551)
        (geospace:hasLonLowerBound -175.428)
        (geospace:hasLatUpperBound 61.029)
        (geospace:hasLonUpperBound 178.617)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_storm_tracks2_mg coastalProtection:StormTracks

  "Data source: UNEP Division of Early Warning and Assessment Global Resource Information Database"

  (observation:Categorization
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.grid.unep.ch/data/gnv199.php")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:storm_tracks2_mg")
        (geospace:hasValueAttribute "name2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -40.2000007629395)
        (geospace:hasLonLowerBound 18.6999988555908)
        (geospace:hasLatUpperBound -7.5)
        (geospace:hasLonUpperBound 96.9000015258789)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_storm_tracks_cumulated_mg weather:Storm

  "Data source: UNEP Division of Early Warning and Assessment Global Resource Information Database"

  (observation:Categorization
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.grid.unep.ch/data/gnv199.php")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:storm_tracks2_mg")
        (geospace:hasValueExpression "self == null ? name2 : (self + \",\" + name2)")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -40.2000007629395)
        (geospace:hasLonLowerBound 18.6999988555908)
        (geospace:hasLatUpperBound -7.5)
        (geospace:hasLonUpperBound 96.9000015258789)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject gdcyc habitat:TropicalStormProbability

  "Data source: CIESIN - Columbia University World Data Center for Human Interactions in the Environment"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://sedac.ciesin.columbia.edu/wdc/hazards.jsp")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "global:ts_probability")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 8631)
        (geospace:hasYRangeMax 3428)
        (geospace:hasLatLowerBound -58.015)
        (geospace:hasLonLowerBound -179.998)
        (geospace:hasLatUpperBound 85.016)
        (geospace:hasLonUpperBound 179.998)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject mg_storm_tracks2_mg geophysics:WindSpeed

  "Data source: UNEP Division of Early Warning and Assessment Global Resource Information Database"

  (measurement:Measurement
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.grid.unep.ch/data/gnv199.php")
    (measurement:unit "km/h")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "mg:storm_tracks2_mg")
        (geospace:hasValueAttribute "wind")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -40.2000007629395)
        (geospace:hasLonLowerBound 18.6999988555908)
        (geospace:hasLatUpperBound -7.5)
        (geospace:hasLonUpperBound 96.9000015258789)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))

(defobject nlcd_lc_ll_01 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_1")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 36404)
        (geospace:hasYRangeMax 28645)
        (geospace:hasLatLowerBound 40.142)
        (geospace:hasLonLowerBound -127.264)
        (geospace:hasLatUpperBound 50.118)
        (geospace:hasLonUpperBound -114.586)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_02 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_2")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 40995)
        (geospace:hasYRangeMax 39540)
        (geospace:hasLatLowerBound 31.396)
        (geospace:hasLonLowerBound -125.131)
        (geospace:hasLatUpperBound 43.763)
        (geospace:hasLonUpperBound -112.309)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_03 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_3")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 41430)
        (geospace:hasYRangeMax 27553)
        (geospace:hasLatLowerBound 40.812)
        (geospace:hasLonLowerBound -119.746)
        (geospace:hasLatUpperBound 50.586)
        (geospace:hasLonUpperBound -105.049)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_04 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_4")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 40737)
        (geospace:hasYRangeMax 35991)
        (geospace:hasLatLowerBound 34.384)
        (geospace:hasLonLowerBound -116.988)
        (geospace:hasLatUpperBound 45.848)
        (geospace:hasLonUpperBound -104.013)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_05 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasNodataValue 0)
        (geospace:hasCoverageId "usa:nlcd_5")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 35265)
        (geospace:hasYRangeMax 24133)
        (geospace:hasLatLowerBound 30.321)
        (geospace:hasLonLowerBound -115.9)
        (geospace:hasLatUpperBound 37.91)
        (geospace:hasLonUpperBound -104.811)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_06 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_6")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 46648)
        (geospace:hasYRangeMax 27044)
        (geospace:hasLatLowerBound 40.056)
        (geospace:hasLonLowerBound -109.903)
        (geospace:hasLatUpperBound 49.428)
        (geospace:hasLonUpperBound -93.737)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_07 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_7")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 31893)
        (geospace:hasYRangeMax 47675)
        (geospace:hasLatLowerBound 28.735)
        (geospace:hasLonLowerBound -107.657)
        (geospace:hasLatUpperBound 42.392)
        (geospace:hasLonUpperBound -98.52)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_08 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_8")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 45259)
        (geospace:hasYRangeMax 24356)
        (geospace:hasLatLowerBound 41.122)
        (geospace:hasLonLowerBound -96.389)
        (geospace:hasLatUpperBound 49.414)
        (geospace:hasLonUpperBound -80.982)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_09 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_9")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 41092)
        (geospace:hasYRangeMax 38013)
        (geospace:hasLatLowerBound 33.359)
        (geospace:hasLonLowerBound -101.054)
        (geospace:hasLatUpperBound 44.811)
        (geospace:hasLonUpperBound -88.674)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_10 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_10")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 36709)
        (geospace:hasYRangeMax 40057)
        (geospace:hasLatLowerBound 25.695)
        (geospace:hasLonLowerBound -101.815)
        (geospace:hasLatUpperBound 37.226)
        (geospace:hasLonUpperBound -91.248)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_11 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_11")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 42667)
        (geospace:hasYRangeMax 27374)
        (geospace:hasLatLowerBound 34.666)
        (geospace:hasLonLowerBound -91.811)
        (geospace:hasLatUpperBound 43.199)
        (geospace:hasLonUpperBound -78.512)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_12 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_12")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 31061)
        (geospace:hasYRangeMax 31004)
        (geospace:hasLatLowerBound 28.498)
        (geospace:hasLonLowerBound -92.836)
        (geospace:hasLatUpperBound 37.417)
        (geospace:hasLonUpperBound -83.901)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_13 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_13")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 55677)
        (geospace:hasYRangeMax 50925)
        (geospace:hasLatLowerBound 34.616)
        (geospace:hasLonLowerBound -81.752)
        (geospace:hasLatUpperBound 49.126)
        (geospace:hasLonUpperBound -65.887)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject nlcd_lc_ll_14 nlcd:NLCDNumeric

  "Data source: NLCD 2001"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.mrlc.gov/")
    (metadata:belongsToDataset "USGS_NLCD_2001")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasNodataValue 127)
        (geospace:hasCoverageId "usa:nlcd_14")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 46447)
        (geospace:hasYRangeMax 55000)
        (geospace:hasLatLowerBound 23.756)
        (geospace:hasLonLowerBound -87.694)
        (geospace:hasLatUpperBound 39.08)
        (geospace:hasLonUpperBound -74.753)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))

(defobject vt_ny_nhdwaterbody geofeatures:Lake
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:vt_ny_nhdwaterbody")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 22561.995596086967)
        (geospace:hasLonLowerBound 291973.7681410905)
        (geospace:hasLatUpperBound 290453.2104549259)
        (geospace:hasLonUpperBound 584468.6001250101)
        (geospace:hasCoordinateReferenceSystem "EPSG:32145")))))


(defobject vt_ny_nhdflowline geofeatures:River
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:vt_ny_nhdflowline")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 4239.490681890846)
        (geospace:hasLonLowerBound 282057.8702283442)
        (geospace:hasLatUpperBound 287740.8899279)
        (geospace:hasLonUpperBound 585102.6905476301)
        (geospace:hasCoordinateReferenceSystem "EPSG:32145")))))


(defobject vt_utility_lines infrastructure:EnergyInfrastructure
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:vt_utility_lines")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound -443.9106652097295)
        (geospace:hasLonLowerBound 414344.5695535828)
        (geospace:hasLatUpperBound 285399.2477500888)
        (geospace:hasLonUpperBound 583186.8473425076)
        (geospace:hasCoordinateReferenceSystem "EPSG:32145")))))


(defobject vt_roads infrastructure:Road
  (measurement:BinaryCoding
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "vermont:vt_roads")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 24772.382167272128)
        (geospace:hasLonLowerBound 422681.6904347708)
        (geospace:hasLatUpperBound 280279.2514770178)
        (geospace:hasLonUpperBound 584687.1924327603)
        (geospace:hasCoordinateReferenceSystem "EPSG:32145")))))


(defobject Slope24_WGS84 recreationService:HikingSlope
  (measurement:Measurement
    (measurement:unit "°")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:slope_RR")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 10216)
        (geospace:hasYRangeMax 12434)
        (geospace:hasLatLowerBound 42.552)
        (geospace:hasLonLowerBound -74.561)
        (geospace:hasLatUpperBound 45.501)
        (geospace:hasLonUpperBound -71.263)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject HikeDisWGS84 recreationService:HikingDistance
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:hike_distance_RR")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 76)
        (geospace:hasYRangeMax 212)
        (geospace:hasLatLowerBound 44.27)
        (geospace:hasLonLowerBound -73.151)
        (geospace:hasLatUpperBound 44.286)
        (geospace:hasLonUpperBound -73.144)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject accessWGS84 recreationService:PublicAccess
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:access_RR")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 168)
        (geospace:hasYRangeMax 377)
        (geospace:hasLatLowerBound 44.26)
        (geospace:hasLonLowerBound -73.157)
        (geospace:hasLatUpperBound 44.287)
        (geospace:hasLonUpperBound -73.14)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject TravCost_prj3 recreationService:TravelTime
  (measurement:Ranking
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:travel_cost_RR")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 168)
        (geospace:hasYRangeMax 377)
        (geospace:hasLatLowerBound 44.26)
        (geospace:hasLonLowerBound -73.157)
        (geospace:hasLatUpperBound 44.287)
        (geospace:hasLonUpperBound -73.14)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


(defobject ViewElevWGS84 recreationService:ViewPosition
  (measurement:Measurement
    (measurement:unit "m")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "vermont:view_elevation_RR")))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 76)
        (geospace:hasYRangeMax 212)
        (geospace:hasLatLowerBound 44.27)
        (geospace:hasLonLowerBound -73.151)
        (geospace:hasLatUpperBound 44.286)
        (geospace:hasLonUpperBound -73.144)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))
