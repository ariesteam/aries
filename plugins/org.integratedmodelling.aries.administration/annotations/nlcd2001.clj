(ns administration.annotations.nlcd2001
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defobject count]))

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
