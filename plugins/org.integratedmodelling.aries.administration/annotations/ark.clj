(ns aries.db.ark  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

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


