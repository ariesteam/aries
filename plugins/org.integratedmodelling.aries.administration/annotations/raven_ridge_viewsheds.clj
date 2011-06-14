(ns aries.db.raven_ridge_viewsheds  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

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


