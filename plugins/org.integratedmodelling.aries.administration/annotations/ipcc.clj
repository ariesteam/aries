(ns administration.annotations.ipcc
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defobject count]))

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
