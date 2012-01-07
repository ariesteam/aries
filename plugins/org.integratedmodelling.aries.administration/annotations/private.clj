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

(ns administration.annotations.private
  (:refer modelling :only [defobject]))


(defobject WPA2a conservation:ProtectedStatus

  "World Database on Protected Areas"

  (measurement:Ranking
    (metadata:hasPriority 10)
    (metadata:hasURL "http://www.wdpa.org/")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "global:wdpa2a")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")
        (geospace:hasLatLowerBound -68.65255775673205)
        (geospace:hasLonLowerBound -179.99999963965692)
        (geospace:hasLatUpperBound 83.7299995730599)
        (geospace:hasLonUpperBound 179.9995396992789)))))


(defobject well_tested_rate waterSupplyService:AnnualWellCapacity

  "Data source: Arizona Department of Water Resources Wells-55 database"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "https://gisweb.azwater.gov/WellRegistry/MapHelp.aspx")
    (measurement:unit "mm")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:well_tested_rate_san_pedro")
        (geospace:hasTransformation "self * 497349")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7234)
        (geospace:hasYRangeMax 8413)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 142225.0)
        (geospace:hasLatUpperBound 4097784.5)
        (geospace:hasLonUpperBound 684925.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject well_depth waterSupplyService:WellDepth

  "Data source: Arizona Department of Water Resources Wells-55 database"

  (measurement:Measurement
    (metadata:hasPriority 0)
    (metadata:hasURL "https://gisweb.azwater.gov/WellRegistry/MapHelp.aspx")
    (measurement:unit "ft")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wcs")
        (geospace:hasCoverageId "sanPedro:well_depth_san_pedro")
        (geospace:hasNodataValue -3.4028234663852886E38)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 7234)
        (geospace:hasYRangeMax 8413)
        (geospace:hasLatLowerBound 3415000.0)
        (geospace:hasLonLowerBound 142225.0)
        (geospace:hasLatUpperBound 4097784.5)
        (geospace:hasLonUpperBound 684925.0)
        (geospace:hasCoordinateReferenceSystem "EPSG:26912")))))


(defobject sanPedro_az_roads infrastructure:Road

  "Data source: Arizona Geographic Information Council"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/main.do")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:az_roads")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.864255436)
        (geospace:hasLonLowerBound 115167.55203442869)
        (geospace:hasLatUpperBound 4106554.7417112836)
        (geospace:hasLonUpperBound 699343.2614684913)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))


(defobject sanPedro_az_land_ownership habitat:LandOwnership

  "Data source: Arizona Geographic Information Council"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/main.do")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:az_land_ownership")
        (geospace:hasValueAttribute "owner")
        (geospace:hasValueType "thinklab-core:LongFloatingPoint")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.8504152074)
        (geospace:hasLonLowerBound 114880.4225935507)
        (geospace:hasLatUpperBound 4106788.0780395595)
        (geospace:hasLonUpperBound 699438.888976447)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))


(defobject sanPedro_hydrography_az_2 geofeatures:River

  "Data source: Arizona Geographic Information Council"

  (measurement:BinaryCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/data/metadata/html/streams.htm")
    (metadata:belongsToDataset "san-pedro-hydrography")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:hydrography_az_2")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3460316.9198390865)
        (geospace:hasLonLowerBound 116430.37599796552)
        (geospace:hasLatUpperBound 4106928.2415295388)
        (geospace:hasLonUpperBound 698532.9688473609)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))


(defobject sanPedro_az_roads recreationService:RoadTravelCapacity

  "Data source: Arizona Geographic Information Council"

  (observation:Categorization
    (metadata:hasPriority 0)
    (metadata:hasURL "http://agic.az.gov/portal/main.do")
    (observation:hasDataSource
      (geospace:WFSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "sanPedro:az_roads")
        (geospace:hasValueAttribute "category")))
    (observation:hasObservationExtent
      (geospace:ArealFeatureSet
        (geospace:hasLatLowerBound 3414973.864255436)
        (geospace:hasLonLowerBound 115167.55203442869)
        (geospace:hasLatUpperBound 4106554.7417112836)
        (geospace:hasLonUpperBound 699343.2614684913)
        (geospace:hasCoordinateReferenceSystem "EPSG:3742")))))