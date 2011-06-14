(ns aries.db.kb1  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

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


(defobject usa_pugeterosionmetrics soilretentionEcology:ConservationPractice

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


(defobject usa_pugeterosionmetrics soilretentionEcology:CoverManagement

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


(defobject dominicanRepublic_coffee_farms soilretentionEcology:FarmlandCode
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


(defobject puget_hydroreservoirs2 soilretentionEcology:HydroelectricUseLevel

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


(defobject HydroDams soilretentionEcology:HydroelectricUseLevel

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


(defobject usa_pugeterosionmetrics soilretentionEcology:RainfallRunoffErosivityIndex

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


(defobject puget_pugeterosionmetrics soilretentionEcology:SedimentSourceValueAnnual

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


(defobject usa_pugeterosionmetrics soilretentionEcology:SlopeSteepnessAndLengthFactor

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


(defobject vegetation_type_ca southernCalifornia:VegTypeSoCal

  "Data source: USDA Forest Service Pacific Southwest Region Remote Sensing Lab"

  (measurement:NumericCoding
    (metadata:hasPriority 0)
    (metadata:hasURL "http://www.fs.fed.us/r5/rsl/clearinghouse/gis-download.shtml")
    (observation:hasDataSource
      (geospace:WCSDataSource
        (geospace:hasServiceUrl "http://ecoinformatics.uvm.edu/geoserver/wfs")
        (geospace:hasCoverageId "california:vegetation_type_ca")
        (geospace:hasNodataValue 255.0)))
    (observation:hasObservationExtent
      (geospace:RasterGrid
        (geospace:hasXRangeMax 3222)
        (geospace:hasYRangeMax 2473)
        (geospace:hasLatLowerBound 33.5646695)
        (geospace:hasLonLowerBound -118.657153)
        (geospace:hasLatUpperBound 34.3319195)
        (geospace:hasLonUpperBound -117.657713)
        (geospace:hasCoordinateReferenceSystem "EPSG:4326")))))


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


