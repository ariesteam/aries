(ns aries.db.common  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

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


(defobject CoastalWetlands soilretentionEcology:CoastalWetlands

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


(defobject pfactor1990_30min soilretentionEcology:ConservationPractice

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


(defobject cfactor1990_30min soilretentionEcology:CoverManagement

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


(defobject rfactor_1980 soilretentionEcology:RainfallRunoffErosivityIndex

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


(defobject lsfactor_30min soilretentionEcology:SlopeSteepnessAndLengthFactor

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


(defobject annual_loss1980 soilretentionEcology:SedimentSourceValueAnnual

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


