(ns aries.db.marine  
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defobject namespace-ontology count)))

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


