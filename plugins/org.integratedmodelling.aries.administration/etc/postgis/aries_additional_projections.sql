-- Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
--
-- This file is part of ARIES.
--
-- ARIES is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License,
-- or (at your option) any later version.
--
-- ARIES is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with ARIES.  If not, see <http://www.gnu.org/licenses/>.
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 96618, 'sr-org', 6618, '+proj=omerc +lat_0=-18.9 +lonc=46.43722917 +alpha=18.9 +k=0.9995000000000001 +x_0=400000 +y_0=800000 +ellps=intl +units=m +no_defs ', 'PROJCS["Madagascar_Laborde_Tan1925",GEOGCS["GCS_Tananarive_1925",DATUM["Tananarive_1925",SPHEROID["International_1924",6378388.0,297.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Hotine_Oblique_Mercator"],PARAMETER["False_Easting",400000.0],PARAMETER["False_Northing",800000.0],PARAMETER["Scale_Factor",0.9995],PARAMETER["Azimuth",18.9],PARAMETER["Longitude_Of_Center",46.43722917],PARAMETER["Latitude_Of_Center",-18.9],PARAMETER["rectified_grid_angle",18.9],UNIT["Meter",1.0]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 929700, 'epsg', 29700, '+proj=omerc +lat_0=-18.9 +lonc=44.10000000000001 +alpha=18.9 +k=0.9995000000000001 +x_0=400000 +y_0=800000 +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +pm=paris +units=m +no_defs ', 'PROJCS["Tananarive (Paris) / Laborde Grid (deprecated)",GEOGCS["Tananarive (Paris)",DATUM["Tananarive_1925_Paris",SPHEROID["International 1924",6378388,297,AUTHORITY["EPSG","7022"]],TOWGS84[-189,-242,-91,0,0,0,0],AUTHORITY["EPSG","6810"]],PRIMEM["Paris",2.33722917,AUTHORITY["EPSG","8903"]],UNIT["grad",0.01570796326794897,AUTHORITY["EPSG","9105"]],AUTHORITY["EPSG","4810"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],PROJECTION["Hotine_Oblique_Mercator"],PARAMETER["latitude_of_center",-21],PARAMETER["longitude_of_center",49],PARAMETER["azimuth",21],PARAMETER["rectified_grid_angle",21],PARAMETER["scale_factor",0.9995],PARAMETER["false_easting",400000],PARAMETER["false_northing",800000],AUTHORITY["EPSG","29700"],AXIS["X",EAST],AXIS["Y",NORTH]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 929701, 'epsg', 29701, '', 'PROJCS["Tananarive (Paris) / Laborde Grid",GEOGCS["Tananarive (Paris)",DATUM["Tananarive_1925_Paris",SPHEROID["International 1924",6378388,297,AUTHORITY["EPSG","7022"]],TOWGS84[-189,-242,-91,0,0,0,0],AUTHORITY["EPSG","6810"]],PRIMEM["Paris",2.33722917,AUTHORITY["EPSG","8903"]],UNIT["grad",0.01570796326794897,AUTHORITY["EPSG","9105"]],AUTHORITY["EPSG","4810"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],PROJECTION["Laborde_Oblique_Mercator"],PARAMETER["latitude_of_center",-21],PARAMETER["longitude_of_center",49],PARAMETER["azimuth",21],PARAMETER["rectified_grid_angle",100],PARAMETER["scale_factor",0.9995],PARAMETER["false_easting",400000],PARAMETER["false_northing",800000],AUTHORITY["EPSG","29701"],AXIS["Y",EAST],AXIS["X",NORTH]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 929702, 'epsg', 29702, '+proj=omerc +lat_0=-18.9 +lonc=44.10000000000001 +alpha=18.9 +k=0.9995000000000001 +x_0=400000 +y_0=800000 +ellps=intl +towgs84=-189,-242,-91,0,0,0,0 +pm=paris +units=m +no_defs ', 'PROJCS["Tananarive (Paris) / Laborde Grid approximation",GEOGCS["Tananarive (Paris)",DATUM["Tananarive_1925_Paris",SPHEROID["International 1924",6378388,297,AUTHORITY["EPSG","7022"]],TOWGS84[-189,-242,-91,0,0,0,0],AUTHORITY["EPSG","6810"]],PRIMEM["Paris",2.33722917,AUTHORITY["EPSG","8903"]],UNIT["grad",0.01570796326794897,AUTHORITY["EPSG","9105"]],AUTHORITY["EPSG","4810"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],PROJECTION["Hotine_Oblique_Mercator"],PARAMETER["latitude_of_center",-21],PARAMETER["longitude_of_center",49],PARAMETER["azimuth",21],PARAMETER["rectified_grid_angle",21],PARAMETER["scale_factor",0.9995],PARAMETER["false_easting",400000],PARAMETER["false_northing",800000],AUTHORITY["EPSG","29702"],AXIS["Y",EAST],AXIS["X",NORTH]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 96703, 'sr-org', 6703, '', 'PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 9102745, 'esri', 102745, '+proj=tmerc +lat_0=42.5 +lon_0=-72.5 +k=0.9999642857142857 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs ', 'PROJCS["NAD_1983_StatePlane_Vermont_FIPS_4400_Feet",GEOGCS["GCS_North_American_1983",DATUM["North_American_Datum_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",1640416.666666667],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",-72.5],PARAMETER["Scale_Factor",0.9999642857142858],PARAMETER["Latitude_Of_Origin",42.5],UNIT["Foot_US",0.30480060960121924],AUTHORITY["EPSG","102745"]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 96700, 'sr-org', 6700, '+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +a=6378137 +b=6378136.027241431 +units=m +no_defs ', 'PROJCS["unnamed",GEOGCS["WGS 84",DATUM["unknown",SPHEROID["WGS84",6378137,6556752.3141]],PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["standard_parallel_1",17.5],PARAMETER["standard_parallel_2",29.5],PARAMETER["latitude_of_origin",12],PARAMETER["central_meridian",-102],PARAMETER["false_easting",2500000],PARAMETER["false_northing",0]]');
INSERT into spatial_ref_sys (srid, auth_name, auth_srid, proj4text, srtext) values ( 9102749, 'esri', 102749, '+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs ', 'PROJCS["NAD_1983_StatePlane_Washington_South_FIPS_4602_Feet",GEOGCS["GCS_North_American_1983",DATUM["North_American_Datum_1983",SPHEROID["GRS_1980",6378137,298.257222101]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["False_Easting",1640416.666666667],PARAMETER["False_Northing",0],PARAMETER["Central_Meridian",-120.5],PARAMETER["Standard_Parallel_1",45.83333333333334],PARAMETER["Standard_Parallel_2",47.33333333333334],PARAMETER["Latitude_Of_Origin",45.33333333333334],UNIT["Foot_US",0.30480060960121924],AUTHORITY["EPSG","102749"]]');