;;; Copyright 2010 Gary W. Johnson (lambdatronic@gmail.com)
;;;
;;; This file is part of geo-xls.
;;;
;;; geo-xls is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; geo-xls is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with geo-xls.  If not, see <http://www.gnu.org/licenses/>.
{
  :spreadsheet-filename  "../etc/catalog/Geoserver_REST_database.xls"
  :spreadsheet-sheetname "Sheet1"
  :column-spec           {:Workspace    :A
                          :Store        :B
                          :Layer        :C
                          :Description  :D
                          :URI          :E
                          :DefaultStyle :F
                          :NativeSRS    :J
                          :DeclaredSRS  :K}
  :namespace-prefix      "http://www.integratedmodelling.org/geo/ns/"
  :geoserver-rest-uri    "http://ecoinformatics.uvm.edu/geoserver/rest"
  :geoserver-username    "admin"
  :geoserver-password    "rnbh304"
  :geoserver-data-dir    "/opt/geoserver-2.4.1/data_dir/"
  :postgis-user          "aries"
}
