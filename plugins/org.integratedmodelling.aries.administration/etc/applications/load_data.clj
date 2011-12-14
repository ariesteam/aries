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

(ns administration.etc.applications.load-data
  (:refer tl :only [with-kbox kbox get-plugin-resource get-property-value])
  (:refer geospace :only [get-centroid get-bounding-box]))

(with-kbox

  (kbox aries-kbox "postgres://postgres:rnbh304@localhost:5432/ariesdata"
        :protocol            "pg"
        :schema              "postgis"
        :metadata            (:centroid    geospace:Point
                              :boundingbox geospace:Polygon
                              :dataset     thinklab-core:Text)
        :sql.log.queries     "false"
        :kbox.parameter.srid "4326")

  ;; rebuild the db from scratch every time this is run
  :storage-policy :recreate-always

  ;; put the kbox definition in the load area of the main plugin so it will be loaded at startup
  :persist org.integratedmodelling.aries.aries

  :metadata-generator {:centroid    #(get-centroid %)
                       :boundingbox #(get-bounding-box %)
                       :dataset     #(get-property-value % "metadata:belongsToDataset")}

  (import (get-plugin-resource 'aries.administration "public.xml"))
  (import (get-plugin-resource 'aries.administration "private.xml")))
