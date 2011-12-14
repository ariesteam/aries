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
