(modelling/with-kbox 
	
	(modelling/kbox aries-kbox "postgres://postgres:rnbh304@localhost:5432/aries" 
				:protocol "pg" 
				:schema "postgis"
				:metadata (
	        :centroid     geospace:Point
	        :boundingbox  geospace:Polygon)
				:sql.log.queries "true"
				:kbox.parameter.srid "4326")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always

		;; put the kbox definition in the load area of the core plugin so it will be loaded at startup
		:persist org.integratedmodelling.aries.core
		
		:metadata-generator {
			:centroid    #(geospace/get-centroid %)
			:boundingbox #(geospace/get-bounding-box %)
		}
		
		(import (tl/get-plugin-resource 'aries.administration "common.xml")) :pop 2
	)