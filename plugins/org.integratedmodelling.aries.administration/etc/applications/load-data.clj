(modelling/with-kbox 
	
	(modelling/kbox aries-kbox "postgres://postgres:rnbh304@localhost:5432/ariesdata" 
				:protocol "pg" 
				:schema "postgis"
				:metadata (
	        :centroid     geospace:Point
	        :boundingbox  geospace:Polygon)
				:sql.log.queries "true"
				:kbox.parameter.srid "4326")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always

		;; put the kbox definition in the load area of the main plugin so it will be loaded at startup
		:persist org.integratedmodelling.aries.aries
		
		:metadata-generator {
			:centroid    #(geospace/get-centroid %)
			:boundingbox #(geospace/get-bounding-box %)
		}
		
		(import (tl/get-plugin-resource 'aries.administration "ark.xml"))
		(import (tl/get-plugin-resource 'aries.administration "common.xml"))
		(import (tl/get-plugin-resource 'aries.administration "marine.xml"))
	)