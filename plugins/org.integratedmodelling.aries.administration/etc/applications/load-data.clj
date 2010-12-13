(ns core.tasks
  (:refer tl :only (with-kbox kbox get-plugin-resource get-property-value))
  (:refer geospace :only (get-centroid get-bounding-box)))

(with-kbox 
	
	(kbox aries-kbox "postgres://postgres:rnbh304@localhost:5432/ariesdata" 
				:protocol "pg" 
				:schema "postgis"
				:metadata (
	        :centroid     geospace:Point
	        :boundingbox  geospace:Polygon
          :dataset      thinklab-core:Text)
				:sql.log.queries "true"
				:kbox.parameter.srid "4326")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always

		;; put the kbox definition in the load area of the main plugin so it will be loaded at startup
		:persist org.integratedmodelling.aries.aries
		
		:metadata-generator {
			:centroid    #(get-centroid %)
			:boundingbox #(get-bounding-box %)
      :dataset     #(get-property-value % "metadata:belongsToDataset")
		}
		
		(import (get-plugin-resource 'aries.administration "ark.xml"))
		(import (get-plugin-resource 'aries.administration "common.xml"))
		(import (get-plugin-resource 'aries.administration "ipcc.xml"))
		(import (get-plugin-resource 'aries.administration "nlcd2001.xml"))
		(import (get-plugin-resource 'aries.administration "agri.xml"))
		(import (get-plugin-resource 'aries.administration "marine.xml"))
		(import (get-plugin-resource 'aries.administration "kb1.xml"))
		(import (get-plugin-resource 'aries.administration "bv1.xml"))
		(import (get-plugin-resource 'aries.administration "raven_ridge_viewsheds.xml")) 
	)