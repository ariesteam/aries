(modelling/with-kbox 
	
	(modelling/kbox aries-kbox "postgres://postgres:rnbh304@localhost:5432/aries" 
				:protocol "pg" 
				:schema "postgis"
				:metadata (
	        :centroid     geospace:Point
	        :boundingbox  geospace:Polygon)
				:sql.log.queries "true")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always
		:metadata-generator {
			:centroid    #(geospace/get-centroid %)
			:boundingbox #(geospace/get-bounding-box %)
		}
	
	 ;; admin data
	 ;(import (tl/get-plugin-resource 'aries.administration "world_adm0.shp")) 
	 
	 	;; BUG - FIXME - needs modifiers or will gobble up the next instruction
	 	;:pop 2

	 ;; puget sound aesthetic values
	 (import (tl/get-plugin-resource 'aries.administration "common.xml")) :pop 2
	 (import (tl/get-plugin-resource 'aries.administration "pugetsound_aesthetics.xml")) :pop 2
	 ; (import (tl/get-plugin-resource 'aries.administration "pugetsound_carbon.xml"))
)	