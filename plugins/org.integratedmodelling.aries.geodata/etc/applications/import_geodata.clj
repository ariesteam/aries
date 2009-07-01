(modelling/with-kbox 
	
	(modelling/kbox geodata "postgres://postgres:rnbh304@localhost:5432/geodata" 
				:protocol "pg" 
				:schema "postgis"
				:metadata (
					:country      thinklab-core:Text
	        :gmicode      thinklab-core:Text
	        :region       thinklab-core:Text
	        :centroid     geospace:Point
	        :boundingbox  geospace:Polygon)
				:sql.use.pooling "false" 
				:sql.log.queries "true")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always
		:metadata-generator {}
	
	 ;; admin data
	 (import (tl/get-plugin-resource 'aries.geodata "world_adm0.shp")) 
	 
	 	;; BUG - FIXME - needs modifiers or will gobble up the next instruction
	 	:pop 2

	 ;; puget sound aesthetic values
	 (import (tl/get-plugin-resource 'aries.geodata "pugetsound_aesthetics.xml"))
	 
)	