(let 
	[kbox (modelling/kbox geodata "postgres://postgres:rnbh304@localhost:5432/geodata" 
					:protocol "pg" 
					:schema "postgis"
					:metadata (
						:country      thinklab-core:Text
	        	:gmicode      thinklab-core:Text
	       	  :region       thinklab-core:Text
	       	  :centroid     geospace:Point
	          :boundingbox  geospace:Polygon)run g
	        :parameters (:srid 4326)
			  	:sql.use.pooling "false" 
				  :sql.log.queries "true")
		 qres (tl/query kbox '(observation:Observation (boundingbox contains "POINT (12 32)")))]
	(println "Nres = " (.getResultCount qres)))