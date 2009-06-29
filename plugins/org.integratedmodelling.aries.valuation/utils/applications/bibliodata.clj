(modelling/with-kbox 
	
	(modelling/kbox esav_sources "postgres://postgres:rnbh304@localhost:5432/esav_sources" 
				:protocol "pg" 
				:metadata (
					:keywords         thinklab-core:Text
	        :title            thinklab-core:Text
	        :authors          thinklab-core:Text
	        :abstract         thinklab-core:Text)
				:sql.use.pooling "false" 
				:sql.log.queries "true")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always
		:metadata-generator {
							:keywords #(.get % "bibtex:hasKeywords")																										
							:title    #(.get % "bibtex:hasTitle")																										
							:authors  #(.get % "bibtex:hasAuthor")																										
							:abstract #(.get % "bibtex:hasAbstract")}
 
 ;; content; will become part of the kbox if evaluated within a with-kbox form
 (modelling/object 'bibtex:Article
	"Costanza et al., Nature, 1997."
	"The value of the world's ecosystem services and natural capital."
	(bibtex:hasJournal "Costanza, R., d'Arge, R., de Groot, R., Farber, S., Grasso, M., Hannon,B., Limburg,K., Naeem,S., O'Neill,R.V., Paruelo,J., Raskiin,R.G., Sutton,P., Van den Belt, M.")
	(bibtex:hasAuthor "Costanza, R., d'Arge, R., de Groot, R., Farber, S., Grasso, M., Hannon,B., Limburg,K., Naeem,S., O'Neill,R.V., Paruelo,J., Raskiin,R.G., Sutton,P., Van den Belt, M.")
	(bibtex:hasYear 1997)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "253-260")
	(bibtex:hasAbstract "The services of ecological systems and the natural capital stocks that produce them are critical to the functioning of the Earth's life-support system. They contribute to human welfare, both directly and indirectly, and therefore represent part of the total economic value of the planet. We have estimated the current economic value of 17 ecosystem services for 16 biomes, based on published studies and a few original calculations. For the entire biosphere, the value (most of which is outside the market) is estimated to be in the range of US$16-54 trillion (10(12)) per year, with an average of US$33 trillion per year. Because of the nature of the uncertainties, this must be considered a minimum estimate. Global gross national product total is around US$18 trillion per year.")
	(bibtex:hasKeywords "year trillion around total product national gross Global estimate minimum considered must this uncertainties nature Because average with range estimated outside which value biosphere entire calculations original studies published based services ecosystem economic current have planet part represent therefore indirectly directly both welfare human contribute They system Earth's functioning critical them produce that stocks capital natural systems ecological world's Belt Sutton Limburg Hannon Grasso Farber Groot Costanza Nature")
	) :id esdbr-2
 (modelling/object 'bibtex:Incollection
	"Kim, Global biogeochemical cycles, 1992."
	"The Global Carbon Cycle"
	(bibtex:hasBooktitle "Holmen Kim")
	(bibtex:hasAuthor "Holmen Kim")
	(bibtex:hasYear 1992)
	(bibtex:hasPages "239--262")
	(bibtex:hasKeywords "Cycle Carbon Global Holmen cycles biogeochemical The Kim")
	) :id esdbr-3
 (modelling/object 'bibtex:Article
	"Broecker et al., Journal of Geophysical Research, 1966."
	"Calcium carbonate precipitation on the Bahama Banks"
	(bibtex:hasJournal "Broecker,W.S., Takahashi,T.")
	(bibtex:hasAuthor "Broecker,W.S., Takahashi,T.")
	(bibtex:hasYear 1966)
	(bibtex:hasNumber 71)
	(bibtex:hasPages "1575-1602")
	(bibtex:hasKeywords "Banks Bahama precipitation carbonate Calcium Takahashi Research Geophysical Journal the")
	) :id esdbr-4)								
	