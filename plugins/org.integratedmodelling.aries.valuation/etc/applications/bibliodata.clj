(modelling/with-kbox 
	
	(modelling/kbox esav_sources "postgres://postgres:rnbh304@localhost:5432/esav_sources" 
				:protocol "pg" 
				:metadata (
					:keywords         thinklab-core:Text
	        :title            thinklab-core:Text
	        :authors          thinklab-core:Text
	        :abstract         thinklab-core:Text)
				:sql.use.pooling "true" 
				:sql.log.queries "true")
				
	  ;; rebuild the db from scratch every time this is run
		:storage-policy :recreate-always
		:metadata-generator {
							:keywords #(.get % "bibtex:hasKeywords")																										
							:title    #(.get % "bibtex:hasTitle")																										
							:authors  #(.get % "bibtex:hasAuthor")																										
							:abstract #(.get % "bibtex:hasAbstract")}
							
 (modelling/object 'bibtex:Article
	"Costanza et al., Nature, 1997."
	"The value of the world's ecosystem services and natural capital."
	(bibtex:hasJournal "Costanza, R., d'Arge, R., de Groot, R., Farber, S., Grasso, M., Hannon,B., Limburg,K., Naeem,S., O'Neill,R.V., Paruelo,J., Raskiin,R.G., Sutton,P., Van den Belt, M.")
	(bibtex:hasAuthor "Costanza, R., d'Arge, R., de Groot, R., Farber, S., Grasso, M., Hannon,B., Limburg,K., Naeem,S., O'Neill,R.V., Paruelo,J., Raskiin,R.G., Sutton,P., Van den Belt, M.")
	(bibtex:hasTitle "The value of the world's ecosystem services and natural capital.")
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
	(bibtex:hasTitle "The Global Carbon Cycle")
	(bibtex:hasYear 1992)
	(bibtex:hasPages "239--262")
	(bibtex:hasKeywords "Cycle Carbon Global Holmen cycles biogeochemical The Kim")
	) :id esdbr-3
 (modelling/object 'bibtex:Article
	"Broecker et al., Journal of Geophysical Research, 1966."
	"Calcium carbonate precipitation on the Bahama Banks"
	(bibtex:hasJournal "Broecker,W.S., Takahashi,T.")
	(bibtex:hasAuthor "Broecker,W.S., Takahashi,T.")
	(bibtex:hasTitle "Calcium carbonate precipitation on the Bahama Banks")
	(bibtex:hasYear 1966)
	(bibtex:hasNumber 71)
	(bibtex:hasPages "1575-1602")
	(bibtex:hasKeywords "Banks Bahama precipitation carbonate Calcium Takahashi Research Geophysical Journal the")
	) :id esdbr-4
 (modelling/object 'bibtex:Article
	"Brookshire et al., American Economic Review, 1982."
	"Valuing public goods: a comparision of survey and hedonic approach"
	(bibtex:hasJournal "Brookshire,D., Thayer,M.A., Schulze,W.D., D'Arge,R.C.")
	(bibtex:hasAuthor "Brookshire,D., Thayer,M.A., Schulze,W.D., D'Arge,R.C.")
	(bibtex:hasTitle "Valuing public goods: a comparision of survey and hedonic approach")
	(bibtex:hasYear 1982)
	(bibtex:hasNumber 72)
	(bibtex:hasPages "165-177")
	(bibtex:hasKeywords "approach hedonic survey goods public Valuing Schulze Thayer Brookshire Review Economic American and")
	) :id esdbr-13
 (modelling/object 'bibtex:Article
	"Burke et al., Ecological Applications, 1995."
	"Soil organic matter recovery in semiarid grasslands: implications for the conservation reserve program."
	(bibtex:hasJournal "Burke,I.C., Lauenroth,W.K., Coffin,D.P.")
	(bibtex:hasAuthor "Burke,I.C., Lauenroth,W.K., Coffin,D.P.")
	(bibtex:hasTitle "Soil organic matter recovery in semiarid grasslands: implications for the conservation reserve program.")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 5)
	(bibtex:hasPages "793-801")
	(bibtex:hasAbstract "Although the effects of cultivation on soil organic matter and nutrient supply capacity are well understood, relatively little work has been done on the long-term recovery of soils from cultivation. We sampled soils from 12 locations within the Pawnee National Grasslands of northeastern Colorado, each having native fields and fields that were historically cultivated but abandoned 50 yr ago. We also sampled fields that had been cultivated for at least 50 yr at 5 of these locations. Our results demonstrated that soil organic matter, silt content, microbial biomass, potentially mineralizable N, and potentially respirable C were significantly lower on cultivated fields than on native fields. Both cultivated and abandoned fields also had significantly lower soil organic matter and silt contents than native fields. Abandoned fields, however, were not significantly different from native fields with respect to microbial biomass, potentially mineralizable N, or respirable C. In addition, we found that the characteristic small-scale heterogeneity of the shortgrass steppe associated with individuals of the dominant plant, Bouteloua gracilis, had recovered on abandoned fields. Soil beneath plant canopies had an average of 200 g/m(2) more C than between-plant locations. We suggest that 50 yr is an adequate time for recovery of active soil organic matter and nutrient availability, but recovery of total soil organic matter pools is a much slower process. Plant population dynamics may play an important role in the recovery of shortgrass steppe ecosystems from disturbance, such that establishment of perennial grasses determines the rate of organic matter recovery.")
	(bibtex:hasKeywords "recovery matter organic rate determines grasses perennial establishment that such disturbance from ecosystems steppe role important play dynamics population Plant process slower pools soil total availability nutrient active time adequate suggest locations than more average canopies plant beneath Soil fields abandoned recovered dominant individuals with associated heterogeneity characteristic found addition respirable mineralizable potentially biomass microbial respect native different significantly were however Abandoned contents silt lower also cultivated Both content demonstrated results these least been sampled historically having each Colorado northeastern Grasslands National Pawnee within soils cultivation term long done work little relatively understood well capacity supply effects Although program reserve conservation implications grasslands semiarid Coffin Lauenroth Burke Applications Ecological")
	) :id esdbr-14
 (modelling/object 'bibtex:Article
	"Bergstrom et al., Ecological Economics, 1990."
	"Economic value of wetlands-based recreation"
	(bibtex:hasJournal "Bergstrom,J.C., Soll, J.R., Titre,J.P., Wright,V.L.")
	(bibtex:hasAuthor "Bergstrom,J.C., Soll, J.R., Titre,J.P., Wright,V.L.")
	(bibtex:hasTitle "Economic value of wetlands-based recreation")
	(bibtex:hasYear 1990)
	(bibtex:hasNumber 2)
	(bibtex:hasPages "129-147")
	(bibtex:hasKeywords "recreation value Economic Wright Titre Soll Bergstrom Economics Ecological")
	) :id esdbr-15
 (modelling/object 'bibtex:Article
	"Bockstael et al., Ecological Economics, 1995."
	"Ecological economic modeling and valuation of ecosystems"
	(bibtex:hasJournal "Bockstael,N., Costanza,R., Strand,I., Boynton,W. Bell,K.,Kainger,L.")
	(bibtex:hasAuthor "Bockstael,N., Costanza,R., Strand,I., Boynton,W. Bell,K.,Kainger,L.")
	(bibtex:hasTitle "Ecological economic modeling and valuation of ecosystems")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 14)
	(bibtex:hasPages "143-159")
	(bibtex:hasAbstract "We are attempting to integrate ecological and economic modeling and analysis in order to improve our understanding of regional systems, assess potential future impacts of various land-use, development, and agricultural policy options, and to better assess the value of ecological systems. Starting with an existing spatially articulated ecosystem model of the Patuxent River drainage basin in Maryland, we are adding modules to endogenize the agricultural components of the system (especially the impacts of agricultural practices and crop choice) and the process of land-use decision making. The integrated model will allow us to evaluate the indirect effects over long time horizons of current policy options. These effects are almost always ignored in partial analyses, although they may be very significant and may reverse many long-held assumptions and policy predictions. This paper is a progress report on this modeling effort, indicating our motivations, ideas, and plans for completion.")
	(bibtex:hasKeywords "completion plans ideas motivations indicating effort modeling this report progress paper This predictions policy assumptions many reverse significant very they although analyses partial ignored always almost effects These options current horizons time long over indirect evaluate allow will model integrated making decision land process crop practices agricultural impacts system components modules adding Maryland basin drainage River Patuxent ecosystem articulated spatially existing with Starting systems ecological value assess better development various future potential regional understanding improve order analysis economic integrate attempting ecosystems valuation Ecological Boynton Strand Costanza Economics")
	) :id esdbr-16
 (modelling/object 'bibtex:Article
	"Burke et al., Soil Science. Society of Amerian Journal, 1989."
	"Texture, climate, and cultivaiton effects on soil organic content in US grassland soils."
	(bibtex:hasJournal "Burke,I.C., Yonker,C.M., Parton,W.J., Cole,C.V., Flach,K., Schimel, D.S.")
	(bibtex:hasAuthor "Burke,I.C., Yonker,C.M., Parton,W.J., Cole,C.V., Flach,K., Schimel, D.S.")
	(bibtex:hasTitle "Texture, climate, and cultivaiton effects on soil organic content in US grassland soils.")
	(bibtex:hasYear 1989)
	(bibtex:hasNumber 53)
	(bibtex:hasPages "800-805")
	(bibtex:hasKeywords "soils grassland content organic soil effects climate Texture Schimel Flach Cole Parton Yonker Burke Journal Society Science Soil")
	) :id esdbr-17
 (modelling/object 'bibtex:Article
	"Chopra, Economic Botany, 1993."
	"The value of non-timber forest products: an estimation for tropical deciduous forests in India."
	(bibtex:hasJournal "Chopra,K.")
	(bibtex:hasAuthor "Chopra,K.")
	(bibtex:hasTitle "The value of non-timber forest products: an estimation for tropical deciduous forests in India.")
	(bibtex:hasYear 1993)
	(bibtex:hasNumber 47)
	(bibtex:hasPages "251-257")
	(bibtex:hasAbstract "Alteration of forested land to other uses incurs costs to a nation's economy that are frequently not calculated in estimates of national productivity. Alternative methods of value are discussed and employed to evaluate the non-timber products of India's tropical deciduous forests. Many undervalued resources found in these forests are described, and monetary value ascribed to them, to demonstrate the importance of the resources to the Indian economy.")
	(bibtex:hasKeywords "economy Indian resources importance demonstrate them ascribed value monetary described forests these found undervalued Many deciduous tropical India's products evaluate employed discussed methods Alternative productivity national estimates calculated frequently that nation's costs incurs uses other land forested Alteration India estimation forest Chopra Botany Economic")
	) :id esdbr-18
 (modelling/object 'bibtex:Article
	"Costanza et al., Man, Environment, Space and Time, 1985."
	"Theories and methods of valuation ofnatural systems: a comparison of willingness-to-pay and energy analysis based approaches."
	(bibtex:hasJournal "Costanza, R., Farber,S.C.")
	(bibtex:hasAuthor "Costanza, R., Farber,S.C.")
	(bibtex:hasTitle "Theories and methods of valuation ofnatural systems: a comparison of willingness-to-pay and energy analysis based approaches.")
	(bibtex:hasYear 1985)
	(bibtex:hasNumber 4)
	(bibtex:hasPages "1-38")
	(bibtex:hasKeywords "approaches based analysis energy comparison systems valuation methods Theories Farber Costanza Time Space Environment")
	) :id esdbr-19
 (modelling/object 'bibtex:Article
	"Copeland et al., Journal of Geophysical Research, 1996."
	"Potential climatic impacts of vegetation change: a regional modelling study."
	(bibtex:hasJournal "Copeland,J.H., Pielke,R.A., Kittel,T.G.F.")
	(bibtex:hasAuthor "Copeland,J.H., Pielke,R.A., Kittel,T.G.F.")
	(bibtex:hasTitle "Potential climatic impacts of vegetation change: a regional modelling study.")
	(bibtex:hasYear 1996)
	(bibtex:hasNumber 101)
	(bibtex:hasPages "7409")
	(bibtex:hasAbstract "The human species has been modifying the landscape long before the development of modern agrarian techniques. Much of the land area of the conterminous United States is currently used for agricultural production. In certain regions this change in vegetative cover from its natural state may have led to local climatic change. A regional climate version of the Colorado State University Regional Atmospheric Modeling System was used to assess the impact of a natural versus current vegetation distribution on the weather and climate of July 1989. The results indicate that coherent regions of substantial changes, of both positive and negative sign, in screen height temperature, humidity, wind speed, and precipitation are a possible consequence of land use change throughout the United States. The simulated changes in the screen height quantities were closely related to changes in the vegetation parameters of albedo, roughness length, leaf area index, and fractional coverage.")
	(bibtex:hasKeywords "coverage fractional index area leaf length roughness albedo parameters vegetation changes related closely were quantities height screen simulated States United throughout change land consequence possible precipitation speed wind humidity temperature sign negative positive both substantial regions coherent that indicate results July climate weather distribution current versus natural impact assess used System Modeling Atmospheric Regional University State Colorado version regional climatic local have state from cover vegetative this certain production agricultural currently conterminous Much techniques agrarian modern development before long landscape modifying been species human study modelling impacts Potential Kittel Copeland Research Geophysical Journal")
	) :id esdbr-20
 (modelling/object 'bibtex:Article
	"Crawford et al., Ecology, 1989."
	"Predation by birds on spruce budworm, Choristoneura fumiferana: functional, nemerical and total responses."
	(bibtex:hasJournal "Crawford, H.S., Jennings,D.T.")
	(bibtex:hasAuthor "Crawford, H.S., Jennings,D.T.")
	(bibtex:hasTitle "Predation by birds on spruce budworm, Choristoneura fumiferana: functional, nemerical and total responses.")
	(bibtex:hasYear 1989)
	(bibtex:hasNumber 70)
	(bibtex:hasPages "152-163")
	(bibtex:hasKeywords "responses total functional spruce birds Predation Jennings Crawford Ecology")
	) :id esdbr-21
 (modelling/object 'bibtex:Article
	"Cruz et al., Journal of Philippine Development, 1988."
	"The on-site and downstream costs of soil erosion in the Magat and Pantabangan watersheds."
	(bibtex:hasJournal "Cruz,W., Francisco,H.A., Conway, Z.T.")
	(bibtex:hasAuthor "Cruz,W., Francisco,H.A., Conway, Z.T.")
	(bibtex:hasTitle "The on-site and downstream costs of soil erosion in the Magat and Pantabangan watersheds.")
	(bibtex:hasYear 1988)
	(bibtex:hasNumber 26)
	(bibtex:hasPages "85-")
	(bibtex:hasKeywords "watersheds Magat erosion soil costs downstream Conway Francisco Cruz Development Philippine Journal")
	) :id esdbr-22
 (modelling/object 'bibtex:Article
	"Dixon et al., Tropical Coastal Area Management, 1988."
	"Economic evaluaiton of Coastal Resources: the El Nino Study."
	(bibtex:hasJournal "Dixon,J.A., Hodgson,G.")
	(bibtex:hasAuthor "Dixon,J.A., Hodgson,G.")
	(bibtex:hasTitle "Economic evaluaiton of Coastal Resources: the El Nino Study.")
	(bibtex:hasYear 1988)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "5-7")
	(bibtex:hasKeywords "Study Nino Resources Coastal Economic Hodgson Dixon Management Area Tropical")
	) :id esdbr-23
 (modelling/object 'bibtex:Article
	"Echeverria et al., Ecological Economics, 1995."
	"Valuation of non-priced amenities provided by the biological resources whithin the Monteverde Cloud Forest preserve, Costa Rita."
	(bibtex:hasJournal "Echeverria,J., Hanrahan,M., Solorzano,R.")
	(bibtex:hasAuthor "Echeverria,J., Hanrahan,M., Solorzano,R.")
	(bibtex:hasTitle "Valuation of non-priced amenities provided by the biological resources whithin the Monteverde Cloud Forest preserve, Costa Rita.")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 13)
	(bibtex:hasPages "43-52")
	(bibtex:hasAbstract "To quantify the economic benefits of the Monteverde Cloud Forest Preserve and to test the contingent valuation method in a third world setting, a contingent valuation survey was designed with five experimental treatments. These determined an overall expected value per visitor; determined and compared two ways of eliciting value, single versus annual lump-sum payments; and compared average values of Costa Rican versus non-Costa Rican visitors. Visitors were willing to pay to prevent the Preserve's conversion to agricultural uses. Monteverde's value as a cloud forest preserve appears much higher than any value it might have in agricultural use. Despite lower incomes, Costa Rican visitors valued the Preserve more highly than non-Costa Rican visitors. Visitors may have differentiated only weakly between greatly differing bid amounts. Expected values derived from econometric analysis of the differing experimental treatments suggest that further methodological adaptation of the contingent valuation method may be required (1) when it is applied in third world settings, and (2) when precision is critical in estimating WTPs.")
	(bibtex:hasKeywords "estimating critical precision when settings world third applied required method valuation contingent adaptation methodological further that suggest treatments experimental differing analysis econometric from derived values Expected amounts greatly between weakly only differentiated have Visitors visitors Rican than highly more Preserve valued Costa incomes lower Despite agricultural might value higher much appears preserve forest cloud uses conversion prevent willing were versus average compared payments annual single eliciting ways determined visitor expected overall These five with designed survey setting test Forest Cloud Monteverde benefits economic quantify Rita resources biological provided amenities Valuation Solorzano Hanrahan Echeverria Economics Ecological")
	) :id esdbr-24
 (modelling/object 'bibtex:Article
	"Edwards, Coastal Management, 1991."
	"The demand for Galapagos vacations: estimation and application to wildness preservation."
	(bibtex:hasJournal "Edwards, S.F.")
	(bibtex:hasAuthor "Edwards, S.F.")
	(bibtex:hasTitle "The demand for Galapagos vacations: estimation and application to wildness preservation.")
	(bibtex:hasYear 1991)
	(bibtex:hasNumber 19)
	(bibtex:hasPages "155-199")
	(bibtex:hasAbstract "The demand for Galapagos vacations by ecotourists was estimated using hedonic demand analysis, a technique being developed by environmental economists. In turn, the demand model was used to explore how a fiscal policy of maximizing tax revenues could help to satisfy the alleged incompatible goals of wilderness preservation and economic growth. Constrained to a carrying capacity of 125,000 visitor days, there is potential to raise about $30 million (U.S.) by substantially increasing the entrance fee charged to ecotourists. This tax revenue could be used to finance both conservation of the Galapagos wilderness and investment in Ecuador's economy.")
	(bibtex:hasKeywords "economy investment wilderness Galapagos conservation both finance used could revenue This charged entrance increasing substantially million about raise potential there days visitor capacity carrying Constrained growth economic preservation goals incompatible alleged satisfy help revenues maximizing policy fiscal explore model demand turn economists environmental developed being technique analysis hedonic using estimated vacations wildness application estimation Edwards Management Coastal")
	) :id esdbr-25
 (modelling/object 'bibtex:Article
	"Farber et al., Journal of Environmental Management, 1987."
	"The economic value of wetlands systems."
	(bibtex:hasJournal "Farber,S., Costanza, R.")
	(bibtex:hasAuthor "Farber,S., Costanza, R.")
	(bibtex:hasTitle "The economic value of wetlands systems.")
	(bibtex:hasYear 1987)
	(bibtex:hasNumber 24)
	(bibtex:hasPages "41-51")
	(bibtex:hasKeywords "systems wetlands value economic Costanza Farber Management Environmental Journal")
	) :id esdbr-26
 (modelling/object 'bibtex:Article
	"Farnworth et al., Interm. J. Environmental. Studies, 1983."
	"Asynthesis of ecological and economic theory toward more complete valuation of tropical moist forest."
	(bibtex:hasJournal "Farnworth, E.G., Tidrick, T.H., Smather, W.M., Jordan, C.F.")
	(bibtex:hasAuthor "Farnworth, E.G., Tidrick, T.H., Smather, W.M., Jordan, C.F.")
	(bibtex:hasTitle "Asynthesis of ecological and economic theory toward more complete valuation of tropical moist forest.")
	(bibtex:hasYear 1983)
	(bibtex:hasNumber 21)
	(bibtex:hasPages "11-28")
	(bibtex:hasKeywords "forest moist tropical valuation complete more toward theory economic ecological Jordan Tidrick Farnworth")
	) :id esdbr-27
 (modelling/object 'bibtex:Article
	"Gill, Acta Horticulturae, 1991."
	"The value of honeybee pollination to society."
	(bibtex:hasJournal "Gill,R.A.")
	(bibtex:hasAuthor "Gill,R.A.")
	(bibtex:hasTitle "The value of honeybee pollination to society.")
	(bibtex:hasYear 1991)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "62-68")
	(bibtex:hasKeywords "society pollination honeybee value Gill Acta")
	) :id esdbr-28
 (modelling/object 'bibtex:Article
	"Godoy et al., Economic Botany, 1993."
	"Amethod for the economic valuation of non-timber tropocal forest products."
	(bibtex:hasJournal "Godoy,R., Lubowski,R., Markandya,A.")
	(bibtex:hasAuthor "Godoy,R., Lubowski,R., Markandya,A.")
	(bibtex:hasTitle "Amethod for the economic valuation of non-timber tropocal forest products.")
	(bibtex:hasYear 1993)
	(bibtex:hasNumber 47)
	(bibtex:hasPages "220-223")
	(bibtex:hasAbstract "By drawing on quantitative studies in social anthropology, zoology, ethnobotany, and economics we present a method for conducting an economic valuation of non-timber forest products. A review of 24 studies suggests that the median value for non-timber forest products is about $50/ha/year. We discuss problems with past studies and suggest ways to get better estimates of output quantities, marginal costs, and prices.")
	(bibtex:hasKeywords "prices costs marginal quantities output estimates better ways suggest studies past with problems discuss about products forest value median that suggests review valuation economic conducting method present economics ethnobotany zoology anthropology social quantitative drawing Godoy Botany Economic")
	) :id esdbr-29
 (modelling/object 'bibtex:Article
	"Gren et al., Environmental and Resource Economics, 1994."
	"Primary and secondaary values of wetlands ecosystems."
	(bibtex:hasJournal "Gren,I.M., Folke,C. et al")
	(bibtex:hasAuthor "Gren,I.M., Folke,C. et al")
	(bibtex:hasTitle "Primary and secondaary values of wetlands ecosystems.")
	(bibtex:hasYear 1994)
	(bibtex:hasNumber 4)
	(bibtex:hasPages "55-74")
	(bibtex:hasKeywords "ecosystems wetlands values Primary Gren Economics Resource Environmental")
	) :id esdbr-30
 (modelling/object 'bibtex:Article
	"Grimes et al., Ambio, 1994."
	"Valuing the Rain Forest: the economic value of nontimber forest products in Ecuador."
	(bibtex:hasJournal "Grimes, A., Loomis, S., Jahnige,P., Burnham,M., Onthank,K., Alarcon,R., Cuenca, W.P., Martinez,C.C., Neil,D., Balick, M., Bennett,B., Mendelsohn,R..")
	(bibtex:hasAuthor "Grimes, A., Loomis, S., Jahnige,P., Burnham,M., Onthank,K., Alarcon,R., Cuenca, W.P., Martinez,C.C., Neil,D., Balick, M., Bennett,B., Mendelsohn,R..")
	(bibtex:hasTitle "Valuing the Rain Forest: the economic value of nontimber forest products in Ecuador.")
	(bibtex:hasYear 1994)
	(bibtex:hasNumber 23)
	(bibtex:hasPages "405-410")
	(bibtex:hasAbstract "This study calculates the value of three ha of primary forest in the Upper Napo region of Amazonian Ecuador based on the potential extraction of nontimber forest products (NTFPs). Through ethnobotanical and market surveys, the annual harvest levels, market prices and extraction costs of seven fruits, three medicinal barks, and one resin are measured. The present value of net revenue from NTFP collection is USD 2830 in the upland plots and USD 1257 in the alluvial plot which is significantly higher than the returns from alternative land uses in this area.")
	(bibtex:hasKeywords "area this uses land alternative from returns than higher significantly which plot alluvial plots upland collection revenue value present measured resin barks medicinal three fruits seven costs extraction prices market levels harvest annual surveys Through products forest potential based Ecuador region Upper primary calculates study This economic Forest Rain Valuing Mendelsohn Bennett Balick Neil Martinez Cuenca Alarcon Burnham Loomis Grimes")
	) :id esdbr-31
 (modelling/object 'bibtex:Article
	"Gupta et al., American Journal of Agricultural Economics, 1975."
	"Economic creteriz for freshwater wetland policy in Massachussettss."
	(bibtex:hasJournal "Gupta, T.R., Forster,J.H.")
	(bibtex:hasAuthor "Gupta, T.R., Forster,J.H.")
	(bibtex:hasTitle "Economic creteriz for freshwater wetland policy in Massachussettss.")
	(bibtex:hasYear 1975)
	(bibtex:hasNumber 57)
	(bibtex:hasPages "40-45")
	(bibtex:hasKeywords "policy wetland freshwater Economic Forster Gupta Economics Agricultural Journal American")
	) :id esdbr-32
 (modelling/object 'bibtex:Article
	"Hickman, Forest Ecology and Management, 1990."
	"Forested-wetland trends in the United States: an economic perspective."
	(bibtex:hasJournal "Hickman,C")
	(bibtex:hasAuthor "Hickman,C")
	(bibtex:hasTitle "Forested-wetland trends in the United States: an economic perspective.")
	(bibtex:hasYear 1990)
	(bibtex:hasNumber 33)
	(bibtex:hasPages "227-238")
	(bibtex:hasKeywords "perspective economic States United trends Hickman Management Ecology Forest")
	) :id esdbr-33
 (modelling/object 'bibtex:Article
	"Higgins et al., Ecological Economics, 1997."
	"An economical simulation model of mountain fynbos ecosystems: dynamics, valuation and management."
	(bibtex:hasJournal "Higgins, S.I., Turpie, J.K., Costanza,R., Cowling,R.W., Le Maitre, D.C., Marais, C., Midgley, G.F.")
	(bibtex:hasAuthor "Higgins, S.I., Turpie, J.K., Costanza,R., Cowling,R.W., Le Maitre, D.C., Marais, C., Midgley, G.F.")
	(bibtex:hasTitle "An economical simulation model of mountain fynbos ecosystems: dynamics, valuation and management.")
	(bibtex:hasYear 1997)
	(bibtex:hasNumber 22)
	(bibtex:hasAbstract "Mountain fynbos ecosystems in South Africa are threatened by alien plant invasions and by a lack of funding for effective management of these invasions. This paper develops an ecological-economic argument for the effective management of plant invasions in mountain fynbos ecosystems. We do this by building a dynamic ecological economic model which values the ecosystem services that fynbos ecosystems provide under different management regimes. We propose that the services that mountain fynbos ecosystems provide fall into six components: water production, wildflower harvest, hiker visitation, ecotourist visitation, endemic species and genetic storage. A scenario analysis based on a hypothetical 4 km(2) mountain fynbos ecosystem in the western part of the fynbos biome estimated that the ecosystem's value varies from R19 million (under low valuation and poor management scenario) to R300 million (under high valuation and good management scenario) [R4.50=US$1]. Water production and genetic storage were the most valuable ecosystem services. The model showed that the cost of clearing alien plants (under the proactive management scenario) was a tiny (0.6-5%) proportion of the value of mountain fynbos ecosystems. This result motivates an injection of funds for clearing alien plants from mountain fynbos ecosystems. (C) 1997 Elsevier Science B.V.")
	(bibtex:hasKeywords "Science Elsevier ecosystems mountain from plants alien clearing funds injection motivates result This value proportion tiny management proactive cost that showed model services ecosystem valuable most were storage genetic production Water good valuation high million poor varies estimated biome part western hypothetical based analysis scenario species endemic visitation hiker harvest wildflower water components into fall provide propose regimes different under values which economic ecological dynamic building this invasions plant effective argument develops paper these funding lack threatened Africa South Mountain dynamics simulation economical Midgley Maitre Cowling Costanza Higgins Economics Ecological")
	) :id esdbr-34
 (modelling/object 'bibtex:Article
	"Houde et al., Estuaries, 1993."
	"Recent trends in estuarine fisheries: predictions of fish production and yield."
	(bibtex:hasJournal "Houde,E.D., Rutherford, E.S.")
	(bibtex:hasAuthor "Houde,E.D., Rutherford, E.S.")
	(bibtex:hasTitle "Recent trends in estuarine fisheries: predictions of fish production and yield.")
	(bibtex:hasYear 1993)
	(bibtex:hasNumber 16)
	(bibtex:hasPages "161-176")
	(bibtex:hasAbstract "Trends in global and United States fish catches were examined to determine the status of estuarine fisheries yields relative to those from other ecosystems. Potential marine fish production, based upon primary production relationships, was estimated globally and for specific marine ecosystems, including estuaries. While global fish catches increased substantially during the past two decades and continued to increase through 1989, catches of estuarine-dependent species have peaked or stabilized. In the United States, total catches have increased but many estuarine-dependent fisheries have declined, although the declines in catches are no more dramatic than those of heavily-fished continental shelf species. Overfishing probably is the primary cause of declines in estuarine and shelf fisheries. A few estuarine-dependent species of the United States have experienced substantial increases in harvests since 1970, for example, Pacific salmons, menhaden, and penaeid shrimps. The percentage contribution of major estuarine fisheries to the United States commercial catch declined between 1970 and 1990, although the yield of these species increased substantially. Global marine fisheries production at trophic level 2.5 was estimated to be 1,359 million tons. Potential yield was estimated to be 307 million tons, but the 1989 world marine catch was only 86.5 million tons. The major fraction, 196 million tons, of the estimated potential yield was for the open ocean where technological constraints may prevent its full realization. Of the remaining 111 million tons of the potential, 18.0 million tons (16.2%) may come from estuaries and probably already is fully exploited. The potential catches from shelves, 68.5 million tons (61.6%), and upwelling areas, 24.8 million tons (22.2%), while considerably larger than those from estuaries, are lower in a relative sense (per unit area) than fisheries production and potential catch in estuarine zones. Relationships between fish production, fish harvest, and primary production were examined in specific estuaries. The developing role of aquaculture and its effect on estuarine fisheries are discussed")
	(bibtex:hasKeywords "discussed fisheries estuarine effect aquaculture role developing estuaries specific examined were production primary harvest fish between Relationships exploited fully already probably from come tons million potential remaining realization full prevent constraints technological where ocean open yield estimated fraction major only catch marine world Potential level trophic Global substantially increased species these although declined commercial States United contribution percentage shrimps menhaden salmons Pacific example since harvests increases substantial experienced have shelf declines cause continental those than dramatic more catches many total stabilized peaked through increase continued decades past during global While including ecosystems globally relationships upon based other relative yields status determine Trends predictions trends Recent Rutherford Houde Estuaries The and its are")
	) :id esdbr-35
 (modelling/object 'bibtex:Article
	"Howarth et al., Biogeochemistry, 1996."
	"Regional nitrogen budgets and riverine N and P fluxes of the drainages to the North Atlantic Ocean: Natural and human influences."
	(bibtex:hasJournal "Howarth,R.W., Billen, G., Swaney,D., Townsend, A., Jaworski,N., Lajtha,K., Downing, J.A., Elmgren,R., Caraco,N., Jordan,T., Beremdse,F., Feemeu,J., Kudeyarov,V., Murdoch,P., Zhu,Z.")
	(bibtex:hasAuthor "Howarth,R.W., Billen, G., Swaney,D., Townsend, A., Jaworski,N., Lajtha,K., Downing, J.A., Elmgren,R., Caraco,N., Jordan,T., Beremdse,F., Feemeu,J., Kudeyarov,V., Murdoch,P., Zhu,Z.")
	(bibtex:hasTitle "Regional nitrogen budgets and riverine N and P fluxes of the drainages to the North Atlantic Ocean: Natural and human influences.")
	(bibtex:hasYear 1996)
	(bibtex:hasNumber 35)
	(bibtex:hasPages "75")
	(bibtex:hasAbstract "We present estimates of total nitrogen and total phosphorus fluxes in rivers to the North Atlantic Ocean from 14 regions in North America, South America, Europe, and Africa which collectively comprise the drainage basins to the North Atlantic. The Amazon basin dominates the overall phosphorus flux and has the highest phosphorus flux per area. The total nitrogen flux from the Amazon is also large, contributing 3.3 Tg yr(-1) out of a total for the entire North Atlantic region of 13.1 Tg yr(-1). On a per area basis, however, the largest nitrogen fluxes are found in the highly disturbed watersheds around the North Sea, in northwestern Europe, and in the northeastern U.S., all of which have riverine nitrogen fluxes greater than 1,000 kg N km(-2) yr(-1). Non-point sources of nitrogen dominate riverine fluxes to the coast in all regions. River fluxes of total nitrogen from the temperate regions of the North Atlantic basin are correlated with population density, as has been observed previously for fluxes of nitrate in the world's major rivers. However, more striking is a strong linear correlation between river fluxes of total nitrogen and the sum of anthropogenically-derived nitrogen inputs to the temperate regions (fertilizer application, human-induced increases in atmospheric deposition of oxidized forms of nitrogen, fixation by leguminous crops, and the import/export of nitrogen in agricultural products). On average, regional nitrogen fluxes in rivers are only 25% of these anthropogenically derived nitrogen inputs. Denitrification in wetlands and aquatic ecosystems is probably the dominant sink, with storage in forests perhaps also of importance. Storage of nitrogen in groundwater, although of importance in some localities, is a very small sink for nitrogen inputs in all regions. Agricultural sources of nitrogen dominate inputs in many regions, particularly the Mississippi basin and the North Sea drainages. Deposition of oxidized nitrogen, primarily of industrial origin, is the major control over river nitrogen export in some regions such as the northeastern U.S. Using data from relatively pristine areas as an index of change, we estimate that riverine nitrogen fluxes in many of the temperate regions have increased from pre-industrial times by 2 to 20 fold, although some regions such as northern Canada are relatively unchanged. Fluxes from the most disturbed region, the North Sea drainages, have increased by 6 to 20 fold. Fluxes from the Amazon basin are also at least 2 to 5 fold greater than estimated fluxes from undisturbed temperate-zone regions, despite low population density and low inputs of anthropogenic nitrogen to the region. This suggests that natural riverine nitrogen fluxes in the tropics may be significantly greater than in the temperate zone. However, deforestation may be contributing to the tropical fluxes. In either case, projected increases in fertilizer use and atmospheric deposition in the coming decades are likely to cause dramatic increases in nitrogen loading to many tropical river systems.")
	(bibtex:hasKeywords "systems river tropical many loading nitrogen increases dramatic cause likely decades coming deposition atmospheric fertilizer projected case either fluxes contributing deforestation However zone temperate than greater significantly tropics riverine natural that suggests This region anthropogenic inputs density population despite regions undisturbed from estimated fold least also basin Amazon Fluxes increased have drainages North disturbed most northeastern such some export over control major origin industrial primarily oxidized Deposition Mississippi particularly dominate sources Agricultural sink small localities importance although groundwater Storage perhaps forests storage with dominant probably ecosystems aquatic wetlands Denitrification derived these only rivers regional average agricultural crops leguminous fixation forms application total between correlation linear strong striking more world's nitrate previously observed been correlated Atlantic River coast which Europe northwestern around watersheds highly found largest however basis area entire large flux phosphorus highest overall dominates basins drainage comprise collectively Africa America South Ocean estimates present influences human Natural budgets Regional Murdoch Jordan Downing Jaworski Townsend Swaney Billen Howarth Biogeochemistry")
	) :id esdbr-36
 (modelling/object 'bibtex:Article
	"Ihori et al., Soil Science Society of American Journal, 1995."
	"Effects of cultivation and abandonment on soil organic matter in northeastern Colorado."
	(bibtex:hasJournal "Ihori,T.,Burke,I.C., Lauenroth, W.K., Coffin,D.P.")
	(bibtex:hasAuthor "Ihori,T.,Burke,I.C., Lauenroth, W.K., Coffin,D.P.")
	(bibtex:hasTitle "Effects of cultivation and abandonment on soil organic matter in northeastern Colorado.")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 59)
	(bibtex:hasPages "1112-1119")
	(bibtex:hasAbstract "The influence of climate and soil texture on soil organic matter losses due to cultivation are rarely addressed due to lack of appropriately paired sites. In addition, soil organic matter recovery on previously cultivated fields is not well understood. In this study, we identified seven sites that have native and abandoned fields and five sites that have native, abandoned, and cultivated fields. All sites are in the northeastern Colorado shortgrass steppe, crossing a precipitation range from 320 to 370 mm, and have sand contents from 36 to 67%. Historical cultivation reduced soil C and N across this region by between 16 and 42%. Although variation in native soil C and N at these sites correlates with climate and soil texture, variation among sites in soil losses due to cultivation is not explained by these variables. We used a statistical model and a simulation model to estimate patterns of soil loss across sites; neither model predicted variation among sites adequately (P > 0.05). We suggest that local-scale variability in organic matter losses due to cultivation are strongly dependent on management practices. With a simulation model and the data from native, abandoned, and cultivated fields, we estimated that 25 to 120 g C m(-2) have been recovered on the abandoned fields during the past 50 yr. Such rates of recovery are small compared with loss rates due to cultivation. Rates of soil organic matter recovery are constrained by natural pedogenic processes, which cannot reverse disturbance processes at a comparable rate in this semiarid environment.")
	(bibtex:hasKeywords "environment semiarid this rate comparable processes disturbance reverse cannot which pedogenic natural constrained recovery matter organic soil Rates cultivation rates loss with compared small Such past during fields abandoned recovered that estimated cultivated native from data model simulation With practices management dependent strongly losses variability suggest adequately sites among variation predicted neither across patterns estimate statistical used variables these explained texture climate correlates Although between region reduced Historical contents sand have range precipitation crossing steppe Colorado northeastern five seven identified study understood well previously addition paired appropriately lack addressed rarely influence abandonment Effects Coffin Lauenroth Burke Journal American Society Science Soil")
	) :id esdbr-37
 (modelling/object 'bibtex:Article
	"Jones et al., Journal of Soil and Water Conservation, 1985."
	"Runoff, soil and nutrient losses from rangeland and dry-farmed cropland in the southern high plain."
	(bibtex:hasJournal "Jones,O.R.,Eck,H.V., Smith, S.J., Coleman,G.A., Hauser,V.L.")
	(bibtex:hasAuthor "Jones,O.R.,Eck,H.V., Smith, S.J., Coleman,G.A., Hauser,V.L.")
	(bibtex:hasTitle "Runoff, soil and nutrient losses from rangeland and dry-farmed cropland in the southern high plain.")
	(bibtex:hasYear 1985)
	(bibtex:hasNumber 1)
	(bibtex:hasPages "161-164")
	(bibtex:hasKeywords "plain high southern cropland rangeland from losses nutrient soil Runoff Hauser Coleman Smith Jones Conservation Water Soil Journal")
	) :id esdbr-38
 (modelling/object 'bibtex:Article
	"Lant et al., Environment and Planning, 1990."
	"Greenbelts in the cornbelt: riparian wetlands, instrinsic values and market failure."
	(bibtex:hasJournal "Lant,C.L., Roberts, R.S.")
	(bibtex:hasAuthor "Lant,C.L., Roberts, R.S.")
	(bibtex:hasTitle "Greenbelts in the cornbelt: riparian wetlands, instrinsic values and market failure.")
	(bibtex:hasYear 1990)
	(bibtex:hasNumber 22)
	(bibtex:hasPages "1375-1388")
	(bibtex:hasKeywords "failure market values wetlands riparian Roberts Lant Planning Environment")
	) :id esdbr-39
 (modelling/object 'bibtex:Article
	"Lynne et al., Journal of Enviromental Economics and Management, 1981."
	"Economic valuation of marsh areas to marine production processes."
	(bibtex:hasJournal "Lynne, G.D., Conroy,P.D., Pochasta,F.")
	(bibtex:hasAuthor "Lynne, G.D., Conroy,P.D., Pochasta,F.")
	(bibtex:hasTitle "Economic valuation of marsh areas to marine production processes.")
	(bibtex:hasYear 1981)
	(bibtex:hasNumber 8)
	(bibtex:hasPages "175-186")
	(bibtex:hasKeywords "processes production marine areas marsh valuation Economic Conroy Lynne Management Economics Journal")
	) :id esdbr-40
)	