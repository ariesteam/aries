(ns core.tasks
  (:refer tl :only (with-kbox kbox defobject)))

(with-kbox 
	
	(kbox esav_sources "postgres://postgres:rnbh304@localhost:5432/esav_sources" 
				:protocol "pg" 
				:metadata (
					:keywords         thinklab-core:Text
	        :title            thinklab-core:Text
	        :authors          thinklab-core:Text
	        :abstract         thinklab-core:Text)
				:sql.use.pooling "false" 
				:sql.log.queries "true")
				
		:metadata-generator {
							:keywords #(.get % "bibtex:hasKeywords")																										
							:title    #(.get % "bibtex:hasTitle")																										
							:authors  #(.get % "bibtex:hasAuthor")																										
							:abstract #(.get % "bibtex:hasAbstract")}

 (defobject 'bibtex:Article
	"Mattews, Journal of Climate and Applied Meteorology, 1983."
	"Global vegetation and land-use: new high-resolution data bases for climatic studies."
	(bibtex:hasJournal "Mattews, E.")
	(bibtex:hasAuthor "Mattews, E.")
	(bibtex:hasTitle "Global vegetation and land-use: new high-resolution data bases for climatic studies.")
	(bibtex:hasYear 1983)
	(bibtex:hasNumber 22)
	(bibtex:hasPages "474-487")
	(bibtex:hasKeywords "studies climatic bases data resolution high land vegetation Global Mattews Meteorology Applied Climate Journal")
	) :id esdbr-41
 (defobject 'bibtex:Article
	"Moister et al., Nature, 1991."
	"Methane and nitrous oxide fluxes in native, fertilized and cultivated grassland."
	(bibtex:hasJournal "Moister, A.D., Schimel,d., Valentine,D., Bronson,K., Parton,W.")
	(bibtex:hasAuthor "Moister, A.D., Schimel,d., Valentine,D., Bronson,K., Parton,W.")
	(bibtex:hasTitle "Methane and nitrous oxide fluxes in native, fertilized and cultivated grassland.")
	(bibtex:hasYear 1991)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "330-332")
	(bibtex:hasKeywords "grassland cultivated fertilized native fluxes oxide nitrous Methane Parton Bronson Valentine Schimel Moister Nature")
	) :id esdbr-42
 (defobject 'bibtex:Article
	"Nixon et al., Biogeochemistry, 1996."
	"The fate of nitrogen and phosphorus at the land-sea margin of the North Atlantic Ocean."
	(bibtex:hasJournal "Nixon,S.W., Ammerman,J.W., Atkinson,L.P., Berounsky,V.M., Billen,G., Boicourt,W.C., Boynton,W.R., Church,T.M., DiToro,D.M., Elmgren,r., Garber,J.H., Giblin,A.E., Jahnke, R.A.,Owens, N.J.P., Pilson,M.")
	(bibtex:hasAuthor "Nixon,S.W., Ammerman,J.W., Atkinson,L.P., Berounsky,V.M., Billen,G., Boicourt,W.C., Boynton,W.R., Church,T.M., DiToro,D.M., Elmgren,r., Garber,J.H., Giblin,A.E., Jahnke, R.A.,Owens, N.J.P., Pilson,M.")
	(bibtex:hasTitle "The fate of nitrogen and phosphorus at the land-sea margin of the North Atlantic Ocean.")
	(bibtex:hasYear 1996)
	(bibtex:hasNumber 35)
	(bibtex:hasPages "141")
	(bibtex:hasAbstract "Five large rivers that discharge on the western North Atlantic continental shelf carry about 45% of the nitrogen (N) and 70% of the phosphorus (P) that others estimate to be the total flux of these elements from the entire North Atlantic watershed, including North, Central and South America, Europe, and Northwest Africa. We estimate that 61 . 10(9) moles y(-1) of N and 20 . 10(9) moles y(-1) of P from the large rivers are buried with sediments in their deltas, and that an equal amount of N and P from the large rivers is lost to the shelf through burial of river sediments that are deposited directly on the continental slope. The effective transport of active N and P from land to the shelf through the very large rivers is thus reduced to 292 . 10(9) moles y(-1) of N and 13 . 10(9) moles y(-1) of P. The remaining riverine fluxes from land must pass through estuaries. An analysis of annual total N and total P budgets for various estuaries around the North Atlantic revealed that the net fractional transport of these nutrients through estuaries to the continental shelf is inversely correlated with the log mean residence time of water in the system. This is consistent with numerous observations of nutrient retention and loss in temperate lakes. Denitrification is the major process responsible for removing N in most estuaries, and the fraction of total N input that is denitrified appears to be directly proportional to the log mean water residence time. In general, we estimate that estuarine processes retain and remove 30-65% of the total N and 10-55% of the total P that would otherwise pass into the coastal ocean. The resulting transport through estuaries to the shelf amounts to 172-335 . 10(9) moles y(-1) of N and 11-19 . 10(9) moles y(-1) of P. These values are similar to the effective contribution from the large rivers that discharge directly on the shelf. For the North Atlantic shelf as a whole, N fluxes from major rivers and estuaries exceed atmospheric deposition by a factor of 3.5-4.7, but this varies widely among regions of the shelf. For example, on the U.S. Atlantic shelf and on the northwest European shelf, atmospheric deposition of N may exceed estuarine exports. Denitrification in shelf sediments exceeds the combined N input from land and atmosphere by a factor of 1.4-2.2. This deficit must be met by a flux of N from the deeper ocean. Burial of organic matter fixed on the shelf removes only a small fraction of the total N and P input (2-12% of N from land and atmosphere; 1-17% of P), but it may be a significant loss for P in the North Sea and some other regions. The removal of N and P in fisheries landings is very small. The gross exchange of N and P between the shelf and the open ocean is much lager than inputs from land and, for the North Atlantic shelf as a whole, it may be much larger than the N and P removed through denitrification, burial, and fisheries. Overall, the North Atlantic continental shelf appears to remove some 700-950 . 10(9) moles of N each year from the deep ocean and to transport somewhere between 18 and 30 . 10(9) moles of P to the open sea. If the N and P associated with riverine sediments deposited on the continental slope are included in the total balance, the net flux of N to the shelf is reduced by 60 . 10(9) moles y(-1) and the P flux to the ocean is increased by 20 . 10(9) moles y(-1). These conclusions are quite tentative. however, because of large uncertainties in our estimates of some important terms in the shelf mass balance.")
	(bibtex:hasKeywords "balance mass shelf terms important some estimates uncertainties large because however tentative quite conclusions These increased ocean flux moles reduced total included slope continental deposited sediments riverine with associated open between somewhere transport remove appears Atlantic North Overall small very landings fisheries removal deeper from must deficit factor atmosphere land input combined exceeds Denitrification exports estuarine exceed deposition atmospheric European northwest example regions among widely varies estuaries rivers major fluxes whole directly discharge that contribution effective similar values amounts through resulting coastal into pass otherwise would retain processes estimate general lakes temperate loss retention nutrient observations numerous consistent This system water time residence mean correlated inversely nutrients these fractional revealed around various budgets annual analysis remaining thus active river burial lost amount equal deltas their buried Africa Northwest Europe America South Central including watershed entire elements others phosphorus nitrogen about carry western Five Ocean margin fate Pilson Owens Jahnke Giblin Garber Church Boynton Boicourt Billen Atkinson Ammerman Nixon Biogeochemistry")
	) :id esdbr-43
 (defobject 'bibtex:Article
	"Oesterheld et al., Nature, 1992."
	"Effect of animal husbandry on herbivore carrying capacity at a regional scale."
	(bibtex:hasJournal "Oesterheld,M., Sala,O.E., McNaughton, S.J.")
	(bibtex:hasAuthor "Oesterheld,M., Sala,O.E., McNaughton, S.J.")
	(bibtex:hasTitle "Effect of animal husbandry on herbivore carrying capacity at a regional scale.")
	(bibtex:hasYear 1992)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "234-236")
	(bibtex:hasAbstract "ALL significant properties of the herbivore trophic level, including biomass, consumption and productivity, are significantly correlated with primary productivity across a broad range of terrestrial ecosystems 1,2. Here we show that livestock biomass in South American agricultural ecosystems across a 25-fold gradient of primary productivity exhibited a relationship with a slope essentially identical to unmanaged ecosystems, but with a substantially greater y-intercept. Therefore the biomass of herbivores supported per unit of primary productivity is about an order of magnitude greater in agricultural than in natural ecosystems, for a given level of primary production. We also present evidence of an increase in livestock body size with primary productivity, a pattern previously characterized in natural ecosystems 3. To our knowledge this is the first quantitative documentation at a regional scale of the impact of animal husbandry practices, such as herding, stock selection and veterinary care, on the biomass and size-structure of livestock herds compared with native herbivores")
	(bibtex:hasKeywords "native with compared herds livestock biomass care veterinary selection stock herding such practices husbandry animal impact scale regional documentation quantitative first this knowledge ecosystems natural characterized previously pattern productivity primary size body increase evidence present also production level given than agricultural greater magnitude order about unit supported Therefore substantially unmanaged identical essentially slope relationship exhibited gradient across American South that show Here terrestrial range broad correlated significantly consumption including trophic herbivore properties significant capacity carrying Effect Sala our the and")
	) :id esdbr-44
 (defobject 'bibtex:Article
	"Pauly et al., Nature, 1995."
	"Primary production required to sustain global fishersies."
	(bibtex:hasJournal "Pauly,D., Christensen,V.")
	(bibtex:hasAuthor "Pauly,D., Christensen,V.")
	(bibtex:hasTitle "Primary production required to sustain global fishersies.")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "255-257")
	(bibtex:hasAbstract "THE mean of reported annual world fisheries catches for 1988-1991 (94.3 million t) was split into 39 species groups, to which fractional trophic levels, ranging from 1.0 (edible algae) to 4.2 (tunas), were assigned, based on 48 published trophic models, providing a global coverage of six major aquatic ecosystem types. The primary production required to sustain each group of species was then computed based on a mean energy transfer efficiency between trophic levels of 10%, a value that was re-estimated rather than assumed. The primary production required to sustain the reported catches, plus 27 million t of discarded bycatch, amounted to 8.0% of global aquatic primary production, nearly four times the previous estimate. By ecosystem type, the requirements were only 2% for open ocean systems, but ranged from 24 to 35% in fresh water, upwelling and shelf systems, justifying current concerns for sustainability and biodiversity.")
	(bibtex:hasKeywords "biodiversity sustainability concerns current justifying systems shelf water fresh from ranged ocean open only were requirements type ecosystem estimate previous times four nearly production primary aquatic global amounted discarded million plus catches reported sustain required assumed than rather that value levels trophic between efficiency transfer energy based computed then species group each types major coverage providing models published assigned ranging fractional which groups into split fisheries world annual mean Primary Christensen Pauly Nature")
	) :id esdbr-45
 (defobject 'bibtex:Article
	"Peters et al., Nature, 1989."
	"Valuation of an Amazonian Rain Forest"
	(bibtex:hasJournal "Peters,C.M., Genrty,A.H., Mendelsohn,R.O.")
	(bibtex:hasAuthor "Peters,C.M., Genrty,A.H., Mendelsohn,R.O.")
	(bibtex:hasTitle "Valuation of an Amazonian Rain Forest")
	(bibtex:hasYear 1989)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "655-656")
	(bibtex:hasKeywords "Forest Rain Valuation Mendelsohn Peters Nature")
	) :id esdbr-46
 (defobject 'bibtex:Article
	"Pielke et al., Ecological Applications, 1996."
	"Use of USGS-provided data to improve weather and climate simulations."
	(bibtex:hasJournal "Pielke,R.A., Lee,T.J., Copeland, J.H., Eastman,J.L., Ziegler,C.L., Finley,C.A.")
	(bibtex:hasAuthor "Pielke,R.A., Lee,T.J., Copeland, J.H., Eastman,J.L., Ziegler,C.L., Finley,C.A.")
	(bibtex:hasTitle "Use of USGS-provided data to improve weather and climate simulations.")
	(bibtex:hasYear 1996)
	(bibtex:hasNumber 7)
	(bibtex:hasPages "3")
	(bibtex:hasAbstract "This paper utilizes United States Geological Survey (USGS) data to investigate the influence of landscape structure on atmospheric circulations. The procedure to insert this data in the Regional Atmospheric Modeling System (RAMS) is described. Simulations are presented for a monthly simulation of summer weather in the United States, for case studies of cumulonimbus convection along a dryline in the Great Plains of the U.S. and over northern Georgia, and for pollutant dispersal in South Carolina. These results demonstrate the significant role that landscape, including its spatial heterogeneity, has on weather and climate. Environmental policy-makers need to consider this feedback to weather and climate, rather than just assuming the atmosphere is an external factor to such issues as ecosystem management and water resource management. This feedback between the atmosphere and the land surface needs to be considered on all spatial scales from the plot scale to the global scale. This includes studies being performed at the Long-Term Ecological Research (LTER) sites that have been established throughout the United States. This paper also demonstrates the value of the USGS data in weather and climate simulations")
	(bibtex:hasKeywords "simulations climate weather data USGS value demonstrates also paper This States United throughout established been have that sites Research Ecological performed being studies includes scale global plot from scales spatial considered needs surface land atmosphere between feedback management resource water ecosystem issues such factor external assuming just than rather this consider need Environmental heterogeneity including landscape role significant demonstrate results These Carolina South dispersal pollutant Georgia northern over Plains Great along convection cumulonimbus case summer simulation monthly presented Simulations described System Modeling Atmospheric Regional insert procedure circulations atmospheric structure influence investigate Survey Geological utilizes improve Finley Ziegler Eastman Copeland Applications the and")
	) :id esdbr-47
 (defobject 'bibtex:Article
	"Pimentel et al., Science, 1995."
	"Environmental and economic costs of soil erosion and conservation benefits."
	(bibtex:hasJournal "Pimentel,D., Harvey,C., Resosudarmo,P., Sinclair,K., Kurz,D., McNair,M., Crist,S., Sphpritz,P., Fitton,L., Saffouri,R., Blair,R.")
	(bibtex:hasAuthor "Pimentel,D., Harvey,C., Resosudarmo,P., Sinclair,K., Kurz,D., McNair,M., Crist,S., Sphpritz,P., Fitton,L., Saffouri,R., Blair,R.")
	(bibtex:hasTitle "Environmental and economic costs of soil erosion and conservation benefits.")
	(bibtex:hasYear 1995)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "1117-1123")
	(bibtex:hasAbstract "Soil erosion is a major environmental threat to the sustainability and productive capacity of agriculture. During the last 40 years, nearly one-third of the world's arable land has been lost by erosion and continues to be lost at a rate of more than 10 million hectares per year. With the addition of a quarter of a million people each day, the world population's food demand is increasing at a time when per capita food productivity is beginning to decline.")
	(bibtex:hasKeywords "decline beginning productivity food capita increasing demand world each people million quarter addition With year hectares than more rate lost continues erosion been land arable world's nearly years last During agriculture capacity productive sustainability threat environmental major Soil benefits conservation soil costs economic Environmental Blair Fitton Crist Kurz Sinclair Harvey Pimentel Science")
	) :id esdbr-48
 (defobject 'bibtex:Article
	"Pinedo-Vasques et al., Ecological Economics, 1992."
	"Economic returns from forest conversion in the Peruvian Amazon."
	(bibtex:hasJournal "Pinedo-Vasques,M., Jip,D.Z.")
	(bibtex:hasAuthor "Pinedo-Vasques,M., Jip,D.Z.")
	(bibtex:hasTitle "Economic returns from forest conversion in the Peruvian Amazon.")
	(bibtex:hasYear 1992)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "76-78")
	(bibtex:hasKeywords "Amazon Peruvian conversion forest from returns Economic Economics Ecological")
	) :id esdbr-49
 (defobject 'bibtex:Article
	"Pope et al., Journal of Environmental Management., 1990."
	"Value of wilderness designation in Utah."
	(bibtex:hasJournal "Pope,C.A., Jones,J.W.")
	(bibtex:hasAuthor "Pope,C.A., Jones,J.W.")
	(bibtex:hasTitle "Value of wilderness designation in Utah.")
	(bibtex:hasYear 1990)
	(bibtex:hasNumber 30)
	(bibtex:hasPages "157-174")
	(bibtex:hasKeywords "Utah designation wilderness Value Jones Pope Management Environmental Journal")
	) :id esdbr-50
 (defobject 'bibtex:Article
	"Robinson et al., American Bee Journal, 1989."
	"The value of honey bees as pollinators of US crops."
	(bibtex:hasJournal "Robinson,W.E., Nowogrodzki,R., Morse,R.A.")
	(bibtex:hasAuthor "Robinson,W.E., Nowogrodzki,R., Morse,R.A.")
	(bibtex:hasTitle "The value of honey bees as pollinators of US crops.")
	(bibtex:hasYear 1989)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "477-487")
	(bibtex:hasKeywords "crops bees honey value Morse Robinson Journal American")
	) :id esdbr-51
 (defobject 'bibtex:Article
	"Ryther, Science, 1969."
	"Photosynthesis and fish production in the sea."
	(bibtex:hasJournal "Ryther,J.H.")
	(bibtex:hasAuthor "Ryther,J.H.")
	(bibtex:hasTitle "Photosynthesis and fish production in the sea.")
	(bibtex:hasYear 1969)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "72-76")
	(bibtex:hasKeywords "production fish Photosynthesis Ryther Science")
	) :id esdbr-52
 (defobject 'bibtex:Article
	"Sala et al., Ecology, 1988."
	"Primary production of the central grassland region o f the United States."
	(bibtex:hasJournal "Sala,O.E., Parton,W.J., Joyce,L.A., Lauenroth,W.K.")
	(bibtex:hasAuthor "Sala,O.E., Parton,W.J., Joyce,L.A., Lauenroth,W.K.")
	(bibtex:hasTitle "Primary production of the central grassland region o f the United States.")
	(bibtex:hasYear 1988)
	(bibtex:hasNumber 69)
	(bibtex:hasPages "40-45")
	(bibtex:hasKeywords "States United region grassland central production Primary Lauenroth Joyce Parton Sala Ecology")
	) :id esdbr-53
 (defobject 'bibtex:Article
	"Southwick et al., Journal of Economic Entomology, 1992."
	"Estimating the economic value of honey bees (Hymenoptera:Apidae) as agricultural pollinators in the United States."
	(bibtex:hasJournal "Southwick, E.E., Southwich,Lawrence,Jr.")
	(bibtex:hasAuthor "Southwick, E.E., Southwich,Lawrence,Jr.")
	(bibtex:hasTitle "Estimating the economic value of honey bees (Hymenoptera:Apidae) as agricultural pollinators in the United States.")
	(bibtex:hasYear 1992)
	(bibtex:hasNumber 85)
	(bibtex:hasPages "622-633")
	(bibtex:hasAbstract "The economic gains due to honey bee (Apis mellifera L.) agricultural pollination are evaluated. The method of analysis focuses on the gains to consumers through lower prices for crops that are benefited by honey bees. Economic demand functions for the major agricultural crops that are pollinated by bees are estimated. The amounts by which the yields of pollinated crops are increased are estimated from a variety of sources. In the final step, the surplus realized by consumers of these crops that would be lost if honey bees were depleted is determined. The annual social gains are estimated to range between $1.6 and $5.7 billion.")
	(bibtex:hasKeywords "billion between range estimated gains social annual determined depleted were bees honey lost would that crops these consumers realized surplus step final sources variety from increased pollinated yields which amounts agricultural major functions demand Economic benefited prices lower through focuses analysis method evaluated pollination economic States United value Estimating Lawrence Southwick Entomology Journal")
	) :id esdbr-54
 (defobject 'bibtex:Article
	"Spurgeon et al., Marine Pollution Bulletin, 1992."
	"The economic valution of coral reefs."
	(bibtex:hasJournal "Spurgeon,J.P.G.")
	(bibtex:hasAuthor "Spurgeon,J.P.G.")
	(bibtex:hasTitle "The economic valution of coral reefs.")
	(bibtex:hasYear 1992)
	(bibtex:hasNumber 24)
	(bibtex:hasPages "529-536")
	(bibtex:hasKeywords "reefs coral economic Spurgeon Bulletin Pollution Marine")
	) :id esdbr-55
 (defobject 'bibtex:Article
	"Stroud et al., Sport Fishing Inst. Bull., 1970."
	"Estuary Values."
	(bibtex:hasJournal "Stroud, R.H.,")
	(bibtex:hasAuthor "Stroud, R.H.,")
	(bibtex:hasTitle "Estuary Values.")
	(bibtex:hasYear 1970)
	(bibtex:hasNumber 127)
	(bibtex:hasPages "7-8")
	(bibtex:hasKeywords "Values Estuary Stroud Fishing Sport")
	) :id esdbr-56
 (defobject 'bibtex:Article
	"Thibodeau et al., Journal of Environmental Managmement, 1981."
	"An Economic analysis of wetland protection"
	(bibtex:hasJournal "Thibodeau, F.R., Ostro,B.D.")
	(bibtex:hasAuthor "Thibodeau, F.R., Ostro,B.D.")
	(bibtex:hasTitle "An Economic analysis of wetland protection")
	(bibtex:hasYear 1981)
	(bibtex:hasNumber 12)
	(bibtex:hasPages "19-30")
	(bibtex:hasKeywords "protection wetland analysis Economic Thibodeau Environmental Journal")
	) :id esdbr-57
 (defobject 'bibtex:Article
	"Tobias et al., Ambio, 1991."
	"Valuing ecotourism in a tropical rainforest."
	(bibtex:hasJournal "Tobias, A.H., Wood, S.E.")
	(bibtex:hasAuthor "Tobias, A.H., Wood, S.E.")
	(bibtex:hasTitle "Valuing ecotourism in a tropical rainforest.")
	(bibtex:hasYear 1991)
	(bibtex:hasNumber 20)
	(bibtex:hasPages "91-93")
	(bibtex:hasKeywords "rainforest tropical Valuing Wood Tobias")
	) :id esdbr-58
 (defobject 'bibtex:Incollection
	"Fankhauser et al., The economics of climate change. Proceedings of an OECD/IEA Conference., 1994."
	"The social costs of greenhouse gas emissions."
	(bibtex:hasBooktitle "Fankhauser,S., Pearce,D.W.")
	(bibtex:hasAuthor "Fankhauser,S., Pearce,D.W.")
	(bibtex:hasTitle "The social costs of greenhouse gas emissions.")
	(bibtex:hasYear 1994)
	(bibtex:hasPages "71-86")
	(bibtex:hasKeywords "emissions greenhouse costs social Pearce Fankhauser Conference Proceedings change climate economics")
	) :id esdbr-60
 (defobject 'bibtex:Incollection
	"Brown et al., The economics of project appraisal and the environment., 1994."
	"The economic value of non-marked benefits of tropical forests:carbon storage."
	(bibtex:hasBooktitle "Brown,K., Pearce, D.W.")
	(bibtex:hasAuthor "Brown,K., Pearce, D.W.")
	(bibtex:hasTitle "The economic value of non-marked benefits of tropical forests:carbon storage.")
	(bibtex:hasYear 1994)
	(bibtex:hasPages "102-123")
	(bibtex:hasKeywords "storage carbon forests tropical benefits value economic Pearce Brown environment appraisal project economics")
	) :id esdbr-61
 (defobject 'bibtex:Incollection
	"Cowling et al., Nature's Services., 1997."
	"Services supplied by South African fynbos ecosystems."
	(bibtex:hasBooktitle "Cowling,R.M., Costanza,R., Higgins,S.I.")
	(bibtex:hasAuthor "Cowling,R.M., Costanza,R., Higgins,S.I.")
	(bibtex:hasTitle "Services supplied by South African fynbos ecosystems.")
	(bibtex:hasYear 1997)
	(bibtex:hasKeywords "ecosystems African South supplied Services Higgins Costanza Cowling Nature's")
	) :id esdbr-62
 (defobject 'bibtex:Incollection
	"de Groot, Functions of nature: evaluation of nature in environmental planning, management and decision making., 1992."
	"Funtions and values of the Dutch Wadden Sea"
	(bibtex:hasBooktitle "de Groot,R.S.")
	(bibtex:hasAuthor "de Groot,R.S.")
	(bibtex:hasTitle "Funtions and values of the Dutch Wadden Sea")
	(bibtex:hasYear 1992)
	(bibtex:hasPages "197-217")
	(bibtex:hasKeywords "Wadden Dutch values Groot making decision management planning environmental nature evaluation Functions Sea")
	) :id esdbr-63
 (defobject 'bibtex:Incollection
	"Foster, Wetland functions and values: the state of our understanding., 1978."
	"Measuring the social value of wetland benefits."
	(bibtex:hasBooktitle "Foster,J.H.")
	(bibtex:hasAuthor "Foster,J.H.")
	(bibtex:hasTitle "Measuring the social value of wetland benefits.")
	(bibtex:hasYear 1978)
	(bibtex:hasPages "84-92")
	(bibtex:hasKeywords "benefits wetland value social Measuring Foster understanding state values functions Wetland")
	) :id esdbr-64
 (defobject 'bibtex:Incollection
	"Holland et al., The chemistry of the atmosphere and oceans., 1978."
	"Holland et al., The chemistry of the atmosphere and oceans., 1978."
	(bibtex:hasBooktitle "Holland, H. D.")
	(bibtex:hasAuthor "Holland, H. D.")
	(bibtex:hasTitle "The chemistry of the atmosphere and oceans.")
	(bibtex:hasYear 1978)
	(bibtex:hasPages "351")
	(bibtex:hasKeywords "oceans atmosphere chemistry Holland")
	) :id esdbr-65
 (defobject 'bibtex:Incollection
	"Kramer et al., Managing the world's forests: looking for balance between conservation and development., 1992."
	"Forest valuation."
	(bibtex:hasBooktitle "Kramer,R.A.,Healy,R., Mendelsohn,R.")
	(bibtex:hasAuthor "Kramer,R.A.,Healy,R., Mendelsohn,R.")
	(bibtex:hasTitle "Forest valuation.")
	(bibtex:hasYear 1992)
	(bibtex:hasKeywords "valuation Forest Mendelsohn Healy Kramer development conservation between balance looking forests world's Managing")
	) :id esdbr-66
 (defobject 'bibtex:Techreport
	"Lampietti et al., Environmental Economics., 1995."
	"Human influence on biodiversity."
	(bibtex:hasJournal "Lampietti,J.A., Dixon,J.A.")
	(bibtex:hasAuthor "Lampietti,J.A., Dixon,J.A.")
	(bibtex:hasTitle "Human influence on biodiversity.")
	(bibtex:hasYear 1995)
	(bibtex:hasKeywords "biodiversity influence Human Dixon Economics Environmental")
	) :id esdbr-67
 (defobject 'bibtex:Incollection
	"Mori, Sustainable harvesting and marketing of rain forest products., 1992."
	"The Brazil nut industry: past,present and future."
	(bibtex:hasBooktitle "Mori,S.A.")
	(bibtex:hasAuthor "Mori,S.A.")
	(bibtex:hasTitle "The Brazil nut industry: past,present and future.")
	(bibtex:hasYear 1992)
	(bibtex:hasKeywords "future present past industry Brazil Mori products forest rain marketing harvesting Sustainable")
	) :id esdbr-68
 (defobject 'bibtex:Incollection
	"Pearce, Economic policy toward the environment., 1991."
	"An economic approach to saving the tropical forests."
	(bibtex:hasBooktitle "Pearce,D.")
	(bibtex:hasAuthor "Pearce,D.")
	(bibtex:hasTitle "An economic approach to saving the tropical forests.")
	(bibtex:hasYear 1991)
	(bibtex:hasPages "239-262")
	(bibtex:hasKeywords "forests tropical saving approach economic Pearce environment toward policy Economic")
	) :id esdbr-69
 (defobject 'bibtex:Incollection
	"Pimental, Techniques for reducing pesticides-environmental an economic benefits., 0."
	"Pest management in agriculture."
	(bibtex:hasBooktitle "Pimental,D.")
	(bibtex:hasAuthor "Pimental,D.")
	(bibtex:hasTitle "Pest management in agriculture.")
	(bibtex:hasYear 1990)
	(bibtex:hasKeywords "agriculture management Pest Pimental benefits economic reducing Techniques")
	) :id esdbr-70
 (defobject 'bibtex:Incollection
	"Postel et al., Nature's services, 1997."
	"Freshwater ecosystem services."
	(bibtex:hasBooktitle "Postel,S., Carpenter,S.")
	(bibtex:hasAuthor "Postel,S., Carpenter,S.")
	(bibtex:hasTitle "Freshwater ecosystem services.")
	(bibtex:hasYear 1997)
	(bibtex:hasPages "195-214")
	(bibtex:hasKeywords "services ecosystem Freshwater Carpenter Postel Nature's")
	) :id esdbr-71
 (defobject 'bibtex:Incollection
	"Sala et al., Ecosystem services., 1996."
	"Ecosystem services in grasslands."
	(bibtex:hasBooktitle "Sala,O.E., Paruelo,J.M.")
	(bibtex:hasAuthor "Sala,O.E., Paruelo,J.M.")
	(bibtex:hasTitle "Ecosystem services in grasslands.")
	(bibtex:hasYear 1996)
	(bibtex:hasKeywords "grasslands services Ecosystem Sala")
	) :id esdbr-72
 (defobject 'bibtex:Incollection
	"Schlesinger, Biogeochemistry: an analysis of global change., 1991."
	"The Sea."
	(bibtex:hasBooktitle "Schlesinger,W.H.")
	(bibtex:hasAuthor "Schlesinger,W.H.")
	(bibtex:hasTitle "The Sea.")
	(bibtex:hasYear 1991)
	(bibtex:hasPages "254-293")
	(bibtex:hasKeywords "Schlesinger change global analysis Biogeochemistry")
	) :id esdbr-73
 (defobject 'bibtex:Incollection
	"Shiklomanov, Water in crisis: a guide to the World's fresh water resources., 1993."
	"World fresh water resources."
	(bibtex:hasBooktitle "Shiklomanov,L.A.")
	(bibtex:hasAuthor "Shiklomanov,L.A.")
	(bibtex:hasTitle "World fresh water resources.")
	(bibtex:hasYear 1993)
	(bibtex:hasPages "13-24")
	(bibtex:hasKeywords "resources water fresh World World's guide crisis Water")
	) :id esdbr-74
 (defobject 'bibtex:Incollection
	"Schlesinger, Biogeochemistry, 1991."
	"The Global Carbon Cycle"
	(bibtex:hasBooktitle "Schlesinger,W.H.")
	(bibtex:hasAuthor "Schlesinger,W.H.")
	(bibtex:hasTitle "The Global Carbon Cycle")
	(bibtex:hasYear 1991)
	(bibtex:hasPages "308-321")
	(bibtex:hasKeywords "Cycle Carbon Global Schlesinger Biogeochemistry The")
	) :id esdbr-75
 (defobject 'bibtex:Techreport
	"Adger et al., Global Environmental Change Series, 1994."
	"Towards estimating total economic value of forest in Mexico."
	(bibtex:hasJournal "Adger,N., Brown,K., Cerigni,R., Moran,D.")
	(bibtex:hasAuthor "Adger,N., Brown,K., Cerigni,R., Moran,D.")
	(bibtex:hasTitle "Towards estimating total economic value of forest in Mexico.")
	(bibtex:hasYear 1994)
	(bibtex:hasKeywords "Mexico forest value economic total estimating Towards Moran Brown Adger Series Change Environmental Global")
	) :id esdbr-77
 (defobject 'bibtex:Techreport
	"Lampietti et al., Environmental Economics, 1995."
	"To see the forest for the Trees: a guide to non-timber forest benefits."
	(bibtex:hasBooktitle "Lampietti,J.A., Dixon,J.A.")
	(bibtex:hasAuthor "Lampietti,J.A., Dixon,J.A.")
	(bibtex:hasTitle "To see the forest for the Trees: a guide to non-timber forest benefits.")
	(bibtex:hasYear 1995)
	(bibtex:hasKeywords "benefits forest guide Trees Dixon Economics Environmental")
	) :id esdbr-78
 (defobject 'bibtex:Techreport
	"Adger et al., Global Environmental Change Series, 1994."
	"Towards estimating total economic valueof forests in Mexico"
	(bibtex:hasBooktitle "Adger,N., Brown,K.,Cervigni,R.,Moran,D.")
	(bibtex:hasAuthor "Adger,N., Brown,K.,Cervigni,R.,Moran,D.")
	(bibtex:hasTitle "Towards estimating total economic valueof forests in Mexico")
	(bibtex:hasYear 1994)
	(bibtex:hasPages "1-41")
	(bibtex:hasKeywords "Mexico forests economic total estimating Towards Moran Brown Adger Series Change Environmental Global")
	) :id esdbr-79
 (defobject 'bibtex:Article
	"Schroth et al., Agriculture Ecosystems and Environment, 1999."
	"Subsoil accumulation of mineral nitrogen under polyculture and monoculture plantations, fallow and primary forest in a ferralitic Amazonia upland soil"
	(bibtex:hasJournal "Schroth, Gotz; Ferreira Da Silva, Luciana; Seixas, Rosangela; Teixeira, Wenceslau Geraldes; Macaeda, Jeferson L.V.; Zech, Wolfgang")
	(bibtex:hasAuthor "Schroth, Gotz; Ferreira Da Silva, Luciana; Seixas, Rosangela; Teixeira, Wenceslau Geraldes; Macaeda, Jeferson L.V.; Zech, Wolfgang")
	(bibtex:hasTitle "Subsoil accumulation of mineral nitrogen under polyculture and monoculture plantations, fallow and primary forest in a ferralitic Amazonia upland soil")
	(bibtex:hasYear 1999)
	(bibtex:hasNumber 75)
	(bibtex:hasPages "109-120")
	(bibtex:hasAbstract "Central Amazonia is characterized by high and intensive rainfall and permeable soils. When rainforests are cleared for agricultural use, the efficient nutrient recycling mechanisms of the forests are disrupted and the nutrient availability in the topsoil is increased by fertilization, thereby increasing the potential for nutrient leaching. In this study, the distribution of mineral N in the upper two meters of a ferralitic upland soil was evaluated as an indicator for nutrient leaching and for the potential contribution of the subsoil to crop nutrition. A perennial polyculture system with four tree crops and a leguminous cover crop at two fertilization levels was compared with a monoculture plantation of peach palm (Bactris gasipaes), spontaneous fallow and primary rainforest. Mineral N accumulated principally as nitrate in the subsoil under all agricultural crops and also under the primary forest, although to a lesser extent. Within the polyculture system, there were significant differences in N accumulation between the tree crop species, and for one of the species (Theobroma grandiflorum) also between fertilization levels. The principal sources of subsoil N were mineral fertilizer and presumably N from the mineralization of leguminous biomass and soil organic matter. The N losses from the agricultural systems and the absence of yield responses of the tree crops to N fertilization indicated that agricultural production was not limited by N at this site, or that N was too rapidly leached to be taken up efficiently by the crops. None of the tree crop species seemed to be efficient in capturing leached N, Strategies are discussed for reducing N losses from agricultural systems with perennial crops, including the development of site- and species-specific fertilizer recommendations, closer tree spacing, and the encouragement of lateral and vertical tree root development")
	(bibtex:hasKeywords "development root tree vertical lateral encouragement spacing closer recommendations fertilizer including crops perennial with systems agricultural from losses reducing discussed Strategies leached capturing efficient seemed species crop None efficiently taken rapidly that site this limited production indicated fertilization responses yield absence matter organic soil biomass leguminous mineralization presumably mineral were subsoil sources principal levels between also accumulation differences significant there system Within extent lesser although forest primary under nitrate principally accumulated Mineral rainforest fallow spontaneous palm peach plantation monoculture compared cover four nutrition contribution potential leaching nutrient indicator evaluated upland meters upper distribution study increasing thereby increased topsoil availability disrupted forests mechanisms recycling cleared When soils permeable rainfall intensive high characterized Amazonia Central plantations nitrogen Subsoil Wolfgang Zech Geraldes Teixeira Luciana Silva Ferreira Schroth Environment Ecosystems Agriculture the are for and")
	) :id esdbr-90
)	