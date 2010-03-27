(ns aries/escalculator
  (:refer-clojure)
  (:refer modelling :only (defmodel measurement classification categorization ranking identification bayesian))
  (:refer aries.valuation :only (es-calculator)))
  
(defmodel land-use 'esclass:HistoricalESLandcover
	"Just a reclass of the NLCD land use layer. Not really that good, but given what we use it for,
	 who cares."
	(classification (ranking 'nlcd:NLCDNumeric)
		#{81 82}	                       'esclass:AgriculturePasture
		#{41 42 43}                      'esclass:Forest
		#{71 72}	                       'esclass:GrasslandsShrublands
		#{23 24}	                       'esclass:Urban
		11	                             'esclass:LakesRiversPondsReservoirs
		#{90 91 92 93 94 95 96 97 98 99} 'esclass:Wetlands
		21	                             'esclass:Coastal
		31	                             'esclass:Rock
		#{12 73 74}	                     'esclass:Desert
		#{51 52}	                       'esclass:Tundra))
		
(defmodel esvalue 'esvaluation:HistoricESValue 
	"The stupid ES value calculator, Costanza/DeGroot/Wilson-style"
	(es-calculator 'esvaluation:HistoricESValue 
		:context (land-use)) )
		
		
  