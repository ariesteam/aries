(ns valuation.models.calculator
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defmodel measurement classification categorization ranking 
  												 numeric-coding binary-coding identification bayesian count))
  (:refer aries.valuation :only (es-calculator)))
  
(defmodel land-use 'esclass:HistoricalESLandcover
	"Just a reclass of common land use typologies. Not really that good, but given what we use it for,
	 who cares. Should work across all the US and Europe if the data are available."
	(classification (numeric-coding 'nlcd:NLCDNumeric)
		#{81 82}	                       'esclass:AgriculturePasture
		#{41 42 43}                      'esclass:Forest
		#{71 72}	                       'esclass:GrasslandsShrublands
		#{23 24}	                       'esclass:Urban
		11	                             'esclass:LakesRiversPondsReservoirs
		#{90 91 92 93 94 95 96 97 98 99} 'esclass:Wetlands
		21	                             'esclass:Coastal
		31	                             'esclass:Rock
		#{12 73 74}	                     'esclass:Desert
		#{51 52}	                       'esclass:Tundra)
	(classification (numeric-coding 'corine:CORINENumeric)
		[1 9 :inclusive]	               'esclass:Urban
		[12 22 :inclusive]	             'esclass:AgriculturePasture
		#{23 24 25}                      'esclass:Forest
		#{2}	                           'esclass:GrasslandsShrublands
		#{40 41}	                       'esclass:LakesRiversPondsReservoirs
		[35 39 :inclusive]               'esclass:Wetlands
		42	                             'esclass:Coastal
		31	                             'esclass:Rock
		#{30 31 32 33 34}	               'esclass:Tundra))

;; ------------------------------------------------------------------------------------------------
;; the crap RB, MW and countless others get paid money to produce.
;; ------------------------------------------------------------------------------------------------
		
(defmodel esvalue 'esvaluation:HistoricESValue 
	"ES value calculator, Costanza/DeGroot/Wilson-style"
	(es-calculator 'esvaluation:HistoricESValue 
		:context (land-use)) )

;; ------------------------------------------------------------------------------------------------
;; "enhanced" valuation models. Still crap, but these would be worth 10 years of publications if they
;; came from the great Gund researchers. All in a day's work for ARIES ecosystemserviceman.
;; ------------------------------------------------------------------------------------------------
		
(defmodel slope 'geophysics:DegreeSlope
	(measurement 'geophysics:DegreeSlope "\u00b0"))

(defmodel elevation 'geophysics:Altitude
	(measurement 'geophysics:Altitude "mm"))

(defmodel precipitation 'habitat:AnnualPrecipitation
	(measurement 'habitat:AnnualPrecipitation "mm"))

(defmodel population 'policytarget:PopulationDensity
  (count 'policytarget:PopulationDensity "/km^2")) 

(defmodel food-production-value 'esclass:FoodProduction 
	(es-calculator 'esclass:FoodProduction
    :influence (
                 (geophysics:DegreeSlope -0.4 :min 0 :max 17)
                 (policytarget:PopulationDensity -0.5 :min 100 :max 5000)
                 (habitat:AnnualPrecipitation 0.3 :min 600 :max 800)
                 (geophysics:Altitude -0.7 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population)))

(defmodel climate-regulation-value 'esclass:ClimateRegulation 
	(es-calculator 'esclass:ClimateRegulation
    :influence (
                 (geophysics:DegreeSlope -0.25 :min 4 :max 17)
                 (policytarget:PopulationDensity -0.25 :min 300 :max 1000)
                 (habitat:AnnualPrecipitation 0.4 :min 700 :max 800)
                 (geophysics:Altitude -0.1 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population)))

(defmodel water-supply-value 'esclass:WaterSupply 
	(es-calculator 'esclass:WaterSupply
    :influence (
                 (geophysics:DegreeSlope -0.15 :min 5 :max 10)
                 (policytarget:PopulationDensity -0.25 :min 1000 :max 5000)
                 (habitat:AnnualPrecipitation 0.2 :min 600 :max 800))
    :normalize false
		:context (land-use slope precipitation population)))
		  
(defmodel soil-formation-value 'esclass:SoilFormation 
	(es-calculator 'esclass:SoilFormation
    :influence (
                 (geophysics:DegreeSlope -0.5 :min 8 :max 15)
                 (policytarget:PopulationDensity -0.5 :min 200 :max 2000)
                 (habitat:AnnualPrecipitation -0.3 :min 600 :max 800))
    :normalize false    
		:context (land-use population precipitation slope)))
		  
(defmodel pollination-value 'esclass:Pollination 
	(es-calculator 'esclass:Pollination
    :influence (
                 (habitat:AnnualPrecipitation 0.11 :min 600 :max 800)
                 (policytarget:PopulationDensity -0.45 :min 500 :max 3000)
                 (geophysics:Altitude -0.45 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation population)))

;; -----------------------------------------------------------------------------
;; enchiladinha
;; -----------------------------------------------------------------------------

(defmodel esvalue-corrected 'esvaluation:ESValueCorrected
  (identification 'esvaluation:ESValueCorrected
    :context (food-production-value climate-regulation-value
               water-supply-value soil-formation-value
               pollination-value))) 

		  		  