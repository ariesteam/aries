(ns valuation.models.calculator
  (:refer-clojure :rename {count length}) 
  (:refer modelling :only (defmodel measurement classification categorization ranking namespace-ontology 
  												 numeric-coding binary-coding identification bayesian count))
  (:refer aries.valuation :only (es-calculator)))
  
(namespace-ontology esvaluation)

(defmodel land-use esclass:HistoricalESLandcover
	"Just a reclass of common land use typologies. Not really that good, but given what we use it for,
	 who cares. Should work across all the US and Europe if the data are available."
	(classification (numeric-coding nlcd:NLCDNumeric)
		#{81 82}	                       esclass:AgriculturePasture
		#{41 42 43}                      esclass:Forest
		#{71 72}	                       esclass:GrasslandsShrublands
		#{23 24}	                       esclass:Urban
		11	                             esclass:LakesRiversPondsReservoirs
		#{90 91 92 93 94 95 96 97 98 99} esclass:Wetlands
		21	                             esclass:Coastal
		31	                             esclass:Rock
		#{12 73 74}	                     esclass:Desert
		#{51 52}	                       esclass:Tundra)
	(classification (numeric-coding corine:CORINENumeric)
		[1 11 :inclusive]	               esclass:Urban
		[12 22 :inclusive]	             esclass:AgriculturePasture
		#{23 24 25}                      esclass:Forest
		#{26 27 28 29}	                 esclass:GrasslandsShrublands
		#{40 41 44}	                     esclass:LakesRiversPondsReservoirs
		[35 39 :inclusive]               esclass:Wetlands
		#{42 43}	                       esclass:Coastal
		31	                             esclass:Rock
		#{30 32 33 34}  	               esclass:Tundra))

;; ------------------------------------------------------------------------------------------------
;; the crap RB, MW and countless others get paid money to produce.
;; ------------------------------------------------------------------------------------------------
		
(defmodel esvalue HistoricESValue 
	"ES value calculator, Costanza/DeGroot/Wilson-style"
	(es-calculator HistoricESValue 
		:context (land-use)) )

;; ------------------------------------------------------------------------------------------------
;; "enhanced" valuation models. Still crap, but these would be worth 10 years of publications if they
;; came from the great Gund researchers.
;; ------------------------------------------------------------------------------------------------
		
(defmodel slope geophysics:DegreeSlope
	(measurement geophysics:DegreeSlope "\u00b0"))

(defmodel elevation geophysics:Altitude
	(measurement geophysics:Altitude "mm"))

(defmodel precipitation habitat:AnnualPrecipitation
	(measurement habitat:AnnualPrecipitation "mm"))

(defmodel population policytarget:PopulationDensity
  (count policytarget:PopulationDensity "/km^2")) 

(defmodel pipeline-small infrastructure:SmallPipeline
  (binary-coding infrastructure:SmallPipeline)) 

(defmodel pipeline-medium infrastructure:MediumPipeline
  (binary-coding infrastructure:MediumPipeline)) 

(defmodel pipeline-large infrastructure:LargePipeline
  (binary-coding infrastructure:LargePipeline)) 

(defmodel food-production-value esclass:FoodProduction 
	(es-calculator esclass:FoodProduction
    :influence (
                 (geophysics:DegreeSlope -0.4 :min 0 :max 17)
                 (policytarget:PopulationDensity -0.5 :min 100 :max 5000)
                 (habitat:AnnualPrecipitation 0.3 :min 600 :max 800)
                 (geophysics:Altitude -0.7 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population)))

(defmodel climate-regulation-value esclass:ClimateRegulation 
	(es-calculator esclass:ClimateRegulation
    :influence (
                 (geophysics:DegreeSlope -0.25 :min 4 :max 17)
                 (policytarget:PopulationDensity -0.25 :min 300 :max 1000)
                 (habitat:AnnualPrecipitation 0.4 :min 700 :max 800)
                 (geophysics:Altitude -0.1 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population)))

(defmodel water-supply-value esclass:WaterSupply 
	(es-calculator esclass:WaterSupply
    :influence (
                 (geophysics:DegreeSlope -0.15 :min 5 :max 10)
                 (policytarget:PopulationDensity -0.25 :min 1000 :max 5000)
                 (habitat:AnnualPrecipitation 0.2 :min 600 :max 800))
    :normalize false
		:context (land-use slope precipitation population)))
		  
(defmodel soil-formation-value esclass:SoilFormation 
	(es-calculator esclass:SoilFormation
    :influence (
                 (geophysics:DegreeSlope -0.5 :min 8 :max 15)
                 (policytarget:PopulationDensity -0.5 :min 200 :max 2000)
                 (habitat:AnnualPrecipitation -0.3 :min 600 :max 800))
    :normalize false    
		:context (land-use population precipitation slope)))
		  
(defmodel pollination-value esclass:Pollination 
	(es-calculator esclass:Pollination
    :influence (
                 (habitat:AnnualPrecipitation 0.11 :min 600 :max 800)
                 (policytarget:PopulationDensity -0.45 :min 500 :max 3000)
                 (geophysics:Altitude -0.45 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation population)))

(defmodel recreation-value esclass:Recreation 
	(es-calculator esclass:Recreation
    :influence (
                 (habitat:AnnualPrecipitation -0.20 :min 700 :max 800)
                 (policytarget:PopulationDensity -0.45 :min 500 :max 3000)
                 (geophysics:DegreeSlope -0.35 :min 25 :max 55))
    :normalize false    
		:context (land-use slope precipitation population)))

;; modified for pipeline effects, val dagri specific

(defmodel food-production-value-pipeline esclass:FoodProduction 
	(es-calculator esclass:FoodProduction
    :influence (
                 (infrastructure:MediumPipeline -1.00 :min 0 :max 1)
                 (geophysics:DegreeSlope -0.4 :min 0 :max 17)
                 (policytarget:PopulationDensity -0.5 :min 100 :max 5000)
                 (habitat:AnnualPrecipitation 0.3 :min 600 :max 800)
                 (geophysics:Altitude -0.7 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population pipeline-medium)))

(defmodel climate-regulation-value-pipeline esclass:ClimateRegulation 
	(es-calculator esclass:ClimateRegulation
    :influence (
                 (infrastructure:SmallPipeline -1.00 :min 0 :max 1)
                 (geophysics:DegreeSlope -0.25 :min 4 :max 17)
                 (policytarget:PopulationDensity -0.25 :min 300 :max 1000)
                 (habitat:AnnualPrecipitation 0.4 :min 700 :max 800)
                 (geophysics:Altitude -0.1 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation slope population pipeline-small)))

(defmodel water-supply-value-pipeline esclass:WaterSupply 
	(es-calculator esclass:WaterSupply
    :influence (
                 (infrastructure:MediumPipeline -0.80 :min 0 :max 1)
                 (geophysics:DegreeSlope -0.15 :min 5 :max 10)
                 (policytarget:PopulationDensity -0.25 :min 1000 :max 5000)
                 (habitat:AnnualPrecipitation 0.2 :min 600 :max 800))
    :normalize false
		:context (land-use slope precipitation population pipeline-medium)))
		  
(defmodel soil-formation-value-pipeline esclass:SoilFormation 
	(es-calculator esclass:SoilFormation
    :influence (
                 (infrastructure:MediumPipeline -0.50 :min 0 :max 1)
                 (geophysics:DegreeSlope -0.5 :min 8 :max 15)
                 (policytarget:PopulationDensity -0.5 :min 200 :max 2000)
                 (habitat:AnnualPrecipitation -0.3 :min 600 :max 800))
    :normalize false    
		:context (land-use population precipitation slope pipeline-medium)))
		  
(defmodel pollination-value-pipeline esclass:Pollination 
	(es-calculator esclass:Pollination
    :influence (
                 (infrastructure:LargePipeline -0.70 :min 0 :max 1)
                 (habitat:AnnualPrecipitation 0.11 :min 600 :max 800)
                 (policytarget:PopulationDensity -0.45 :min 500 :max 3000)
                 (geophysics:Altitude -0.45 :min 500 :max 1750))
    :normalize false    
		:context (land-use elevation precipitation population pipeline-large)))

(defmodel recreation-value-pipeline esclass:Recreation 
	(es-calculator esclass:Recreation
    :influence (
                 (infrastructure:LargePipeline -1.00 :min 0 :max 1)
                 (habitat:AnnualPrecipitation -0.20 :min 700 :max 800)
                 (policytarget:PopulationDensity -0.45 :min 500 :max 3000)
                 (geophysics:DegreeSlope -0.35 :min 25 :max 55))
    :normalize false    
		:context (land-use slope precipitation population pipeline-large)))

;; -----------------------------------------------------------------------------
;; enchiladinhas
;; -----------------------------------------------------------------------------

(defmodel esvalue-corrected ESValueCorrected
  (identification ESValueCorrected
    :context (food-production-value climate-regulation-value
               water-supply-value soil-formation-value
               pollination-value recreation-value))) 

(defmodel esvalue-corrected-pipeline ESValueCorrected
  (identification ESValueCorrected
    :context (food-production-value-pipeline climate-regulation-value-pipeline
               water-supply-value-pipeline soil-formation-value-pipeline
               pollination-value-pipeline recreation-value-pipeline))) 
		  		  