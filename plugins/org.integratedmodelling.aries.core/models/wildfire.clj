;;; --------------------------------------------------------------------------------------------------
;;; models for 
;;; wildfire namespace
;;; ta Nov 20, 2009
;;; --------------------------------------------------------------------------------------------------

(ns models.wildfire
	(:refer modelling :only (defmodel probability classification ranking bayesian)))

(defmodel mature-gorse 'wildfireService:MatureGorse
  (probability 'wildfireService:MatureGorse 'wildfireService:AbsentMatureGorse 'wildfireService:PresentMatureGorse 
  :state #(condp = (:gorse-presence %)
                -1 [0.5 0.5] ; (discretize-distribution states (make-binomial n p))
                 0 [1.0 0.0]
                 1 [0.0 1.0])
  :context ((ranking 'wildfireService:MatureGorsePresenceProbability) :as gorse-presence)))

(defmodel burnable-vegetation 'wildfireService:BurnableVegetation
  (classification (ranking 'wildfireService:BurnableVegetation)
    0 'wildfireService:AbsentBurnableVegetation
    1 'wildfireService:PresentBurnableVegetation))

(defmodel burnable-structure 'wildfireService:BurnableStructure
  (classification (ranking 'wildfireService:BurnableStructure)
  0 'wildfireService:AbsentBurnableStructure
  1 'wildfireService:PresentBurnableStructure))
    
(defmodel wildfire-potential 'wildfireService:WildfirePotential
  (bayesian 'wildfireService:WildfirePotential 
    :import "aries.core::WildfireSourceHazard.xdsl"
    :keep ('wildfireService:WildfirePotential)
    :context (mature-gorse burnable-vegetation burnable-structures)))

(defmodel presence-of-structures 'wildfireService:PresenceOfStructures
  (classification (ranking 'wildfireService:PresenceOfStructures)
    0 'wildfireService:NoStructures
    1 'wildfireService:YesStructures))
    
(defmodel structure-value 'wildfireService:StructureValue
  (classification (ranking 'economics:AppraisedPropertyValue)
    [:< 100000]      'wildfireService:VeryLowStructureValue
    [100000 200000]  'wildfireService:LowStructureValue
    [200000 400000]  'wildfireService:ModerateStructureValue
    [400000 1000000] 'wildfireService:HighStructureValue
    [1000000 :>]     'wildfireService:VeryHighStructureValue))

(defmodel wildfire-damage 'wildfireService:WildfireDamage	
  (bayesian 'wildfireService:WildfireDamage 
    :import "aries.core::WildfireDamage.xdsl"
    :keep ('wildfireService:WildfireDamage)
    :context (presence-of-structures structure-value)))



		
;; first draft of stochastic input
;;(defmodel mature-gorse-static 'wildfireService:MatureGorse
;;  (classification 'wildfireService:MatureGorse)
;;    :state (condp = gorse-presence
;;                  -1 ([(tl/conc 'wildfireService:AbsentMatureGorse) (tl/conc 'wildfireService:PresentMatureGorse)] (binomial-draw 2 0.5))
;;                   0 (tl/conc 'wildfireService:AbsentMatureGorse)
;;                   1 (tl/conc 'wildfireService:PresentMatureGorse)
;;    :context ((ranking 'wildfireService:KnownGorsePresence) :as gorse-presence))
		 			