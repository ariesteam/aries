;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.

;; ---------------------------------------------------------------------------------------------------
;; test runs; should eventually contain functions to test run all ARIES models at the command line
;; ---------------------------------------------------------------------------------------------------

;; run the view model
(defn run-view-model
  "Run the view model at the given resolution"
  [location resolution]
  (let [flow-params {:trans-threshold  1.0
		     :source-threshold 0.05
		     :sink-threshold   0.20
		     :use-threshold    0.05
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival
		     :rv-max-states    10}]
    (aries.demo/run-span-interactive
     "aries/view/data" 
     'aestheticService:NaturalBeauty
     'aestheticService:HomeownersEnjoyment
     'aestheticService:ViewSink
     'geophysics:Altitude flow-params location resolution)))

(defn diocan
	[model-id location max-resolution]
	(let [data-obs  (modelling/run model-id location max-resolution)]
		(println (geospace/grid-rows data-obs) "X" (geospace/grid-columns data-obs) "\n"
						 (corescience/find-state data-obs 'aestheticService:NaturalBeauty) "\n" 
						 (corescience/find-state data-obs 'aestheticService:HomeownersEnjoyment) "\n" 
						 (corescience/find-state data-obs 'aestheticService:ViewSink) "\n" 
						 (corescience/find-state data-obs 'geophysics:Altitude))))

(diocan "aries/view/data" 'chehalis 128)

;(defn run-proximity-model
;  "Run the proximity model at the given resolution" 
;  [location resolution]
;  (let [flow-params {:trans-threshold  1.0
;		     :source-threshold 0.0
;		     :sink-threshold   0.0
;		     :use-threshold    0.0
;		     :sink-type        :relative
;		     :use-type         :relative
;		     :benefit-type     :non-rival
;		     :rv-max-states    10}]
;    (aries.demo/run-span-interactive
;	   ""
;     'aestheticService:ProximityToBeauty
;     'aestheticService:AestheticProximityUse
;     'aestheticService:ProximitySink
;     'aestheticService:Proximity flow-params location resolution)))

;(run-view-model 'chehalis 128)
;;(run-proximity-model 256)

;(aries.demo/make-dataset 'aestheticService:ProximityToBeauty "proximity_data" 512)
;(aries.demo/run-gssm-demo 'aestheticService:ProximityToBeauty 'aestheticService:AestheticProximityUse 64 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:AllPeopleEverywhere 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:GreenhouseGasEmitters 256 0.2)
;(aries.demo/run-gssm-demo 'carbonService:ClimateStability 'carbonService:ParticularlyVulnerableGroups 256 0.2)
