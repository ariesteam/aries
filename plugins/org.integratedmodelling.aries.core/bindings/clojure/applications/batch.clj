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
(defn save-view-model
  "Run the view model at the given resolution" 
  [resolution output-file]
  (let [flow-params {:trans-threshold  1.0
		     :source-threshold 0.05
		     :sink-threshold   0.20
		     :use-threshold    0.05
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival
		     :rv-max-states    10}]
    (aries.demo/save-gssm-demo-data 
     'aestheticService:SensoryEnjoyment
     'aestheticService:AestheticViewshedUse
     'aestheticService:ViewSink
     'aestheticService:LineOfSight flow-params resolution output-file)))
		
(defn save-proximity-model
  "Run the proximity model at the given resolution"
  [resolution output-file]
  (let [flow-params {:trans-threshold  1.0
		     :source-threshold 0.0
		     :sink-threshold   0.0
		     :use-threshold    0.0
		     :sink-type        :relative
		     :use-type         :relative
		     :benefit-type     :non-rival
		     :rv-max-states    10}]
    (aries.demo/save-gssm-demo-data
     'aestheticService:ProximityToBeauty
     'aestheticService:AestheticProximityUse
     'aestheticService:ProximitySink
     'aestheticService:Proximity flow-params resolution output-file)))

;;(save-view-model 256 "/home/gjohnson/viewflow.nc")
(save-proximity-model 256 "/home/gjohnson/proximityflow.nc")
;;(save-proximity-model 256 "C:/A/results/proximityflow.nc")