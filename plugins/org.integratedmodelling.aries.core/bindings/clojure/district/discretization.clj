;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-DISTRICT.
;;;
;;; CLJ-DISTRICT is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; CLJ-DISTRICT is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-DISTRICT.  If not, see
;;; <http://www.gnu.org/licenses/>.

(ns district.discretization
  (:refer-clojure))

(defn- get-range-name [range-boundaries range-names value]
  (cond (empty? range-boundaries) (first range-names)
	(<= value (first range-boundaries)) (first range-names)
	:otherwise (recur (rest range-boundaries) (rest range-names) value)))

(def discretization-table
  {"WaterBody"          {0.0 "NoWater", 1.0 "Lake", 2.0 "Ocean"}
   "Mountain"           #(get-range-name [2000.0 2750.0 50000.0] ["NoMountain" "SmallMountain" "LargeMountain" "NoMountain"] %)
   "PresenceOfHousing"  {0.0 "No", 1.0 "Yes"}
   "HousingValue"       #(get-range-name [100000.0 200000.0 400000.0 1000000.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "Highways"           {0.0 "No", 1.0 "Yes"}
   "CommercialIndustrialTransportation" {0.0 "No", 1.0 "Yes"}
   "Clearcuts"          {0.0 "No", 1.0 "Yes"}
   "Obstructions"       {0.0 "None", 1.0 "Partial", 2.0 "Full"}
   "Area"               #(get-range-name [20230.0 101200.0 404700.0] ["VerySmall" "Small" "Large" "VeryLarge"] %)
   "WaterQuality"       {0.0 "NoSurfaceWater", 1.0 "MeetsStandards",  2.0 "OfConcern", 5.0 "RequiringTMDL"}
   "Crime"              {0.0 "NonUrban", 1.0 "Urban"}
   "FormalProtection"   {0.0 "NotProtected", 1.0 "Protected"}
   "Farm"               {0.0 "No", 1.0 "Yes"}
   "Park"               {0.0 "No", 1.0 "Yes"}
   "Cemetery"           {0.0 "No", 1.0 "Yes"}
   "GolfCourse"         {0.0 "No", 1.0 "Yes"}
   "LakeFront"          {0.0 "No", 1.0 "Yes"}
   "RiverFront"         {0.0 "No", 1.0 "Yes"}
   "Beach"              {0.0 "No", 1.0 "Yes"}
   "EmergentWetland"    {0.0 "No", 1.0 "Yes"}
   "WoodyWetland"       {0.0 "No", 1.0 "Yes"}
   "Forest"             {0.0 "No", 1.0 "Yes"}
   "UrbanProximity"     #(get-range-name [77.2 308.9] ["Rural" "Suburban" "Urban"] %)
   "HydroelectricDams"  {0.0 "No", 1.0 "Yes"}
   "MountainGlaciers"   {0.0 "No", 1.0 "Yes"}
   "PopulationDensity"  #(get-range-name [0.0 38.6 193.0 772.2 1930.5] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CoastalFloodZone"   {0.0 "No", 1.0 "Yes"}
   "CoastalProperty"    {0.0 "No", 1.0 "Yes"}
   "LandslideRiskZone"  {0.0 "NoRisk", 1.0 "AtRisk"}
   "DownslopeHousing"   {0.0 "No", 1.0 "Yes"}
   "Rainfall"           #(get-range-name [76.2 152.4 228.6 330.2] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "PotentialEvapotranspiration" #(get-range-name [50.0 100.0] ["Low" "Moderate" "High"] %)
   "AgriculturalLand"   {0.0 "No", 1.0 "Yes"}
   "FireProneHousing"   {0.0 "No", 1.0 "Yes"}
   "FireFrequency"      #(get-range-name [0.0 1.0 3.0] ["None" "Low" "Moderate" "High"] %)
   "GreenhouseGasEmissions"  #(get-range-name [0.0 6500 20000 37500 65000] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "VegetationCarbonStorage" #(get-range-name [0.0 50.0 100.0 150.0 200.0] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SoilCarbonStorage"       #(get-range-name [0.0 5.0 10.0 15.0 30.0] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CanopyCover"             #(get-range-name [20.0 40.0 60.0 80.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SuccessionalStage"       {0.0 "None", 1.0 "Early", 2.0 "Pole", 3.0 "Mid", 4.0 "Late", 5.0 "OldGrowth"}
   "HardwoodSoftwoodRatio"   {0.0 "None", 5.0 "VerySoft", 4.0 "Soft", 3.0 "Moderate", 2.0 "Hard", 1.0 "VeryHard"}
   "SoilCNRatio"             #(get-range-name [10.0 20.0 35.0] ["VeryLow" "Low" "High" "VeryHigh"] %)
   "SummerHighWinterLow"     #(get-range-name [20.0 40.0 60.0 80.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)})

(def undiscretization-table
  {"SensoryEnjoyment"  {"None" 0.0, "Low" 16.7, "Moderate" 50.0, "High" 83.3}
   "ProximityToBeauty" {"None" 0.0, "Low" 16.7, "Moderate" 50.0, "High" 83.3}
   "ClimateStability"  {"None" 0.0, "VeryLow" 10.0, "Low" 30.0, "Moderate" 50.0, "High" 70.0, "VeryHigh" 90.0}})
