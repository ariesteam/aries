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

(defn- between [val low high] (and (>= val low) (< val high)))

(defn- get-range-name [range-boundaries range-names value]
  (cond (empty? range-boundaries) (first range-names)
	(<= value (first range-boundaries)) (first range-names)
	:otherwise (recur (rest range-boundaries) (rest range-names) value)))

(def discretization-table
  {"WaterBody"          {0 "NoWater", 1 "Lake", 2 "Ocean"}
   "Mountain"           #(get-range-name [2000 2750] ["NoMountain" "SmallMountain" "LargeMountain"] %)
   "PresenceOfHousing"  {0 "No", 1 "Yes"}
   "HousingValue"       #(get-range-name [100000 200000 400000 1000000] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "Highways"           {0 "No", 1 "Yes"}
   "CommercialIndustrialTransportation" {0 "No", 1 "Yes"}
   "Clearcuts"          {0 "No", 1 "Yes"}
   "Obstructions"       {0 "None", 1 "Partial", 2 "Full"}
   "Area"               #(get-range-name [20230 101200 404700] ["VerySmall" "Small" "Large" "VeryLarge"] %)
   "WaterQuality"       {0 "NoSurfaceWater", 5 "RequiringTMDL", 2 "OfConcern", 4 "OfConcern",
			 24 "OfConcern", 1 "MeetsStandards"}
   "Crime"              {0 "NonUrban", 1 "Urban"}
   "FormalProtection"   {0 "NonProtected", 1 "Protected"}
   "Farm"               {0 "No", 1 "Yes"}
   "Park"               {0 "No", 1 "Yes"}
   "Cemetery"           {0 "No", 1 "Yes"}
   "GolfCourse"         {0 "No", 1 "Yes"}
   "LakeFront"          {0 "No", 1 "Yes"}
   "RiverFront"         {0 "No", 1 "Yes"}
   "Beach"              {0 "No", 1 "Yes"}
   "EmergentWetland"    {0 "No", 1 "Yes"}
   "WoodyWetland"       {0 "No", 1 "Yes"}
   "Forest"             {0 "No", 1 "Yes"}
   "UrbanProximity"     #(get-range-name [77.2 308.9] ["Rural" "Suburban" "Urban"] %)
   "HydroelectricDams"  {0 "Absent", 1 "Present"}
   "MountainGlaciers"   {0 "Absent", 1 "Present"}
   "PopulationDensity"  #(get-range-name [38.6 193.0 772.2 1930.5] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CoastalFloodZone"   {0 "No", 1 "Yes"}
   "CoastalProperty"     {22 "LowDensity", 23 "ModerateDensity", 24 "HighDensity"}
   "LandslideRiskZone"  {0 "NoRisk", 1 "ModerateRisk", 2 "HighRisk"}
   "DownslopeHousing"   {0 "Absent", 1 "Present"}
   "Rainfall"           #(get-range-name [76.2 152.4 228.6 330.2] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "PotentialEvapotranspiration" #(get-range-name [50 100] ["Low" "Moderate" "High"] %)
   "AgriculturalLand"   {0 "No", 1 "Yes"}
   "FireProneHousing"   {0 "Absent", 1 "Present"}
   "FireFrequency"      #(get-range-name [0 1 3] ["None" "Low" "Moderate" "High"] %)
   "Forest "            {1 "Forest", 2 "Forest", 3 "Forest", 4 "Forest", 5 "Forest", 6 "Forest"}
   "GreenhouseGasEmitters"   #(get-range-name [0 2 5 10 20] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "VegetationCarbonStorage" #(get-range-name [0 50 100 150 200] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SoilCarbonStorage"       #(get-range-name [0 5 10 15 30] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CanopyCover"             #(get-range-name [20 40 60 80] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SuccessionalStage"       #(get-range-name [12.7 25.4 50.8 76.2] ["Early" "Pole" "Mid" "Late" "OldGrowth"] %)
   "HardwoodSoftwoodRatio"   #(get-range-name [20 40 60 80] ["VerySoft" "Soft" "Moderate" "Hard" "VeryHard"] %)
   "SoilCNRatio"             #(get-range-name [5 10 20] ["VeryLow" "Low" "High" "VeryHigh"] %)
   "SummerHighWinterLow"     #(get-range-name [20 40 60 80] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)})
