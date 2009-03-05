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

(def undiscretization-table
  {"WaterBody"          {"NoWater" 0, "Lake" 1, "Ocean" 2}
   "Mountain"           {"NoMountain" 1000, "SmallMountain" 2375, "LargeMountain" 3750}
   "PresenceOfHousing"  {"No" 0, "Yes" 1}
   "HousingValue"       {"VeryLow" 50000, "Low" 150000, "Moderate" 300000, "High" 700000, "VeryHigh" 1500000}
   "Highways"           {"No" 0, "Yes" 1}
   "CommercialIndustrialTransportation" {"No" 0, "Yes" 1}
   "Clearcuts"          {"No" 0, "Yes" 1}
   "Obstructions"       {"None" 0, "Partial" 1, "Full" 2}
   "Area"               {"VerySmall" 10115, "Small" 60715, "Large" 252950, "VeryLarge" 809400}
   "WaterQuality"       {"NoSurfaceWater" 0, "RequiringTMDL" 5, "OfConcern" 2, "OfConcern" 4,
			 "OfConcern" 24, "MeetsStandards" 1}
   "Crime"              {"NonUrban" 0, "Urban" 1}
   "FormalProtection"   {"NonProtected" 0, "Protected" 1}
   "Farm"               {"No" 0, "Yes" 1}
   "Park"               {"No" 0, "Yes" 1}
   "Cemetery"           {"No" 0, "Yes" 1}
   "GolfCourse"         {"No" 0, "Yes" 1}
   "LakeFront"          {"No" 0, "Yes" 1}
   "RiverFront"         {"No" 0, "Yes" 1}
   "Beach"              {"No" 0, "Yes" 1}
   "EmergentWetland"    {"No" 0, "Yes" 1}
   "WoodyWetland"       {"No" 0, "Yes" 1}
   "Forest"             {"No" 0, "Yes" 1}
   "UrbanProximity"     {"Rural" 38.6, "Suburban" 193.05, "Urban" 965.25}
   "HydroelectricDams"  {"Absent" 0, "Present" 1}
   "MountainGlaciers"   {"Absent" 0, "Present" 1}
   "PopulationDensity"  {"VeryLow" 19.3, "Low" 115.8, "Moderate" 482.6, "High" 1351.35, "VeryHigh" 2702.7}
   "CoastalFloodZone"   {"No" 0, "Yes" 1}
   "CoastalProperty"    {"LowDensity" 22, "ModerateDensity" 23, "HighDensity" 24}
   "LandslideRiskZone"  {"NoRisk" 0, "ModerateRisk" 1, "HighRisk" 2}
   "DownslopeHousing"   {"Absent" 0, "Present" 1}
   "Rainfall"           {"VeryLow" 38.1, "Low" 114.3, "Moderate" 190.5, "High" 279.4, "VeryHigh" 409.8}
   "PotentialEvapotranspiration" {"Low" 25, "Moderate" 75, "High" 125}
   "AgriculturalLand"   {"No" 0, "Yes" 1}
   "FireProneHousing"   {"Absent" 0, "Present" 1}
   "FireFrequency"      {"None" 0, "Low" 1, "Moderate" 2.5, "High" 4}
   "Forest "            {"No" 0, "Yes" 1}
   "GreenhouseGasEmitters"   {"None" 0, "VeryLow" 1, "Low" 3.5, "Moderate" 7.5, "High" 15, "VeryHigh" 30}
   "VegetationCarbonStorage" {"None" 0, "VeryLow" 25, "Low" 75, "Moderate" 125, "High" 175, "VeryHigh" 225}
   "SoilCarbonStorage"       {"None" 0, "VeryLow" 2.5, "Low" 7.5, "Moderate" 12.5, "High" 22.5, "VeryHigh" 37.5}
   "CanopyCover"             {"VeryLow" 10, "Low" 30, "Moderate" 50, "High" 70, "VeryHigh" 90}
   "SuccessionalStage"       {"Early" 6.35, "Pole" 19.05, "Mid" 38.1, "Late" 63.5, "OldGrowth" 95.25}
   "HardwoodSoftwoodRatio"   {"VerySoft" 10, "Soft" 30, "Moderate" 50, "Hard" 70, "VeryHard" 90}
   "SoilCNRatio"             {"VeryLow" 2.5, "Low" 7.5, "High" 15, "VeryHigh" 30}
   "SummerHighWinterLow"     {"VeryLow" 10, "Low" 30, "Moderate" 50, "High" 70, "VeryHigh" 90}})
