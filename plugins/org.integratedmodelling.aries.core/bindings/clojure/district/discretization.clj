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
  {"WaterBody"          {0.0 "NoWater", 1.0 "Lake", 2.0 "Ocean"}
   "Mountain"           #(get-range-name [2000.0 2750.0] ["NoMountain" "SmallMountain" "LargeMountain"] %)
   "PresenceOfHousing"  {0.0 "No", 1.0 "Yes"}
   "HousingValue"       #(get-range-name [100000.0 200000.0 400000.0 1000000.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "Highways"           {0.0 "No", 1.0 "Yes"}
   "CommercialIndustrialTransportation" {0.0 "No", 1.0 "Yes"}
   "Clearcuts"          {0.0 "No", 1.0 "Yes"}
   "Obstructions"       {0.0 "None", 1.0 "Partial", 2.0 "Full"}
   "Area"               #(get-range-name [20230.0 101200.0 404700.0] ["VerySmall" "Small" "Large" "VeryLarge"] %)
   "WaterQuality"       {0.0 "NoSurfaceWater", 5.0 "RequiringTMDL", 2.0 "OfConcern", 4.0 "OfConcern",
			 24.0 "OfConcern", 1.0 "MeetsStandards"}
   "Crime"              {0.0 "NonUrban", 1.0 "Urban"}
   "FormalProtection"   {0.0 "NonProtected", 1.0 "Protected"}
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
   "HydroelectricDams"  {0.0 "Absent", 1.0 "Present"}
   "MountainGlaciers"   {0.0 "Absent", 1.0 "Present"}
   "PopulationDensity"  #(get-range-name [38.6 193.0 772.2 1930.5] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CoastalFloodZone"   {0.0 "No", 1.0 "Yes"}
   "CoastalProperty"     {22.0 "LowDensity", 23.0 "ModerateDensity", 24.0 "HighDensity"}
   "LandslideRiskZone"  {0.0 "NoRisk", 1.0 "ModerateRisk", 2.0 "HighRisk"}
   "DownslopeHousing"   {0.0 "Absent", 1.0 "Present"}
   "Rainfall"           #(get-range-name [76.2 152.4 228.6 330.2] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "PotentialEvapotranspiration" #(get-range-name [50.0 100.0] ["Low" "Moderate" "High"] %)
   "AgriculturalLand"   {0.0 "No", 1.0 "Yes"}
   "FireProneHousing"   {0.0 "Absent", 1.0 "Present"}
   "FireFrequency"      #(get-range-name [0.0 1.0 3.0] ["None" "Low" "Moderate" "High"] %)
   "Forest "            {1.0 "Forest", 2.0 "Forest", 3.0 "Forest", 4.0 "Forest", 5.0 "Forest", 6.0 "Forest"}
   "GreenhouseGasEmitters"   #(get-range-name [0.0 2.0 5.0 10.0 20.0] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "VegetationCarbonStorage" #(get-range-name [0.0 50.0 100.0 150.0 200.0] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SoilCarbonStorage"       #(get-range-name [0.0 5.0 10.0 15.0 30.0] ["None" "VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "CanopyCover"             #(get-range-name [20.0 40.0 60.0 80.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)
   "SuccessionalStage"       #(get-range-name [12.7 25.4 50.8 76.2] ["Early" "Pole" "Mid" "Late" "OldGrowth"] %)
   "HardwoodSoftwoodRatio"   #(get-range-name [20.0 40.0 60.0 80.0] ["VerySoft" "Soft" "Moderate" "Hard" "VeryHard"] %)
   "SoilCNRatio"             #(get-range-name [5.0 10.0 20.0] ["VeryLow" "Low" "High" "VeryHigh"] %)
   "SummerHighWinterLow"     #(get-range-name [20.0 40.0 60.0 80.0] ["VeryLow" "Low" "Moderate" "High" "VeryHigh"] %)})

(def undiscretization-table
  {"WaterBody"          {"NoWater" 0.0, "Lake" 1.0, "Ocean" 2.0}
   "Mountain"           {"NoMountain" 1000.0, "SmallMountain" 2375.0, "LargeMountain" 3750.0}
   "PresenceOfHousing"  {"No" 0.0, "Yes" 1.0}
   "HousingValue"       {"VeryLow" 50000.0, "Low" 150000.0, "Moderate" 300000.0, "High" 700000.0, "VeryHigh" 1500000.0}
   "Highways"           {"No" 0.0, "Yes" 1.0}
   "CommercialIndustrialTransportation" {"No" 0.0, "Yes" 1.0}
   "Clearcuts"          {"No" 0.0, "Yes" 1.0}
   "Obstructions"       {"None" 0.0, "Partial" 1.0, "Full" 2.0}
   "Area"               {"VerySmall" 10115.0, "Small" 60715.0, "Large" 252950.0, "VeryLarge" 809400.0}
   "WaterQuality"       {"NoSurfaceWater" 0.0, "RequiringTMDL" 5.0, "OfConcern" 2.0, "OfConcern" 4.0,
			 "OfConcern" 24.0, "MeetsStandards" 1.0}
   "Crime"              {"NonUrban" 0.0, "Urban" 1.0}
   "FormalProtection"   {"NonProtected" 0.0, "Protected" 1.0}
   "Farm"               {"No" 0.0, "Yes" 1.0}
   "Park"               {"No" 0.0, "Yes" 1.0}
   "Cemetery"           {"No" 0.0, "Yes" 1.0}
   "GolfCourse"         {"No" 0.0, "Yes" 1.0}
   "LakeFront"          {"No" 0.0, "Yes" 1.0}
   "RiverFront"         {"No" 0.0, "Yes" 1.0}
   "Beach"              {"No" 0.0, "Yes" 1.0}
   "EmergentWetland"    {"No" 0.0, "Yes" 1.0}
   "WoodyWetland"       {"No" 0.0, "Yes" 1.0}
   "Forest"             {"No" 0.0, "Yes" 1.0}
   "UrbanProximity"     {"Rural" 38.6, "Suburban" 193.05, "Urban" 965.25}
   "HydroelectricDams"  {"Absent" 0.0, "Present" 1.0}
   "MountainGlaciers"   {"Absent" 0.0, "Present" 1.0}
   "PopulationDensity"  {"VeryLow" 19.3, "Low" 115.8, "Moderate" 482.6, "High" 1351.35, "VeryHigh" 2702.7}
   "CoastalFloodZone"   {"No" 0.0, "Yes" 1.0}
   "CoastalProperty"    {"LowDensity" 22.0, "ModerateDensity" 23.0, "HighDensity" 24.0}
   "LandslideRiskZone"  {"NoRisk" 0.0, "ModerateRisk" 1.0, "HighRisk" 2.0}
   "DownslopeHousing"   {"Absent" 0.0, "Present" 1.0}
   "Rainfall"           {"VeryLow" 38.1, "Low" 114.3, "Moderate" 190.5, "High" 279.4, "VeryHigh" 409.8}
   "PotentialEvapotranspiration" {"Low" 25.0, "Moderate" 75.0, "High" 125.0}
   "AgriculturalLand"   {"No" 0.0, "Yes" 1.0}
   "FireProneHousing"   {"Absent" 0.0, "Present" 1.0}
   "FireFrequency"      {"None" 0.0, "Low" 1.0, "Moderate" 2.5, "High" 4.0}
   "Forest "            {"No" 0.0, "Yes" 1.0}
   "GreenhouseGasEmitters"   {"None" 0.0, "VeryLow" 1.0, "Low" 3.5, "Moderate" 7.5, "High" 15.0, "VeryHigh" 30.0}
   "VegetationCarbonStorage" {"None" 0.0, "VeryLow" 25.0, "Low" 75.0, "Moderate" 125.0, "High" 175.0, "VeryHigh" 225.0}
   "SoilCarbonStorage"       {"None" 0.0, "VeryLow" 2.5, "Low" 7.5, "Moderate" 12.5, "High" 22.5, "VeryHigh" 37.5}
   "CanopyCover"             {"VeryLow" 10.0, "Low" 30.0, "Moderate" 50.0, "High" 70.0, "VeryHigh" 90.0}
   "SuccessionalStage"       {"Early" 6.35, "Pole" 19.05, "Mid" 38.1, "Late" 63.5, "OldGrowth" 95.25}
   "HardwoodSoftwoodRatio"   {"VerySoft" 10.0, "Soft" 30.0, "Moderate" 50.0, "Hard" 70.0, "VeryHard" 90.0}
   "SoilCNRatio"             {"VeryLow" 2.5, "Low" 7.5, "High" 15.0, "VeryHigh" 30.0}
   "SummerHighWinterLow"     {"VeryLow" 10.0, "Low" 30.0, "Moderate" 50.0, "High" 70.0, "VeryHigh" 90.0}
   "SensoryEnjoyment"  {"None" 0.0, "Low" 16.7, "Moderate" 50.0, "High" 83.3}
   "ProximityToBeauty" {"None" 0.0, "Low" 16.7, "Moderate" 50.0, "High" 83.3}
   "ClimateStability"  {"None" 0.0, "VeryLow" 10.0, "Low" 30.0, "Moderate" 50.0, "High" 70.0, "VeryHigh" 90.0}})
