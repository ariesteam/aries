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

(ns core.contexts.ontario
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))
;;;"

(defcontext lakeofthewoods-wgs84
  "Two sub-watersheds on the south side of the Lake of the Woods Provincial Park region."
  (grid
   512
   "EPSG:4326 MULTIPOLYGON (((-94.26 49.10,-94.26 49.10,-94.27 49.09,-94.27 49.09,-94.26 49.09,-94.26 49.09,-94.25 49.08,-94.25 49.08,-94.25 49.08,-94.24 49.08,-94.23 49.07,-94.22 49.08,-94.22 49.08,-94.22 49.08,-94.22 49.08,-94.22 49.08,-94.22 49.07,-94.22 49.07,-94.22 49.07,-94.22 49.07,-94.21 49.06,-94.21 49.06,-94.21 49.06,-94.20 49.07,-94.19 49.07,-94.18 49.07,-94.17 49.08,-94.17 49.08,-94.16 49.08,-94.16 49.07,-94.16 49.07,-94.16 49.07,-94.15 49.08,-94.15 49.08,-94.14 49.08,-94.14 49.07,-94.14 49.07,-94.13 49.06,-94.13 49.06,-94.14 49.06,-94.15 49.05,-94.15 49.05,-94.16 49.05,-94.15 49.04,-94.15 49.04,-94.14 49.04,-94.13 49.04,-94.13 49.04,-94.13 49.04,-94.13 49.04,-94.14 49.04,-94.14 49.05,-94.14 49.05,-94.13 49.05,-94.13 49.05,-94.13 49.05,-94.12 49.05,-94.10 49.05,-94.09 49.04,-94.09 49.04,-94.09 49.03,-94.10 49.02,-94.11 49.02,-94.11 49.02,-94.12 49.02,-94.12 49.02,-94.12 49.02,-94.13 49.02,-94.12 49.01,-94.12 49.01,-94.12 49.01,-94.11 49.01,-94.11 49.01,-94.11 49.01,-94.11 49.00,-94.11 49.00,-94.11 48.99,-94.11 48.99,-94.11 48.99,-94.11 48.99,-94.12 49.00,-94.12 49.00,-94.12 48.99,-94.12 48.99,-94.12 48.99,-94.12 48.99,-94.12 48.98,-94.12 48.98,-94.12 48.98,-94.11 48.98,-94.11 48.98,-94.11 48.98,-94.11 48.98,-94.11 48.98,-94.11 48.98,-94.11 48.97,-94.13 48.97,-94.14 48.97,-94.14 48.97,-94.14 48.97,-94.15 48.97,-94.16 48.96,-94.16 48.96,-94.16 48.96,-94.16 48.96,-94.16 48.96,-94.17 48.95,-94.17 48.95,-94.18 48.95,-94.18 48.95,-94.19 48.95,-94.19 48.96,-94.19 48.96,-94.19 48.96,-94.18 48.97,-94.17 48.97,-94.17 48.97,-94.17 48.98,-94.18 48.98,-94.18 48.98,-94.18 48.98,-94.18 48.98,-94.19 48.97,-94.19 48.97,-94.19 48.97,-94.20 48.97,-94.20 48.97,-94.21 48.97,-94.21 48.97,-94.22 48.97,-94.23 48.98,-94.24 48.98,-94.24 48.99,-94.24 48.99,-94.25 48.99,-94.26 48.98,-94.26 48.98,-94.28 48.98,-94.28 48.98,-94.29 48.99,-94.30 48.99,-94.31 49.00,-94.32 49.00,-94.32 49.00,-94.33 49.00,-94.33 49.00,-94.35 48.99,-94.37 48.99,-94.38 48.99,-94.38 48.99,-94.38 48.99,-94.38 48.99,-94.38 48.98,-94.38 48.98,-94.38 48.98,-94.39 48.98,-94.40 48.98,-94.42 48.98,-94.42 48.98,-94.43 48.98,-94.44 48.98,-94.44 48.98,-94.44 48.99,-94.44 48.99,-94.43 49.00,-94.42 49.00,-94.42 49.00,-94.41 49.00,-94.42 49.01,-94.42 49.01,-94.42 49.01,-94.46 49.02,-94.46 49.03,-94.46 49.03,-94.47 49.03,-94.47 49.03,-94.48 49.04,-94.48 49.04,-94.48 49.04,-94.48 49.05,-94.47 49.05,-94.45 49.05,-94.42 49.06,-94.42 49.06,-94.42 49.06,-94.41 49.06,-94.41 49.06,-94.40 49.06,-94.40 49.06,-94.39 49.06,-94.39 49.06,-94.39 49.07,-94.39 49.08,-94.39 49.09,-94.39 49.09,-94.39 49.09,-94.39 49.09,-94.38 49.09,-94.36 49.10,-94.34 49.10,-94.34 49.10,-94.33 49.10,-94.33 49.10,-94.32 49.10,-94.32 49.10,-94.31 49.10,-94.30 49.11,-94.27 49.11,-94.27 49.11,-94.26 49.10,-94.26 49.10,-94.26 49.10,-94.26 49.10,-94.26 49.10)),((-93.98 49.19, -93.98 49.19, -93.97 49.19, -93.96 49.19, -93.96 49.19, -93.96 49.19, -93.95 49.18, -93.93 49.18, -93.92 49.17, -93.91 49.17, -93.91 49.17, -93.87 49.16, -93.86 49.16, -93.85 49.16, -93.85 49.16, -93.83 49.15, -93.81 49.15, -93.80 49.15, -93.80 49.15, -93.79 49.15, -93.77 49.15, -93.77 49.15, -93.76 49.15, -93.75 49.15, -93.73 49.16, -93.73 49.16, -93.73 49.15, -93.73 49.15, -93.73 49.15, -93.72 49.15, -93.72 49.15, -93.71 49.15, -93.70 49.15, -93.68 49.15, -93.66 49.15, -93.66 49.15, -93.65 49.15, -93.65 49.14, -93.63 49.12, -93.63 49.11, -93.63 49.11, -93.64 49.11, -93.64 49.10, -93.64 49.11, -93.64 49.11, -93.65 49.11, -93.65 49.11, -93.65 49.11, -93.65 49.11, -93.65 49.11, -93.65 49.11, -93.66 49.11, -93.67 49.11, -93.67 49.09, -93.67 49.09, -93.67 49.09, -93.66 49.08, -93.66 49.08, -93.66 49.08, -93.66 49.08, -93.66 49.08, -93.66 49.06, -93.66 49.06, -93.67 49.06, -93.68 49.06, -93.69 49.05, -93.69 49.04, -93.69 49.04, -93.69 49.04, -93.69 49.04, -93.69 49.03, -93.70 49.03, -93.70 49.03, -93.70 49.03, -93.70 49.03, -93.70 49.03, -93.70 49.03, -93.71 49.03, -93.72 49.02, -93.72 49.02, -93.72 49.02, -93.72 49.02, -93.72 49.02, -93.72 49.02, -93.72 49.01, -93.72 49.01, -93.72 49.01, -93.73 49.02, -93.74 49.02, -93.74 49.02, -93.74 49.02, -93.74 49.02, -93.74 49.02, -93.74 49.02, -93.75 49.01, -93.75 49.00, -93.76 49.00, -93.76 49.00, -93.76 49.00, -93.75 48.99, -93.75 48.99, -93.75 49.00, -93.74 49.00, -93.74 49.00, -93.74 49.00, -93.73 49.00, -93.73 49.00, -93.73 49.00, -93.73 49.00, -93.74 48.99, -93.75 48.98, -93.77 48.98, -93.77 48.98, -93.78 48.98, -93.78 48.98, -93.79 48.97, -93.79 48.97, -93.79 48.97, -93.79 48.97, -93.78 48.97, -93.78 48.97, -93.79 48.96, -93.79 48.96, -93.79 48.96, -93.79 48.96, -93.80 48.95, -93.80 48.95, -93.81 48.95, -93.82 48.94, -93.82 48.94, -93.83 48.94, -93.83 48.94, -93.84 48.94, -93.84 48.94, -93.84 48.93, -93.84 48.93, -93.85 48.93, -93.85 48.93, -93.85 48.94, -93.85 48.94, -93.85 48.94, -93.84 48.94, -93.84 48.94, -93.84 48.95, -93.84 48.95, -93.84 48.95, -93.85 48.96, -93.86 48.96, -93.87 48.96, -93.88 48.96, -93.88 48.96, -93.88 48.95, -93.88 48.95, -93.90 48.94, -93.90 48.93, -93.90 48.93, -93.91 48.93, -93.91 48.93, -93.91 48.93, -93.91 48.93, -93.91 48.93, -93.91 48.93, -93.92 48.92, -93.91 48.92, -93.90 48.92, -93.89 48.92, -93.89 48.92, -93.88 48.92, -93.88 48.91, -93.88 48.91, -93.88 48.91, -93.89 48.89, -93.90 48.89, -93.90 48.89, -93.91 48.89, -93.92 48.89, -93.92 48.89, -93.92 48.89, -93.92 48.89, -93.92 48.89, -93.92 48.90, -93.92 48.90, -93.94 48.89, -93.95 48.89, -93.96 48.88, -93.96 48.87, -93.96 48.87, -93.97 48.87, -93.97 48.87, -93.97 48.87, -93.98 48.87, -93.98 48.87, -93.98 48.87, -93.98 48.88, -93.99 48.88, -93.99 48.88, -93.99 48.88, -94.00 48.88, -94.00 48.88, -94.00 48.88, -94.00 48.88, -94.00 48.87, -94.02 48.87, -94.02 48.87, -94.02 48.87, -94.03 48.87, -94.03 48.87, -94.03 48.87, -94.05 48.89, -94.05 48.90, -94.06 48.90, -94.06 48.90, -94.06 48.90, -94.07 48.89, -94.08 48.89, -94.09 48.89, -94.09 48.88, -94.09 48.88, -94.10 48.88, -94.10 48.88, -94.10 48.88, -94.11 48.88, -94.11 48.88, -94.11 48.88, -94.13 48.88, -94.13 48.87, -94.13 48.87, -94.14 48.87, -94.14 48.87, -94.14 48.87, -94.14 48.87, -94.14 48.87, -94.14 48.88, -94.14 48.90, -94.13 48.90, -94.13 48.90, -94.13 48.90, -94.13 48.90, -94.13 48.90, -94.13 48.91, -94.13 48.91, -94.14 48.91, -94.17 48.91, -94.17 48.92, -94.17 48.92, -94.17 48.93, -94.17 48.93, -94.17 48.93, -94.16 48.93, -94.15 48.94, -94.15 48.94, -94.16 48.94, -94.17 48.94, -94.17 48.94, -94.18 48.95, -94.18 48.95, -94.17 48.95, -94.17 48.95, -94.17 48.95, -94.16 48.96, -94.16 48.96, -94.16 48.96, -94.16 48.96, -94.16 48.96, -94.15 48.97, -94.14 48.97, -94.14 48.97, -94.14 48.97, -94.13 48.97, -94.11 48.97, -94.11 48.98, -94.11 48.98, -94.11 48.98, -94.11 48.98, -94.11 48.98, -94.11 48.98, -94.12 48.98, -94.12 48.98, -94.12 48.98, -94.12 48.99, -94.12 48.99, -94.12 48.99, -94.12 48.99, -94.12 49.00, -94.12 49.00, -94.11 48.99, -94.11 48.99, -94.11 48.99, -94.11 48.99, -94.11 49.00, -94.11 49.00, -94.11 49.01, -94.11 49.01, -94.11 49.01, -94.12 49.01, -94.12 49.01, -94.12 49.01, -94.13 49.02, -94.12 49.02, -94.12 49.02, -94.12 49.02, -94.11 49.02, -94.11 49.02, -94.10 49.02, -94.09 49.03, -94.09 49.04, -94.09 49.04, -94.10 49.05, -94.12 49.05, -94.13 49.05, -94.13 49.05, -94.13 49.05, -94.14 49.05, -94.14 49.05, -94.14 49.04, -94.13 49.04, -94.13 49.04, -94.13 49.04, -94.13 49.04, -94.14 49.04, -94.15 49.04, -94.15 49.04, -94.16 49.05, -94.15 49.05, -94.15 49.05, -94.14 49.06, -94.13 49.06, -94.13 49.06, -94.14 49.07, -94.14 49.07, -94.14 49.08, -94.15 49.08, -94.15 49.08, -94.16 49.07, -94.16 49.07, -94.16 49.07, -94.16 49.08, -94.17 49.08, -94.17 49.08, -94.18 49.07, -94.19 49.07, -94.20 49.07, -94.21 49.06, -94.21 49.06, -94.21 49.06, -94.22 49.07, -94.22 49.07, -94.22 49.07, -94.22 49.07, -94.22 49.08, -94.22 49.08, -94.22 49.08, -94.22 49.08, -94.22 49.08, -94.23 49.07, -94.24 49.08, -94.25 49.08, -94.25 49.08, -94.25 49.08, -94.26 49.09, -94.26 49.09, -94.26 49.10, -94.26 49.10, -94.26 49.10, -94.26 49.10, -94.27 49.11, -94.27 49.11, -94.30 49.11, -94.31 49.10, -94.32 49.10, -94.32 49.10, -94.33 49.10, -94.33 49.10, -94.34 49.10, -94.34 49.11, -94.35 49.13, -94.34 49.13, -94.34 49.13, -94.34 49.13, -94.34 49.14, -94.34 49.14, -94.33 49.14, -94.33 49.14, -94.35 49.14, -94.36 49.14, -94.37 49.14, -94.41 49.14, -94.36 49.15, -94.35 49.15, -94.34 49.15, -94.33 49.16, -94.33 49.16, -94.33 49.16, -94.33 49.16, -94.33 49.16, -94.33 49.16, -94.32 49.16, -94.32 49.16, -94.32 49.16, -94.31 49.16, -94.31 49.16, -94.30 49.16, -94.29 49.15, -94.29 49.15, -94.26 49.15, -94.26 49.15, -94.26 49.15, -94.25 49.16, -94.26 49.16, -94.25 49.16, -94.24 49.16, -94.24 49.17, -94.23 49.16, -94.23 49.16, -94.23 49.16, -94.22 49.16, -94.21 49.16, -94.18 49.17, -94.17 49.17, -94.17 49.18, -94.16 49.18, -94.14 49.18, -94.13 49.18, -94.13 49.18, -94.13 49.18, -94.12 49.18, -94.11 49.18, -94.10 49.18, -94.10 49.18, -94.09 49.18, -94.08 49.18, -94.08 49.18, -94.08 49.18, -94.08 49.18, -94.07 49.19, -94.04 49.19, -94.03 49.19, -94.02 49.19, -94.00 49.19, -94.00 49.19, -93.99 49.19, -93.99 49.19, -93.99 49.19, -93.98 49.19))) "))


(defcontext algonquin-wgs84
  "Two Algonquin Provincial Park region."
  (grid
  512
  "EPSG:4326 POLYGON ((-79.18 46.02, -79.18 46.02, -79.18 46.02, -79.18 46.02, -79.18 46.02, -79.18 46.02, -79.18 46.02, -79.18 46.03, -79.18 46.03, -79.18 46.03, -79.17 46.03, -79.17 46.03, -79.17 46.03, -79.17 46.03, -79.17 46.03, -79.17 46.03, -79.17 46.03, -79.16 46.03, -79.12 46.05, -79.10 46.05, -79.10 46.05, -79.10 46.05, -79.10 46.05, -79.08 46.06, -79.08 46.06, -79.06 46.06, -79.03 46.07, -78.97 46.09, -78.96 46.09, -78.95 46.09, -78.95 46.09, -78.95 46.09, -78.62 46.14, -78.62 46.14, -78.62 46.13, -78.56 46.15, -78.55 46.15, -78.55 46.15, -78.55 46.15, -78.55 46.15, -78.55 46.15, -78.55 46.15, -78.55 46.15, -78.54 46.15, -77.88 46.10, -77.88 46.10, -77.88 46.09, -77.87 46.08, -77.87 46.08, -77.86 46.08, -77.86 46.08, -77.77 46.11, -77.77 46.11, -77.76 46.11, -77.73 46.12, -77.70 46.13, -77.70 46.13, -77.70 46.13, -77.69 46.13, -77.69 46.13, -77.69 46.13, -77.69 46.13, -77.69 46.13, -77.69 46.13, -77.68 46.13, -77.68 46.13, -77.68 46.13, -77.68 46.13, -77.68 46.12, -77.68 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.67 46.12, -77.66 46.12, -78.95 46.09, -78.95 46.09, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.10, -78.95 46.11, -78.95 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.94 46.11, -78.93 46.11, -78.93 46.11, -78.90 46.12, -78.89 46.13, -78.89 46.13, -78.89 46.13, -78.89 46.13, -78.89 46.13, -78.89 46.13, -78.89 46.13, -78.89 46.14, -78.89 46.14, -78.89 46.14, -78.89 46.14, -78.89 46.14, -78.88 46.14, -78.88 46.14, -78.88 46.14, -78.88 46.14, -78.88 46.15, -78.88 46.15, -78.88 46.15, -78.88 46.15, -78.88 46.15, -78.87 46.15, -78.87 46.15, -78.87 46.15, -78.87 46.15, -78.82 46.16, -78.82 46.16, -78.82 46.16, -78.82 46.16, -78.82 46.16, -78.82 46.16, -78.81 46.16, -78.81 46.16, -78.81 46.16, -78.81 46.16, -78.81 46.16, -78.81 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.80 46.16, -78.79 46.16, -78.79 46.16, -78.79 46.15, -78.79 46.15, -78.79 46.15, -78.79 46.15, -78.79 46.15, -78.78 46.15, -78.76 46.16, -78.76 46.16, -78.75 46.16, -78.75 46.16, -78.75 46.16, -78.75 46.16, -78.75 46.16, -78.75 46.16, -78.53 46.15, -78.53 46.15, -78.53 46.15, -78.53 46.15, -78.52 46.15, -78.52 46.14, -78.52 46.14, -78.52 46.14, -78.52 46.14, -78.51 46.13, -78.51 46.12, -78.51 46.12, -78.50 46.11, -78.50 46.11, -78.49 46.11, -78.46 46.12, -78.46 46.12, -78.46 46.12, -78.46 46.12, -78.45 46.12, -78.74 46.16, -78.74 46.16, -78.74 46.16, -78.74 46.16, -78.74 46.16, -78.74 46.16, -78.73 46.16, -78.73 46.16, -78.73 46.16, -78.73 46.16, -78.73 46.16, -78.73 46.16, -78.73 46.16, -78.72 46.15, -78.72 46.15, -78.72 46.15, -78.72 46.15, -78.72 46.15, -78.72 46.15, -78.71 46.14, -78.71 46.13, -78.71 46.13, -78.71 46.13, -78.71 46.13, -78.71 46.13, -78.66 46.15, -78.66 46.15, -78.65 46.15, -78.65 46.15, -78.65 46.15, -78.65 46.15, -78.65 46.15, -78.65 46.15, -78.64 46.15, -78.64 46.15, -78.64 46.15, -78.64 46.15, -78.64 46.15, -78.64 46.14, -78.63 46.14, -78.63 46.14, -78.63 46.14, -78.63 46.14, -78.63 46.14, -78.05 45.60, -78.05 45.60, -78.05 45.60, -78.05 45.60, -78.63 46.14, -78.63 46.14, -78.63 46.14, -78.62 46.14, -78.54 46.15, -78.54 46.15, -78.54 46.15, -78.54 46.15, -78.54 46.15, -78.53 46.15, -78.53 46.15, -78.53 46.15, -78.45 46.12, -78.45 46.12, -78.45 46.12, -78.45 46.12, -78.45 46.12, -78.44 46.12, -78.44 46.12, -78.44 46.12, -78.44 46.12, -78.44 46.12, -78.44 46.12, -78.43 46.12, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.43 46.11, -78.42 46.11, -78.40 46.12, -78.37 46.12, -78.37 46.12, -78.37 46.12, -78.37 46.12, -78.36 46.13, -78.36 46.13, -78.33 46.13, -78.33 46.13, -78.33 46.13, -78.33 46.13, -78.33 46.13, -78.33 46.13, -78.32 46.13, -78.32 46.13, -78.32 46.13, -78.32 46.13, -78.32 46.13, -78.32 46.13, -78.31 46.13, -78.31 46.13, -78.31 46.13, -78.31 46.13, -78.31 46.13, -78.31 46.13, -78.31 46.13, -78.30 46.13, -78.30 46.13, -78.30 46.13, -78.30 46.12, -78.30 46.12, -78.30 46.12, -78.23 46.14, -78.22 46.14, -78.16 46.16, -78.15 46.16, -78.14 46.16, -78.14 46.16, -78.14 46.16, -78.14 46.16, -78.14 46.17, -78.13 46.17, -78.13 46.17, -78.13 46.17, -78.13 46.17, -78.13 46.17, -78.13 46.17, -78.12 46.17, -78.12 46.17, -78.12 46.17, -78.12 46.17, -78.12 46.16, -78.12 46.16, -78.12 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -78.11 46.16, -77.90 46.12, -77.90 46.12, -77.90 46.12, -77.90 46.12, -77.90 46.12, -77.89 46.12, -77.89 46.12, -77.89 46.12, -77.89 46.11, -77.89 46.11, -77.89 46.11, -77.89 46.11, -78.10 46.15, -78.10 46.15, -78.08 46.11, -78.07 46.09, -78.07 46.09, -78.07 46.09, -78.06 46.09, -78.04 46.09, -78.01 46.10, -77.99 46.11, -77.96 46.12, -77.94 46.12, -77.92 46.12, -77.92 46.12, -77.92 46.12, -77.92 46.12, -77.92 46.12, -77.92 46.12, -77.92 46.12, -77.91 46.12, -77.91 46.12, -77.91 46.12, -77.91 46.12, -77.91 46.12, -77.91 46.12, -77.90 46.12, -77.90 46.12, -77.90 46.12, -77.66 46.11, -77.65 46.10, -77.65 46.09, -77.64 46.08, -77.64 46.08, -77.63 46.06, -77.62 46.04, -77.61 46.02, -77.60 46.01, -77.60 46.00, -77.59 45.99, -77.59 45.99, -77.59 45.99, -77.57 45.95, -77.57 45.95, -77.55 45.92, -77.53 45.89, -77.53 45.87, -77.47 45.77, -77.47 45.77, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.76, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.47 45.75, -77.48 45.75, -77.48 45.75, -77.48 45.74, -77.48 45.74, -77.48 45.74, -77.48 45.74, -77.48 45.74, -77.51 45.74, -77.54 45.73, -77.58 45.72, -77.61 45.71, -77.65 45.70, -77.66 45.70, -77.67 45.69, -77.68 45.69, -77.72 45.68, -77.74 45.67, -77.77 45.66, -77.79 45.66, -77.82 45.65, -77.82 45.65, -77.83 45.65, -77.83 45.65, -77.83 45.65, -77.83 45.65, -77.83 45.65, -77.83 45.65, -77.84 45.65, -77.84 45.65, -77.84 45.65, -77.88 45.63, -77.91 45.63, -77.91 45.63, -77.93 45.62, -77.94 45.62, -77.96 45.61, -77.97 45.61, -77.97 45.61, -77.97 45.61, -77.97 45.61, -77.98 45.61, -77.98 45.61, -77.98 45.61, -77.98 45.61, -77.98 45.61, -77.98 45.61, -77.99 45.61, -77.99 45.61, -77.99 45.61, -77.99 45.61, -77.99 45.61, -77.99 45.61, -77.99 45.61, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.00 45.62, -78.01 45.63, -78.01 45.63, -78.02 45.63, -78.02 45.63, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.62, -78.02 45.61, -78.02 45.61, -78.02 45.61, -78.02 45.61, -78.02 45.61, -78.02 45.61, -78.03 45.61, -78.03 45.61, -78.03 45.61, -78.03 45.61, -78.03 45.61, -78.03 45.61, -78.27 45.14, -78.27 45.14, -78.27 45.14, -78.27 45.14, -78.03 45.61, -78.04 45.60, -78.04 45.60, -78.05 45.60, -78.05 45.60, -78.06 45.60, -78.06 45.60, -78.06 45.60, -78.06 45.60, -78.07 45.60, -78.08 45.60, -78.08 45.60, -78.09 45.60, -78.09 45.60, -78.10 45.59, -78.10 45.59, -78.13 45.58, -78.17 45.57, -78.17 45.57, -78.17 45.57, -78.18 45.57, -78.18 45.57, -78.19 45.57, -78.19 45.57, -78.19 45.57, -78.19 45.57, -78.19 45.57, -78.20 45.57, -78.21 45.56, -78.22 45.55, -78.22 45.55, -78.22 45.54, -78.22 45.54, -78.22 45.54, -78.22 45.53, -78.22 45.53, -78.23 45.52, -78.23 45.52, -78.23 45.52, -78.23 45.51, -78.23 45.51, -78.24 45.49, -78.24 45.48, -78.24 45.48, -78.23 45.47, -78.23 45.47, -78.22 45.45, -78.22 45.45, -78.22 45.44, -78.21 45.43, -78.20 45.40, -78.19 45.38, -78.17 45.35, -78.15 45.31, -78.14 45.30, -78.13 45.27, -78.12 45.25, -78.11 45.24, -78.11 45.24, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.20, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.09 45.19, -78.10 45.18, -78.10 45.18, -78.10 45.18, -78.10 45.18, -78.10 45.18, -78.10 45.18, -78.11 45.18, -78.15 45.17, -78.15 45.17, -78.17 45.16, -78.17 45.16, -78.22 45.15, -78.22 45.15, -78.27 45.14, -78.27 45.14, -78.28 45.14, -78.28 45.14, -78.28 45.14, -78.28 45.14, -78.28 45.14, -78.28 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.29 45.14, -78.30 45.15, -78.30 45.15, -78.46 45.32, -78.47 45.33, -78.48 45.34, -78.48 45.35, -78.48 45.35, -78.48 45.35, -78.51 45.35, -78.56 45.33, -78.56 45.33, -78.56 45.33, -78.56 45.33, -78.56 45.33, -78.56 45.33, -78.56 45.33, -78.57 45.33, -78.57 45.33, -78.69 45.43, -78.69 45.43, -78.69 45.43, -78.69 45.43, -78.70 45.43, -78.73 45.42, -78.79 45.41, -78.79 45.41, -78.79 45.41, -78.79 45.41, -78.79 45.41, -78.80 45.41, -78.80 45.41, -78.80 45.41, -78.82 45.40, -78.84 45.39, -78.84 45.39, -78.84 45.39, -78.84 45.39, -78.85 45.39, -78.85 45.39, -78.85 45.39, -78.85 45.39, -78.85 45.39, -78.30 45.15, -78.30 45.15, -78.30 45.15, -78.30 45.15, -78.31 45.17, -78.32 45.19, -78.32 45.20, -78.33 45.20, -78.33 45.20, -78.34 45.22, -78.34 45.22, -78.34 45.23, -78.34 45.23, -78.34 45.24, -78.34 45.24, -78.35 45.25, -78.35 45.25, -78.36 45.26, -78.36 45.27, -78.36 45.27, -78.37 45.27, -78.37 45.27, -78.39 45.26, -78.41 45.26, -78.41 45.26, -78.41 45.26, -78.41 45.26, -78.41 45.26, -78.41 45.26, -78.41 45.26, -78.42 45.26, -78.42 45.26, -78.42 45.26, -78.42 45.26, -78.42 45.26, -78.42 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.26, -78.43 45.27, -78.44 45.27, -78.44 45.27, -78.44 45.27, -78.44 45.27, -78.44 45.27, -78.46 45.31, -78.46 45.31, -78.46 45.32, -78.57 45.33, -78.57 45.33, -78.57 45.33, -78.57 45.33, -78.58 45.33, -78.58 45.33, -78.58 45.33, -78.58 45.33, -78.58 45.34, -78.58 45.34, -78.58 45.34, -78.59 45.34, -78.59 45.34, -78.59 45.34, -78.59 45.34, -78.59 45.34, -78.59 45.34, -78.59 45.35, -78.59 45.35, -78.59 45.35, -78.59 45.35, -78.59 45.35, -78.62 45.39, -78.62 45.39, -78.63 45.41, -78.67 45.42, -78.67 45.42, -78.67 45.42, -78.68 45.42, -78.68 45.42, -78.68 45.42, -78.68 45.42, -78.68 45.43, -78.68 45.43, -78.68 45.43, -78.68 45.43, -78.69 45.43, -78.69 45.43, -78.69 45.43, -78.69 45.43, -78.85 45.39, -78.86 45.39, -78.86 45.39, -78.86 45.39, -78.86 45.39, -78.86 45.39, -78.86 45.39, -78.86 45.39, -78.87 45.39, -78.87 45.39, -78.87 45.39, -78.87 45.40, -78.87 45.40, -78.87 45.40, -78.88 45.40, -78.88 45.40, -78.88 45.40, -78.88 45.40, -78.88 45.40, -78.88 45.40, -78.89 45.43, -78.89 45.43, -78.91 45.46, -78.91 45.46, -78.92 45.49, -78.92 45.49, -78.94 45.51, -78.94 45.51, -78.94 45.51, -78.94 45.51, -78.94 45.51, -78.94 45.51, -78.94 45.52, -78.95 45.52, -78.95 45.52, -78.95 45.52, -78.95 45.52, -78.95 45.52, -78.96 45.54, -78.96 45.54, -78.98 45.58, -78.98 45.58, -79.01 45.62, -79.01 45.62, -79.01 45.64, -79.01 45.64, -79.01 45.64, -79.01 45.64, -79.02 45.64, -79.02 45.64, -79.02 45.65, -79.02 45.65, -79.04 45.70, -79.04 45.70, -79.05 45.72, -79.05 45.72, -79.10 45.75, -79.10 45.75, -79.10 45.75, -79.10 45.75, -79.11 45.75, -79.11 45.75, -79.11 45.75, -79.11 45.75, -79.11 45.75, -79.11 45.76, -79.11 45.76, -79.11 45.76, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.11 45.77, -79.10 45.78, -79.10 45.78, -79.10 45.78, -79.10 45.78, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.06 45.72, -79.07 45.72, -79.07 45.72, -79.07 45.72, -79.07 45.72, -79.07 45.73, -79.07 45.73, -79.07 45.73, -79.08 45.74, -79.08 45.74, -79.08 45.74, -79.08 45.74, -79.09 45.74, -79.09 45.74, -79.09 45.74, -79.09 45.74, -79.09 45.74, -79.09 45.74, -79.10 45.74, -79.10 45.74, -79.10 45.74, -79.10 45.78, -79.10 45.78, -79.10 45.78, -79.10 45.78, -79.10 45.78, -79.09 45.79, -79.10 45.80, -79.10 45.80, -79.12 45.84, -79.12 45.84, -79.14 45.88, -79.14 45.88, -79.14 45.88, -79.14 45.88, -79.14 45.88, -79.14 45.88, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.89, -79.14 45.90, -79.14 45.90, -79.14 45.90, -79.14 45.90, -79.13 45.90, -79.13 45.90, -79.13 45.90, -79.13 45.90, -79.13 45.90, -79.13 45.90, -79.13 45.90, -79.12 45.90, -79.13 45.92, -79.14 45.93, -79.16 45.97, -79.16 45.97, -79.18 46.01, -79.18 46.01, -79.18 46.01, -79.18 46.01, -79.18 46.01, -79.18 46.02, -79.18 46.02, -79.18 46.02))"))

(defcontext algonquin-bbox-wgs84
  "Two Algonquin Provincial Park region."
  (grid
   "50 m"
   "EPSG:4326 POLYGON ((-79.18 46.1710058593745,  -77.4700000000009 46.1710058593745, -77.4700000000009 45.138994140625,  -79.18 45.138994140625, -79.18 46.1710058593745))"))

(defcontext lakeofthewoods-wshed2
  "Context 2 from the list of Lake of the Woods watersheds"
  (grid
   "100 m"
   "EPSG:4326 POLYGON ((-94.280040 49.85116, -93.46478 49.85116, -93.46478 49.43051, -94.280040 49.43051, -94.280040 49.85116))"))

(defcontext lakeofthewoods-wshed7
  "Context 7 from the list of Lake of the Woods watersheds"
  (grid
  "75 m"
  "EPSG:4326 POLYGON ((-94.68109 49.005374, -94.165895 49.005374, -94.165895 48.83393, -94.68109  48.83393, -94.68109 49.005374))"))


