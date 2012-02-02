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

(ns core.contexts.la_antigua
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext la_antigua1024
  ""
  (grid
   1024
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))

(defcontext la_antigua800
  ""
  (grid
   800
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))

(defcontext la_antigua512
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-97.2746372601786 19.142240226562222, -96.27930746109703 19.142240226562222, -96.27930746109703 19.576122241899572, -97.2746372601786 19.576122241899572,-97.2746372601786 19.142240226562222))"))