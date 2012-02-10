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

(ns core.contexts.dominican
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext DR256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-71.7 19.9, -70.5 19.9, -70.5 18.85, -71.7 18.85, -71.7 19.9))"))

(defcontext DR512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-71.7 19.9, -70.5 19.9, -70.5 18.85, -71.7 18.85, -71.7 19.9))"))