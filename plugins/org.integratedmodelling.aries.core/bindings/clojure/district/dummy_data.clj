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

(ns district.dummy-data
  (:refer-clojure)
  (:use [district.utils :only (maphash linearize)]))

(defn make-feature-map
  [feature-matrices]
  (maphash identity linearize feature-matrices))

(def cl-maps
 {:water [[0 0 1 0 0 0 0 0 0 0]
	  [0 0 1 0 0 0 0 0 0 0]
	  [0 0 1 0 0 0 0 0 0 0]
	  [0 0 1 0 0 0 0 0 0 0]
	  [0 0 0 1 0 0 0 0 0 0]
	  [0 0 0 0 1 0 0 0 0 0]
	  [0 0 0 0 0 1 0 0 0 0]
	  [0 0 0 0 0 1 1 0 0 0]
	  [0 0 0 0 0 1 1 1 0 0]
	  [0 0 0 0 1 1 1 1 1 1]]

  :elevation [[1 1 0 1 1 2 2 3 4 4]
	      [1 1 0 1 1 2 2 3 4 4]
	      [1 1 0 1 1 2 2 3 3 4]
	      [1 1 0 1 1 1 2 2 3 3]
	      [1 1 1 0 1 1 2 2 2 3]
	      [2 1 1 1 0 1 1 1 2 2]
	      [2 2 1 1 1 0 1 1 1 1]
	      [3 2 2 1 1 0 0 1 1 1]
	      [3 2 2 1 1 0 0 0 1 1]
	      [3 2 1 1 0 0 0 0 0 0]]

  :lulc [[\A \A \W \E \E \E \A \A \A \A]
	 [\A \A \W \F \E \F \F \A \A \A]
	 [\P \P \P \P \F \F \F \F \F \A]
	 [\F \F \W \P \F \P \P \P \P \P]
	 [\F \F \F \P \P \P \F \F \F \F]
	 [\A \A \A \P \W \F \F \F \F \F]
	 [\A \A \A \P \E \W \E \E \F \F]
	 [\A \A \A \P \E \W \W \E \E \E]
	 [\A \A \P \E \E \W \W \W \E \E]
	 [\P \P \P \E \W \W \W \W \W \W]]

  :population-density [[1 1 0 0 0 0 0 1 1 1]
		       [1 0 0 0 0 0 0 0 1 1]
		       [0 0 0 0 0 0 0 0 0 0]
		       [0 0 0 0 0 0 0 0 0 0]
		       [0 0 0 0 0 0 2 2 2 2]
		       [1 0 0 0 0 0 2 2 2 2]
		       [1 1 0 0 0 0 2 2 2 2]
		       [1 1 0 0 0 0 0 0 2 2]
		       [1 0 0 0 0 0 0 0 0 0]
		       [0 0 0 0 0 0 0 0 0 0]]

  :farms [[1 1 0 0 0 0 1 1 1 1]
	  [1 1 0 0 0 0 0 1 1 1]
	  [0 0 0 0 0 0 0 0 0 1]
	  [0 0 0 0 0 0 0 0 0 0]
	  [0 0 0 0 0 0 0 0 0 0]
	  [1 1 1 0 0 0 0 0 0 0]
	  [1 1 1 0 0 0 0 0 0 0]
	  [1 1 1 0 0 0 0 0 0 0]
	  [1 1 0 0 0 0 0 0 0 0]
	  [0 0 0 0 0 0 0 0 0 0]]

  :roads [[0 0 0 0 0 0 0 0 0 0]
	  [0 0 0 0 0 0 0 0 0 0]
	  [1 1 1 1 0 0 0 0 0 0]
	  [0 0 0 1 0 1 1 1 1 1]
	  [0 0 0 1 1 1 0 0 0 0]
	  [0 0 0 1 0 0 0 0 0 0]
	  [0 0 0 1 0 0 0 0 0 0]
	  [0 0 0 1 0 0 0 0 0 0]
	  [0 0 1 0 0 0 0 0 0 0]
	  [1 1 1 0 0 0 0 0 0 0]]})
