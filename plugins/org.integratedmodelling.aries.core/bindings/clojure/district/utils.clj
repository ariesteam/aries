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

(ns district.utils
  (:refer-clojure))

(defn multi-conj
  "Conjoins an element multiple times onto a base-coll."
  [element times base-coll]
  (nth (iterate #(conj % element) base-coll) times))

(defn expand-runtime-encoded-vector
  "Expands a vector of the form [:foo 2 :bar 1 :baz 3] into
   [:foo :foo :bar :baz :baz :baz]."
  [avec]
  (assert (even? (count avec)))
  (loop [orig-vec avec
	 expanded-vec []]
    (if (empty? orig-vec)
      expanded-vec
      (recur (drop 2 orig-vec)
	     (multi-conj (first orig-vec) (second orig-vec) expanded-vec)))))

(defn contains-item?
  "Returns true if sequence contains item.  Otherwise nil."
  [sequence item]
  (some #(= % item) sequence))

(defn breadth-first-search
  "The classic breadth-first-search.  Bread and butter of computer
   science.  Implemented using tail recursion, of course! ;)"
  [open-list closed-list successors goal?]
  (when-first [this-node open-list]
      (if (contains-item? closed-list this-node)
	(breadth-first-search (rest open-list) closed-list successors goal?)
	(if (goal? this-node)
	  this-node
	  (breadth-first-search (concat (rest open-list)
					(filter #(not (contains-item? closed-list %))
						(successors this-node)))
				(cons this-node closed-list)
				successors
				goal?)))))
