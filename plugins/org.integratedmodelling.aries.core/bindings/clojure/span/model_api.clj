;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-SPAN.
;;;
;;; CLJ-SPAN is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-SPAN is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-SPAN.  If not, see <http://www.gnu.org/licenses/>.

(ns span.model-api)

(defmulti distribute-flow!
  ;;"Service-specific flow distribution functions."
  (fn [flow-concept-name location-map rows cols] flow-concept-name))

(defmethod distribute-flow! :default
  [flow-concept-name _ _ _]
  (throw (Exception. (str "distribute-flow! is undefined for flow type: " flow-concept-name))))

(defmulti decay
  ;;"Service-specific decay functions."
  (fn [flow-concept-name weight steps] flow-concept-name))

(defmethod decay :default [_ weight _] weight)

(defmulti undecay
  ;;"Service-specific inverse decay functions."
  (fn [flow-concept-name weight steps] flow-concept-name))

(defmethod undecay :default [_ weight _] weight)

(defstruct service-carrier :weight :route)

(defn distribute-load-over-processors
  [action-fn arg-seq]
  (let [num-processors (.availableProcessors (Runtime/getRuntime))
	agents (map agent (replicate (+ 2 num-processors) ()))]
    (println "Sending Tasks to" (count agents) "Agents...")
    (dorun (map #(send %1 action-fn %2) (cycle agents) arg-seq))
    (println "Waiting for Agents to Finish...")
    (apply await agents)))
