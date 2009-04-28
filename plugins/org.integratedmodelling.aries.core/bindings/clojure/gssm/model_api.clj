;;; Copyright 2009 Gary Johnson
;;;
;;; This file is part of CLJ-GSSM.
;;;
;;; CLJ-GSSM is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; CLJ-GSSM is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with CLJ-GSSM.  If not, see <http://www.gnu.org/licenses/>.

(ns gssm.model-api
  (:refer-clojure))

(defmulti
  #^{:doc "Service-specific flow distribution functions."}
  distribute-flow!
  (fn [flow-concept flow-params location-map rows cols]
    (.getLocalName flow-concept)))

(defmethod distribute-flow! :default
  [flow-concept _ _ _ _]
  (throw (Exception. (str "Flow type '" (.getLocalName flow-concept) "' is unrecognized."))))

(defstruct service-carrier :weight :route)

(defn distribute-load-over-processors
  [action-fn arg-seq]
  (let [num-processors (.availableProcessors (Runtime/getRuntime))
	agents (map agent (replicate (* 2 num-processors) nil))]
    (map #(send %1 action-fn %2) (cycle agents) arg-seq)
    (apply await agents)))
