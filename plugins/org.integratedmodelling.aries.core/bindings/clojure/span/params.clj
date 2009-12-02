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

(ns span.params
  (:refer-clojure))

;; FIXME do we ever want both absolute and relative, sources, sinks, or uses?
;; FIXME *source-type* should exist now since it can also be absolute or relative
(def *source-threshold* 0.0)

(def *sink-threshold* 0.0)

(def *use-threshold* 0.0)

(def *trans-threshold* 0.0)

(def *decay-rate* 0.0)

(def *sink-type* nil)

(def *use-type* nil)

(def *benefit-type* nil)

(defn set-global-params!
  [flow-params]
  (alter-var-root #'*source-threshold* (constantly (flow-params :source-threshold)))
  (alter-var-root #'*sink-threshold*   (constantly (flow-params :sink-threshold)))
  (alter-var-root #'*use-threshold*    (constantly (flow-params :use-threshold)))
  (alter-var-root #'*trans-threshold*  (constantly (flow-params :trans-threshold)))
  (alter-var-root #'*decay-rate*       (constantly (flow-params :decay-rate)))
  (alter-var-root #'*sink-type*        (constantly (flow-params :sink-type)))
  (alter-var-root #'*use-type*         (constantly (flow-params :use-type)))
  (alter-var-root #'*benefit-type*     (constantly (flow-params :benefit-type))))
