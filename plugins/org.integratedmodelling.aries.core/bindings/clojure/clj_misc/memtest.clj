;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-misc.
;;;
;;; clj-misc is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-misc is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-misc.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines functions for checking the JVM's current
;;; memory usage and estimating the memory usage for a passed in form.

(ns clj-misc.memtest
  (:import (java.lang.management ManagementFactory MemoryType)))

(defn memory-usage []
  (let [pools (ManagementFactory/getMemoryPoolMXBeans)
        mb (* 1024.0 1024.0)
        step (fn [pools tu tr tm]
               (if (not (seq pools))
                 [(/ tu mb) (/ tr mb) (/ tm mb)]
                 (let [pool (first pools)
                       usage (. pool getUsage)]
                   (recur (rest pools)
                          (+ tu (. usage getUsed))
                          (+ tr (. usage getCommitted))
                          (+ tm (. usage getMax))))))]
    (step (filter #(= (. % getType) MemoryType/HEAP) pools) 0 0 0)))

(defn dump-memory-usage [mem-usage]
  (println (apply format "used: %.2fMB, reserved: %.2fMB, max: %.2fMB" mem-usage)))

(defmacro check-mem [form]
  `(let [mem-before# (memory-usage)
         result#     ~form
         mem-after#  (memory-usage)]
     (dump-memory-usage (map - mem-after# mem-before#))
     result#))
