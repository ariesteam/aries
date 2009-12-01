(ns misc.memtest
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
