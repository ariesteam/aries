;;; Copyright 2010 Gary Johnson
;;;
;;; This file is part of clj-span.
;;;
;;; clj-span is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-span is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-span.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace defines functions for displaying matrices as color
;;; images. The run-animation and end-animation functions provide a
;;; somewhat ad-hoc toolkit for simple animations.

(ns clj-span.gui
  (:use [clj-misc.utils      :only (&)]
        [clj-misc.matrix-ops :only (get-rows
                                    get-cols
                                    map-matrix
                                    make-matrix
                                    matrix-min
                                    matrix-max
                                    normalize-matrix)])
  (:require (clj-misc [numbers :as nb] [varprop :as vp] [randvars :as rv]))
  (:import (java.awt Color Graphics Dimension)
           (java.awt.image BufferedImage)
           (javax.swing JPanel JFrame)
           (javax.imageio ImageIO)
           (java.io IOException File)))

(defn fill-cell [#^Graphics g x y scale color]
  (doto g
    (.setColor color)
    (.fillRect (* x scale) (* y scale) scale scale)))

;; (defn get-cell-color [mean stdev]
;;   (let [r (float mean)
;;         g (float (- 1.0 mean))
;;         b (float 0.0)
;;         a (float (- 1.0 stdev))]
;;     (Color. r g b a)))

(defn get-cell-color [percentage] ; [0-1]
  (let [h (float (- 0.7 (* percentage 0.7))) ; blue to red
        s (float 1.0)
        b (float 1.0)]
    (Color/getHSBColor h s b)))

(def #^{:dynamic true} *legend-color-height* 20) ; pixels
(def #^{:dynamic true} *legend-text-height*  10) ; pixels
(def #^{:dynamic true} *legend-padding*       5) ; pixels

(defn render [layer scale x-dim y-dim rv-to-number]
  (let [numeric-layer    (map-matrix rv-to-number layer)
        min-layer-value  (matrix-min numeric-layer 0.0)
        max-layer-value  (matrix-max numeric-layer)
        normalized-layer (normalize-matrix numeric-layer)
        img              (BufferedImage. (* scale x-dim)
                                         (+ (* scale y-dim) *legend-color-height* *legend-text-height* (* *legend-padding* 3))
                                         BufferedImage/TYPE_INT_ARGB)
        bg               (.getGraphics img)]
    ;; Set background color to white
    (doto bg
      (.setColor Color/WHITE)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    ;; Draw map image
    (doseq [x (range x-dim)]
      (doseq [y (range y-dim)]
        (let [percentage (get-in normalized-layer [y x])]
          (if-not (zero? percentage)
            (fill-cell bg x (- y-dim y 1) scale (get-cell-color percentage))))))
    ;; Draw color legend
    (doseq [x (range *legend-padding* (- (.getWidth img) *legend-padding*))] ; add whitespace padding on left and right of legend
      (let [cell-color (get-cell-color (/ (- x *legend-padding*) (- (.getWidth img) (* 2 *legend-padding*))))] ; ranges from [0-1]
        (doseq [y (range *legend-color-height*)]
          (fill-cell bg x (+ (* scale y-dim) *legend-padding* y) 1 cell-color))))
    ;; Add legend text
    (let [metrics        (.getFontMetrics bg)
          min-val-string (format "Min: %.1f" min-layer-value)
          min-val-width  (.stringWidth metrics min-val-string)
          max-val-string (format "Max: %.1f" max-layer-value)
          max-val-width  (.stringWidth metrics max-val-string)]
      (doto bg
        (.setColor (get-cell-color (if-not (zero? max-layer-value) (/ min-layer-value max-layer-value) 1.0)))
        (.drawString min-val-string *legend-padding* (- (.getHeight img) *legend-padding*))
        (.setColor (get-cell-color 1.0))
        (.drawString max-val-string (- (.getWidth img) *legend-padding* max-val-width) (- (.getHeight img) *legend-padding*))))
    ;; Dispose of Graphics context and return BufferedImage
    (.dispose bg)
    img))

(defn write-layer-to-file [dirname file-prefix layer scale value-type]
  (let [[rv-mean rv-stdev] (cond
                            (= value-type :numbers)  [nb/rv-mean nb/rv-stdev]
                            (= value-type :varprop)  [vp/rv-mean vp/rv-stdev]
                            (= value-type :randvars) [rv/rv-mean rv/rv-stdev])
        y-dim (get-rows layer)
        x-dim (get-cols layer)]
    (let [outfile (File. dirname (str file-prefix "-mean.png"))]
      (try (ImageIO/write (render layer scale x-dim y-dim rv-mean) "png" outfile)
           (catch IOException e (println "Failed to write mean layer for" file-prefix "to file" (.getName outfile)))))
    (if-not (= value-type :numbers)
      (let [outfile (File. dirname (str file-prefix "-stdev.png"))]
        (try (ImageIO/write (render layer scale x-dim y-dim rv-stdev) "png" outfile)
             (catch IOException e (println "Failed to write stdev layer for" file-prefix "to file" (.getName outfile))))))))

(defn draw-layer [title layer scale value-type]
  (let [[rv-mean rv-stdev] (cond
                            (= value-type :numbers)  [nb/rv-mean nb/rv-stdev]
                            (= value-type :varprop)  [vp/rv-mean vp/rv-stdev]
                            (= value-type :randvars) [rv/rv-mean rv/rv-stdev])
        y-dim       (get-rows layer)
        x-dim       (get-cols layer)
        mean-panel  (doto (proxy [JPanel] [] (paint [g] (let [img (render layer scale x-dim y-dim rv-mean)]
                                                          (.drawImage g img 0 0 nil))))
                      (.setPreferredSize (Dimension. (* scale x-dim)
                                                     (+ (* scale y-dim)
                                                        *legend-color-height*
                                                        *legend-text-height*
                                                        (* 3 *legend-padding*)))))
        stdev-panel (doto (proxy [JPanel] [] (paint [g] (let [img (render layer scale x-dim y-dim rv-stdev)]
                                                          (.drawImage g img 0 0 nil))))
                      (.setPreferredSize (Dimension. (* scale x-dim)
                                                     (+ (* scale y-dim)
                                                        *legend-color-height*
                                                        *legend-text-height*
                                                        (* 3 *legend-padding*)))))]
    (doto (JFrame. (str title " Mean")) (.add mean-panel) .pack .show)
    (if-not (= value-type :numbers)
      (doto (JFrame. (str title " Standard Deviation")) (.add stdev-panel) .pack .show))
    [mean-panel stdev-panel]))

(defn draw-ref-layer [title ref-layer scale value-type]
  (let [[rv-mean rv-stdev] (cond
                            (= value-type :numbers)  [nb/rv-mean nb/rv-stdev]
                            (= value-type :varprop)  [vp/rv-mean vp/rv-stdev]
                            (= value-type :randvars) [rv/rv-mean rv/rv-stdev])
        y-dim       (get-rows ref-layer)
        x-dim       (get-cols ref-layer)
        mean-panel  (doto (proxy [JPanel] [] (paint [g] (let [layer (map-matrix deref ref-layer)
                                                              img   (render layer scale x-dim y-dim rv-mean)]
                                                          (.drawImage g img 0 0 nil))))
                      (.setPreferredSize (Dimension. (* scale x-dim)
                                                     (+ (* scale y-dim)
                                                        *legend-color-height*
                                                        *legend-text-height*
                                                        (* 3 *legend-padding*)))))
        stdev-panel (doto (proxy [JPanel] [] (paint [g] (let [layer (map-matrix deref ref-layer)
                                                              img   (render layer scale x-dim y-dim rv-stdev)]
                                                          (.drawImage g img 0 0 nil))))
                      (.setPreferredSize (Dimension. (* scale x-dim)
                                                     (+ (* scale y-dim)
                                                        *legend-color-height*
                                                        *legend-text-height*
                                                        (* 3 *legend-padding*)))))]
    (doto (JFrame. (str title " Mean")) (.add mean-panel) .pack .show)
    (if-not (= value-type :numbers)
      (doto (JFrame. (str title " Standard Deviation")) (.add stdev-panel) .pack .show))
    [mean-panel stdev-panel]))

(defn draw-points [ids scale value-type]
  (let [[_+ _0_]    (cond
                     (= value-type :numbers)  [nb/_+ nb/_0_]
                     (= value-type :varprop)  [vp/_+ vp/_0_]
                     (= value-type :randvars) [rv/_+ rv/_0_])
        max-y       (apply max (map first  ids))
        max-x       (apply max (map second ids))
        point-vals  (zipmap ids (repeat (_+ _0_ 1.0)))
        point-layer (make-matrix (inc max-y) (inc max-x) #(get point-vals % _0_))]
    (draw-layer "Points" point-layer scale value-type)))

(def #^{:dynamic true} *animation-sleep-ms* 100)

(defn run-animation [[mean-panel stdev-panel]]
  (send-off *agent* run-animation)
  (Thread/sleep *animation-sleep-ms*)
  [(doto mean-panel  (.repaint))
   (doto stdev-panel (.repaint))])

(defn end-animation [[mean-panel stdev-panel]] [mean-panel stdev-panel])
