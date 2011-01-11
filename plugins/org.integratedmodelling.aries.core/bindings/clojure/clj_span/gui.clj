(ns clj-span.gui
  (:use [clj-misc.utils      :only (&)]
        [clj-misc.randvars   :only (rv-mean)]
        [clj-misc.matrix-ops :only (get-rows
                                    get-cols
                                    map-matrix
                                    normalize-matrix)])
  (:import (java.awt Color Graphics Dimension)
           (java.awt.image BufferedImage)
           (javax.swing JPanel JFrame)))

(defn fill-cell [#^Graphics g x y scale color]
  (doto g
    (.setColor color)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn get-cell-color [type alpha]
  (condp = type
      :source         (Color. 255   0   0 (int (* 255 alpha)))
      :sink           (Color.   0 255   0 (int (* 255 alpha)))
      :use            (Color.   0   0 255 (int (* 255 alpha)))
      :possible-flow  (Color. 255   0 255 (int (* 255 alpha)))
      :actual-flow    (Color.   0 255 255 (int (* 255 alpha)))))

(defn render [g layer type scale x-dim y-dim]
  (let [normalized-layer (normalize-matrix (map-matrix (& rv-mean deref) layer))
        img              (BufferedImage. (* scale x-dim) (* scale y-dim) BufferedImage/TYPE_INT_ARGB)
        bg               (.getGraphics img)]
    (doto bg
      (.setColor Color/WHITE)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (doseq [x (range x-dim)]
      (doseq [y (range y-dim)]
        (fill-cell bg x (- y-dim y 1) scale (get-cell-color type (get-in normalized-layer [y x])))))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(defn draw-layer [title layer type scale]
  (let [y-dim (get-rows layer)
        x-dim (get-cols layer)
        panel (doto (proxy [JPanel] [] (paint [g] (render g layer type scale x-dim y-dim)))
                (.setPreferredSize (Dimension. (* scale x-dim) (* scale y-dim))))]
    (doto (JFrame. title) (.add panel) .pack .show)
    panel))
