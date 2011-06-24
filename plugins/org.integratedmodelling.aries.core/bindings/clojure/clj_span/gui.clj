(ns clj-span.gui
  (:use [clj-misc.utils      :only (&)]
        [clj-misc.varprop    :only (rv-mean _+ _0_)]
        [clj-misc.matrix-ops :only (get-rows
                                    get-cols
                                    map-matrix
                                    make-matrix
                                    normalize-matrix)])
  (:import (java.awt Color Graphics Dimension)
           (java.awt.image BufferedImage)
           (javax.swing JPanel JFrame)))

(defn fill-cell [#^Graphics g x y scale color]
  (doto g
    (.setColor color)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn get-cell-color [type alpha]
  (cond (= type :source) (Color. 255   0   0 (int (* 255.0 alpha)))
        (= type :sink)   (Color.   0 255   0 (int (* 255.0 alpha)))
        (= type :use)    (Color.   0   0 255 (int (* 255.0 alpha)))
        (= type :flow)   (Color. 255   0 255 (int (* 255.0 alpha)))
        (= type :pflow)  (Color.   0 255 255 (int (* 255.0 alpha)))
        (= type :aflow)  (Color. 255 255   0 (int (* 255.0 alpha)))
        (= type :gray)   (let [val (int (* 255.0 (- 1.0 alpha)))] (Color. val val val 255))))

;; FIXME: This is really slow. Speed it up.
(defn render [g layer type scale x-dim y-dim]
  (let [normalized-layer (normalize-matrix (map-matrix rv-mean layer))
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

(defn draw-ref-layer [title ref-layer type scale]
  (let [y-dim (get-rows ref-layer)
        x-dim (get-cols ref-layer)
        panel (doto (proxy [JPanel] [] (paint [g] (let [layer (map-matrix deref ref-layer)]
                                                    (render g layer type scale x-dim y-dim))))
                (.setPreferredSize (Dimension. (* scale x-dim) (* scale y-dim))))]
    (doto (JFrame. title) (.add panel) .pack .show)
    panel))

(defn draw-points [ids type scale]
  (let [max-y       (apply max (map first  ids))
        max-x       (apply max (map second ids))
        point-vals  (zipmap ids (repeat (_+ _0_ 1.0)))
        point-layer (make-matrix (inc max-y) (inc max-x) #(get point-vals % _0_))]
    (draw-layer "Points" point-layer type scale)))

(def *animation-sleep-ms* 100)

(defn run-animation [panel]
  (send-off *agent* run-animation)
  (Thread/sleep *animation-sleep-ms*)
  (doto panel (.repaint)))

(defn end-animation [panel] panel)
