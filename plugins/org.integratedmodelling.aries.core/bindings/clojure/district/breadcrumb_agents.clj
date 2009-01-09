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

(ns district.breadcrumb-agents
  (:refer-clojure :exclude (agent))
  (:use [district.utils :only (contains-item?)]
	[district.matrix-ops :only (in-bounds?)]))

(def #^{:private true} *directions* [:n :e :s :w])
(def #^{:private true} *num-directions* (count *directions*))

(defn- rand-direction
  "Select a random value from *directions*."
  []
  (get *directions* (rand-int *num-directions*)))

(defstruct agent :agent-id :agent-type :location
                 :orientation :left-prob :right-prob
                 :fwd-prob :wait-prob)

(defn make-agent
  "Creates an agent (which is a struct-map) whose action probabilities
   are determined by its type."
  [agent-id agent-type starting-location]
  (assert (contains-item? [:water :wetland :forest :farm :urban] agent-type))
  (let [new-agent (struct-map agent
                    :agent-id agent-id
                    :agent-type agent-type
                    :location starting-location
                    :orientation (rand-direction))]
    (cond
     (= agent-type :water)   (-> new-agent
                                 (assoc :left-prob  0.25)
                                 (assoc :right-prob 0.25)
                                 (assoc :fwd-prob   0.5)
                                 (assoc :wait-prob  0.0))
     (= agent-type :wetland) (-> new-agent
                                 (assoc :left-prob  0.3)
                                 (assoc :right-prob 0.0)
                                 (assoc :fwd-prob   0.5)
                                 (assoc :wait-prob  0.2))
     (= agent-type :forest)  (-> new-agent
                                 (assoc :left-prob  0.1)
                                 (assoc :right-prob 0.3)
                                 (assoc :fwd-prob   0.6)
                                 (assoc :wait-prob  0.0))
     (= agent-type :farm)    (-> new-agent
                                 (assoc :left-prob  0.3)
                                 (assoc :right-prob 0.1)
                                 (assoc :fwd-prob   0.6)
                                 (assoc :wait-prob  0.0))
     (= agent-type :urban)   (-> new-agent
                                 (assoc :left-prob  0.25)
                                 (assoc :right-prob 0.25)
                                 (assoc :fwd-prob   0.5)
                                 (assoc :wait-prob  0.0)))))

(defn make-agent-list
  "Generate a list of agents to populate our type-matrix, where each
   will begin at one of the given starting points."
  [agent-types starting-points]
  (assert (== (count agent-types) (count starting-points)))
  (map (fn [type pt id] (make-agent id type pt))
       agent-types starting-points (range (count starting-points))))

(defn make-coord-list
  "Return a list of (x,y) points."
  [rows cols num-pairs]
  (loop [r rows
	 c cols
	 n num-pairs
	 coords ()]
    (if (== n 0)
      coords
      (let [pt [(rand-int r) (rand-int c)]]
	(if (contains-item? coords pt)
	  (recur r c n coords)
	  (recur r c (dec n) (cons pt coords)))))))

(defn unvisited-location?
  "Return true if the coordinates of location are in-bounds and its
   value is nil."
  [matrix i j bounds]
  (and (in-bounds? [i j] bounds)
       (nil? (aget matrix i j))))

(defn agent-stuck?
  "Return true if all neighboring cells are non-nil, false otherwise."
  [agent matrix rows cols]
  (let [[row col] (:location agent)
        bounds [[0 rows][0 cols]]]
    (not (some (fn [[i j]] (unvisited-location? matrix i j bounds))
               (map #(vector %1 %2)
                    [(dec row) (inc row) row row]
                    [col col (dec col) (inc col)])))))

(defn select-next-action
  "Select the agent's next action using its cumulative probability
   density function (CDF)."
  [agent]
  (let [dice-roll (rand 1.0)
        l (:left-prob agent)
        r (:right-prob agent)
        f (:fwd-prob agent)]
    (cond (< dice-roll l)         :left
          (< dice-roll (+ l r))   :right
          (< dice-roll (+ l r f)) :forward
          :otherwise              :wait)))

(defn turn
  [agent direction]
  (let [cycle-dir (partial reduce (fn [x y]
                                    (cond (= x y) nil
                                          (= x nil) y
                                          :otherwise x))
                           (:orientation agent))]
    (assoc agent :orientation
	   (if (= direction :right)
	     (or (cycle-dir *directions*) (first *directions*))
	     (or (cycle-dir (reverse *directions*)) (last *directions*))))))

(defn- shift-row
  "Adjust row value by 1 step in the direction of orient."
  [row orient]
  (cond (= orient :n) (inc row)
	(= orient :s) (dec row)
	:otherwise    row))

(defn- shift-col
  "Adjust col value by 1 step in the direction of orient."
  [col orient]
  (cond (= orient :e) (inc col)
	(= orient :w) (dec col)
	:otherwise    col))

(defn move-forward
  "Try to move forward in the direction the agent is facing.  If this
   fails, turn left and try again.  Repeat until either a valid move
   is made or all the directions have been checked.  If this happens,
   return the agent's state unchanged.  Otherwise, return the new
   agent state after the move."
  [agent matrix rows cols]
  (if (agent-stuck? agent matrix rows cols)
    agent
    (let [[row col] (:location agent)
	  bounds [[0 rows][0 cols]]
	  orient (:orientation agent)
	  i (shift-row row orient)
	  j (shift-col col orient)]
      (if (unvisited-location? matrix i j bounds)
	(assoc agent :location [i j])
	(move-forward (turn agent :left) matrix rows cols)))))

(defn drop-breadcrumb!
  "Make the agent mark its current location in the matrix with its
   agent-id.  Destructively modifies the passed in matrix."
  [agent matrix]
  (let [[i j] (:location agent)]
    (aset matrix i j (:agent-type agent))))

(defn perform-next-action!
  "The agent rolls a die to determine which of its 4 actions to take next.
   1. Turning left or right will cause a change in its orientation only.
   2. Waiting will have no effect on the world (or agent) state as it represents
      forfeiting a turn.
   3. Stepping forward will move the agent into the square on the matrix
      that it is currently facing, provided that no breadcrumbs have previously
      been dropped there.
   X. Upon completion of the action, this function will return the new
      state of the agent."
  [agent matrix rows cols]
  (let [action (select-next-action agent)
        new-agent (cond (= action :left)    (turn agent :left)
                        (= action :right)   (turn agent :right)
                        (= action :forward) (move-forward agent matrix rows cols)
                        (= action :wait)    agent)]
    (drop-breadcrumb! new-agent matrix)
    new-agent))
