(defn neighbors-and-slopes
  "Return a map of {point -> slope} for all neighbors."
  [point maps]
  (let [[i j] point
	elev (maps :elevation)
	rows (count elev)
	cols (count (elev 0))
	here-3d (conj point (get-in elev [i j]))
	neighbors (get-neighbors point rows cols)
	neighbors-3d (map (fn [[i j :as p]] (conj p (get-in elev [i j]))) neighbors)]
    (zipmap neighbors (map #(slope-between here-3d %) neighbors-3d))))

(defn avg-abs-slope
  [point maps]
  (let [nas (neighbors-and-slopes point maps)]
    (if (seq nas)
      (/ (reduce + (map #(Math/abs (val %)) nas)) (count nas))
      0)))

(defn absorptiveness
  [point maps]
  (let [[i j] point
	lulc (get-in maps [:lulc i j])]
    (cond (= lulc \E) 0.5
	  (= lulc \F) 0.4
	  (= lulc \A) 0.2
	  (= lulc \W) 0.0
	  (= lulc \P) 0.0)))

(defn pathfinder
  "Return a map of points to which the asset may travel from this
   location in one step and the distribution of the remaining flow
   over them.  Return nil if no points are reachable from here."
  [point maps unsunk-prob]
  (let [downhill-nas (filter #(<= (val %) 0) (neighbors-and-slopes point maps))]
    (when downhill-nas
      (let [slopes (vals downhill-nas)
	    mu (mean slopes)
	    sigma (stdev slopes)]
	(loop [keyvals (seq downhill-nas)
	       norm-slopes {}]
	  (if keyvals
	    (let [[pt slope] (first keyvals)]
	      (recur (rest keyvals)
		     (assoc norm-slopes pt (normalize slope mu sigma))))
	    norm-slopes))))))

(defn source-val
  "Look this up in a static map above.  In the future, read in from a
   user-specified map or compute from the local feature vector."
  [point maps]
  (get-in maps (cons :rainfall point)))

(defn sink-val
  "Return this location's water absorption ability [0.0,1.0].
   Absorptiveness is weighted inversely by average neighboring slope."
  [point maps]
  (let [abs (absorptiveness point maps)
	aas (avg-abs-slope point maps)]
    (* abs (Math/pow 2 (- aas)))))
