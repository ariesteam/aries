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

(ns district.kmeans
  (:refer-clojure)
  (:use district.point-algebra
	[district.breadcrumb-agents :only (make-coord-list)]
	[district.worldgen :only (make-world)]))

(defn get-nearest-cluster
  "Return the index of the cluster-center nearest to point."
  [point cluster-centers]
  (apply min-key #(euclidean-distance point %) cluster-centers))

(defn collect-points-by-cluster
  "Return a list of lists grouping all the points in data-matrix by
   their cluster assignments from type-matrix."
  [rows cols cluster-centers data-matrix type-matrix]
  (let [points-by-cluster (take (count cluster-centers) (repeatedly (atom ())))]
    (dotimes [i rows]
	(dotimes [j cols]
	    (swap! (nth points-by-cluster (aget type-matrix i j)) conj (aget data-matrix i j))))
    points-by-cluster))

(defn find-cluster-centers
  "Return a list of new cluster centers by averaging the values
  of all the points in each cluster.

  BEWARE: Degenerate cases may occur in which a cluster has lost all
          of its members.  It's new center will be nil.

  ISODATA Notes: If a cluster-center's membership falls below a chosen
                 threshold (currently 1), then its value shall be set
                 to nil, thus preventing any points from associating
                 with it again.  This is currently working.

  ISODATA: To implement this, follow these rules:
  1) Compute intra-cluster-variances
  2) If variance of a cluster is above max-cluster-var,
     create a new centroid selected from the cluster.
  3) If membership count of a cluster is below min-cluster-membership,
     set centroid to nil (and remove from list).
  4) If two centroids are within min-centroid-distance of each other,
     remove them and replace them with a new centroid created exactly
     between them."
  [points-by-cluster]
  (map average-points points-by-cluster))

(defn make-data-matrix
  "Create a rows-x-cols matrix and assign n-dimensional vector values
   to all elements in matrix, drawing each point from the cluster/type
   mapping in type-matrix."
  [rows cols n type-matrix cluster-centers intra-cluster-stdevs]
  (let [matrix (make-array Double/TYPE rows cols n)]
    (dotimes [i rows]
	(dotimes [j cols]
	    (let [cluster-index (aget type-matrix i j)]
	      (aset matrix i j
		    (make-point n (get cluster-centers cluster-index)
				0.0 (get intra-cluster-stdevs cluster-index))))))
    matrix))

(defn generate-dataset
  "This is a 3-stage process.
   Step 1: Partition true-type-matrix into irregular polygons.
   Step 2: Assign a point value to each partition and store in true-cluster-centers.
   Step 3: Populate artificial-dataset with points drawn from a multi-variate
           normal distribution centered around its assigned type point."
 [rows cols dimensions world-specs inter-cluster-stdev
  intra-cluster-stdevs cluster-shapes]
 (let [true-type-matrix (make-world rows cols world-specs)
       true-cluster-centers (make-point-list (/ (count world-specs) 2)
					     dimensions
					     (zero-point dimensions)
					     0.0 inter-cluster-stdev)
       artificial-dataset (make-data-matrix rows cols dimensions
					    true-type-matrix
					    true-cluster-centers
					    intra-cluster-stdevs)]
   [artificial-dataset true-type-matrix true-cluster-centers]))

(defn select-points-randomly
  "Selects num-points randomly from the dataset."
  [num-points rows cols dataset]
  (loop [coord-list (make-coord-list rows cols num-points)
	 point-list nil]
    (if (empty? coord-list)
      point-list
      (recur (rest coord-list)
	     (cons (aget dataset
			 (first  (first coord-list))
			 (second (first coord-list)))
		   point-list)))))

(defn assign-types-from-data!
  "For each element in data-matrix, calculate the distance between it
   and each of the cluster-centers.  Take the nearest cluster's index
   and assign it as the value for that position in type-matrix."
  [rows cols type-matrix data-matrix cluster-centers]
  (dotimes [i rows]
      (dotimes [j cols]
	  (aset type-matrix i j
		(get-nearest-cluster (aget data-matrix i j)
				     cluster-centers))))
  type-matrix)

(defn run-k-means
  "All of k-means implemented in a single loop construct.  Schweet!
   The first iteration sets up all the data structures, and the rest
   execute the measure-average-move sequence of k-means until the
   cluster-centers are no longer moving.  Returns the found
   type-matrix and cluster-centers."
  [dataset rows cols initial-k]
  (loop [iter 0
	 prev-cluster-centers nil
	 type-matrix (make-array Integer/TYPE rows cols)
	 cluster-centers (select-points-randomly initial-k rows cols dataset)
	 prev-centroid-movement nil
	 centroid-movement nil]
    (if (= cluster-centers prev-cluster-centers)
      [type-matrix cluster-centers iter]
      (recur (inc iter)
	     cluster-centers
	     (assign-types-from-data! rows cols type-matrix dataset cluster-centers)
	     (find-cluster-centers (collect-points-by-cluster rows cols cluster-centers
							      dataset type-matrix))
	     centroid-movement
	     (map (fn [new-point old-point] (euclidean-distance new-point old-point))
		  cluster-centers prev-cluster-centers)))))

(defn multi-run-k-means
  "Run k-means multiple times.  Duh."
  [runs dataset rows cols initial-k]
  (loop [type-matrices    []
	 centroid-sets    []
	 iteration-counts []
	 times-remaining  runs]
    (if (== times-remaining 0)
      [type-matrices centroid-sets iteration-counts]
      (let [[type-matrix cluster-centers iterations]
	      (run-k-means dataset rows cols initial-k)]
	(recur (conj type-matrices type-matrix)
	       (conj centroid-sets cluster-centers)
	       (conj iteration-counts iterations)
	       (dec times-remaining))))))

(defn total-variance
  "FIXME: This assumes the origin point is the n-dimensional mean."
  [dataset rows cols]
  (div-point-by-scalar
   (reduce add-points (for [row (seq dataset) col (seq row)]
			(square-point (seq col))))
   (* rows cols)))

(defn intra-cluster-variance
  [point-list cluster-center]
  (div-point-by-scalar
   (reduce add-points (for [point point-list]
			(square-point (subtract-points point cluster-center))))
   (count point-list)))

(defn intra-cluster-variances
  [rows cols cluster-centers dataset type-matrix]
  (map (fn [point-list cluster-center]
	 (intra-cluster-variance point-list cluster-center))
       (collect-points-by-cluster rows cols cluster-centers dataset type-matrix)
       cluster-centers))

(defn remap-type-matrix
  [type-matrix nearest-centroids rows cols]
  (let [remapped-type-matrix (aclone type-matrix)]
    (dotimes [i rows]
	(dotimes [j cols]
	    (aset remapped-type-matrix i j
		  (get nearest-centroids (aget type-matrix i j)))))
    remapped-type-matrix))

(defn find-type-matrix-errors
  [remapped-type-matrix true-type-matrix rows cols]
  (let [error-matrix (aclone remapped-type-matrix)
	error-count  (count (for [i (range rows) j (range cols)
				  :when (not= (aget remapped-type-matrix i j)
					      (aget true-type-matrix i j))]
			      (aset error-matrix i j \*)))]
    [error-count error-matrix]))

(defn analyze-results
  [type-matrices true-type-matrix centroid-sets
   true-cluster-centers dataset rows cols dimensions runs]
  (loop [within-cluster-variances []
	 nearest-centroids        []
	 remapped-type-matrices   []
	 error-counts             []
	 error-matrices           []
	 i                        0]
    (if (== i runs)
      [(total-variance dataset rows cols dimensions)
       within-cluster-variances nearest-centroids
       remapped-type-matrices error-counts error-matrices]
      (let [wcv (intra-cluster-variances rows cols dimensions
					 (get centroid-sets i)
					 dataset
					 (get type-matrices i))
	    nc (map (fn [point] (get-nearest-cluster point
						     true-cluster-centers))
		    (get centroid-sets i))
	    rtm (remap-type-matrix (get type-matrices i)
				   (get nearest-centroids i)
				   rows cols)
	    [ec em] (find-type-matrix-errors
		     (get remapped-type-matrices i)
		     true-type-matrix rows cols)]
	(recur (conj within-cluster-variances wcv)
	       (conj nearest-centroids nc)
	       (conj remapped-type-matrices rtm)
	       (conj error-counts ec)
	       (conj error-matrices em)
	       (inc i))))))

(defn print-results
  "This simply prints the results to the console. For graphable
   visualizations through gnuplot, use visualize-results."
  [type-matrix true-type-matrix cluster-centers true-cluster-centers
   artificial-dataset total-dataset-variance within-cluster-variances
   nearest-centroids remapped-type-matrix error-count error-matrix
   rows cols iterations]
  (let [titles [[type-matrix "Type Matrix"]
		[true-type-matrix "True Type Matrix"]
		[cluster-centers "Cluster Centers / Type Means"]
		[true-cluster-centers "True Cluster Centers / True Type Means"]
		[remapped-type-matrix "Remapped Type Matrix"]
		[error-matrix "Error Matrix"]]]
    (doseq (matrix (list type-matrix true-type-matrix
			 remapped-type-matrix error-matrix))
	(printf "\n%a:\n\n" (get titles matrix))
        (dotimes [i rows]
	    (dotimes [j cols]
		(printf "%3a" (aget matrix i j)))
	    (newline)))

    (printf "\nError Count: %d\n" error-count)
    (printf "\nPercent Error: %f (%d)\n"
	    (* 100 (/ error-count (* rows cols)))
	    (/ error-count (* rows cols)))

    (println "\nNearest Centroids:")
    (dotimes [i (count nearest-centroids)]
	(printf "  %d : %d\n" i (aget nearest-centroids i)))

    (doseq [centers (list cluster-centers true-cluster-centers)]
      (printf "\n%a:\n\n" (get titles centers))
      (dotimes [i (count centers)]
	  (printf "%3d: %a\n" i (get centers i))))

    (printf "\nTotal Dataset Variance:\n%a\n\n" total-dataset-variance)
    (printf "Within Cluster Variances:\n%a\n\n" within-cluster-variances)
    (printf "Total Iterations: %d\n\n" iterations)
    ;;(printf "Artificial Dataset:\n\n%a\n" artificial-dataset)
    ))

(defn visualize-results
  "This uses nlisp's gnuplot callouts to display the datasets on
   colored 2D/3D graphs."
  [type-matrix true-type-matrix cluster-centers
   true-cluster-centers artificial-dataset
   total-dataset-variance within-cluster-variances
   nearest-centroids remapped-type-matrix error-count
   error-matrix rows cols iterations]
  (println "Visualization does not yet work."))

(defn test-k-means
  "Runs the k-means/ISODATA algorithm within the environment defined by the input parameters:

     rows,cols: Number of rows and columns in the matrices, where the
                points will be stored; rows x cols equals total number
                of points - Ex: 50

     dimensions: Number of spatial dimensions for each point - Ex: 3

     polygons: Number of polygons in which to partition the type
                matrix; should be greater than or equal to initial-k -
                Ex: 6

     types: Number of types to be distributed over the polygons. Ex: 4

     initial-k: Initial guess at number of expected clusters - Ex: 5

     inter-cluster-stdev: Between cluster stdev, used to compute
                          initial distribution of cluster centers from
                          a normal distribution - Ex: 2

     intra-cluster-stdevs: Within cluster stdevs, used to compute
                           initial distribution of points in each
                           cluster; should be a vector of length
                           initial-k.  Ex: #(3.4 1 2 1 7.2)

     cluster-shapes: By specifying the weights for each dimension by
                     cluster, you can alter the geometric shapes of
                     the clusters in hyperspace; should be a vector of
                     vectors, where the first dimension is initial-k and
                     the second is dimensions.
                     Ex: #((1 2 .5) (1 1 1) (.5 1 .5) (3 2 1) (4 0.3 .2))"
  [rows cols dimensions world-specs initial-k
   inter-cluster-stdev intra-cluster-stdevs cluster-shapes runs]
  (println "Generating Artificial Dataset...")
  (let [[artificial-dataset true-type-matrix true-cluster-centers]
	(generate-dataset rows cols dimensions world-specs
			inter-cluster-stdev intra-cluster-stdevs
			cluster-shapes)]
    (printf "Running k-means (%d times)...\n" runs)
    (let [[type-matrices centroid-sets iteration-counts]
	  (multi-run-k-means runs artificial-dataset rows cols initial-k)]
      (println "Analyzing results...")
      (let [[total-dataset-variance within-cluster-variance-sets
	     nearest-centroid-sets remapped-type-matrices
	     error-counts error-matrices]
	    (analyze-results type-matrices true-type-matrix centroid-sets
			     true-cluster-centers artificial-dataset
			     rows cols dimensions runs)]
	(dotimes [i runs]
	    (let [type-matrix (aget type-matrices i)
		  cluster-centers (aget centroid-sets i)
		  within-cluster-variances (aget within-cluster-variance-sets i)
		  nearest-centroids (aget nearest-centroid-sets i)
		  remapped-type-matrix (aget remapped-type-matrices i)
		  error-count (aget error-counts i)
		  error-matrix (aget error-matrices i)
		  iterations (aget iteration-counts i)]
	      (printf "\nPrinting results [%d]...\n" i)
	      (print-results type-matrix true-type-matrix cluster-centers
			     true-cluster-centers artificial-dataset
			     total-dataset-variance within-cluster-variances
			     nearest-centroids remapped-type-matrix error-count
			     error-matrix rows cols iterations)
	      (printf "\nVisualizing results [%d]...\n" i)
	      (visualize-results type-matrix true-type-matrix cluster-centers
				 true-cluster-centers artificial-dataset
				 total-dataset-variance within-cluster-variances
				 nearest-centroids remapped-type-matrix error-count
				 error-matrix rows cols iterations)))))))
