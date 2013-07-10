(ns hotel_nlp.algorithms.optics "OPTICS clustering algorithm"
  (:use  clojure.data.priority-map)
  (:require [clojure.core.reducers :as r]
            [clojure.pprint :refer [pprint]]
            [incanter.core :as icore]
            [incanter.stats :as istat]
            [incanter.charts :as ichart]))
        
(set! *warn-on-reflection* true)        

(defmulti dist  "Euclidian distances between a [point|martrix] and another point"    
  (fn [a _] (class a)))
  
(defmethod dist :incanter.core/matrix [a a1] 
  (if (= (first (icore/dim a)) 1) 
    (dist (icore/to-list a) a1) 
    (map #(dist (icore/to-list %) a1) a)))
    
(defmethod dist :default [a0 a1] 
  (istat/euclidean-distance a0 a1))

(defn- neighbors
"Finds neighbor points in 'a' falling within eps of point with index i. 
The list of neighbors is sorted by their distance from point i."
[a i eps]   
  (into (priority-map)
        (r/filter #(and (< (val %) eps) (not= (key %) i))
                (zipmap (range (count a)) 
                        (dist a (icore/sel a :rows i))))))
                        
    
(defn- core-distance "Determines core distance based on a map of neighbors and min required number of points"
[nbrs min-pts]   
  (if (< (count nbrs) min-pts)  nil 
    (-> nbrs 
      seq 
     (nth (dec min-pts)) 
      second) ))    

(defn- p-enq 
"Enqueue nbrs of a core object into priority queue pq using cd to determine 
reachability distance as max(cd, dist(o,p)). If cd is nil (not a core object), 
then no new elementes are enqueued. Re-enqueues object if it's rd decreased."
[pq nbrs cd]  
  (if-not cd pq
    (let [nbrs-cd (into  {}
            	    (map (juxt key #(if-let [pqv (pq (key %))] 
              			      (min pqv (max cd (val %)))
             			      (max cd (val %))))   nbrs))]
      (into pq nbrs-cd))))
    
;OPTICS outputs the points in a particular ordering, annotated with their smallest reachability distance (:rd) & core-distance (:cd) 
;(http://en.wikipedia.org/wiki/OPTICS_algorithm)
(defn optics 
"OPTICS clustering algorithm. This function expands all available clusters recursively,
 drawing from remaining pt set if local expansion is not possible."
[data eps min-pts]
{:pre [(> (count data) 0) (> eps 0) (> min-pts 0)]}
  (let [rem-all (apply sorted-set (range (count data)))]
    (loop [id (first rem-all)
           rd nil 
           rem (disj rem-all id) 
           pq (priority-map) 
           exp #{id} 
           comm []] 
      (if-not id comm
        (let [nbrs (neighbors data id eps) ; all neighbors
              rem-nbrs (apply dissoc nbrs exp) ; unprocessed neighbors
              cd (core-distance nbrs min-pts) ; core dist
              pq (p-enq pq rem-nbrs cd) ; enqueue any unprocessed neighbors (local expansion)
              pq1 (peek pq) ; dequeue one immediately (nil if pq is empty)
              comm (into comm [{:id id, :cd cd, :rd rd}])]
          ;(println id) (println nbrs) (println rem-nbrs) (println pq)
          (if pq1
            (recur (key pq1) 
                   (val pq1) 
                   (disj rem (key pq1))
                   (pop pq) 
                   (conj exp (key pq1))
                    comm)
            (recur (first rem) 
                   nil 
                   (disj rem (first rem)),
                   pq
                   (if-let [rem1 (first rem)] 
                     (conj exp rem1) exp)
                    comm)))))))
        
;; *** plotting ***

(defn scatter-plot-matrix [m]
  (let [pl (ichart/scatter-plot (icore/sel m :cols 0) (icore/sel m :cols 1))]
    (doseq [[i x y] (mapv #(cons %1 %2) (range (count m)) m)] 
      (ichart/add-text pl x y (str i)))
    (icore/view pl)))

(defn rd-plot "Draw reachability-plot." [opt]
  (icore/view (ichart/xy-plot (range (count opt)) (map :rd opt)))) 
  
(comment   
;; *** test data ***
(def M1 (icore/matrix [[15 70] [31 87] [45 32] [5   8] [73 9]
                       [32 83] [26 50] [7  31] [43 97] [97 9]]))

(def M2 (icore/matrix (concat (istat/sample-normal 500 :sd 0.4) 
                              (istat/sample-normal 500 :mean 1 :sd 0.5)) 2)) 
                              
(rd-plot (optics M1 50 3))
(rd-plot (optics M2 1 5))

                               
)  
  
         
