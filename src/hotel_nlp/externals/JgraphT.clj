(ns hotel_nlp.externals.JgraphT
  (:require [flatland.ordered.set :refer [ordered-set]])
  (:import  [org.jgrapht.graph SimpleDirectedGraph DirectedMultigraph DefaultEdge ClassBasedEdgeFactory]
            [org.jgrapht Graph]
            [hotel_nlp.externals RelationEdge]))


(defn add-vertices! "Add the given vertice-labels [vs] to the graph g."
  [^org.jgrapht.Graph g & vs]
  (doseq [v vs]
    (.addVertex g v)) g)

(defn add-edge! 
([^Graph g [source target] ^DefaultEdge e]
   (.addEdge g source target e) g)
([^Graph g [source target]] 
   (.addEdge g source target) g) )     

(defn add-edges! 
  "Add the given edges [es] to the graph g. Each edge should be a seq with at least 2 elements [from, start]."
  [^Graph g & es]
  (doseq [[from to e] es]
    (if (nil? e) 
      (add-edge! g [from to])
      (add-edge! g [from to] e))) g)

(def dummy ["These" "plants" "shielded" "the" "face" "of" "the" "bank"])
;(def dummy2 ["The" "bioavailability" "of" "oral" "midazolam" "was" "significantly" (P less than 0.05) higher in patients than controls (76% vs. 38%)."])
(def G (SimpleDirectedGraph. RelationEdge))
(apply add-vertices! G dummy)
#_(-> dummy 
    (add-vertices! "drugA" "drugB" "drugC" "and" "inhibits") 
    (add-edges! ["inhibits" "drugA"] ["drugB" "and"]))


;quick example adopted from https://github.com/jgrapht/jgrapht/wiki/LabeledEdges
(defn example []
  (let [[friend enemy] ["friend" "enemy"] 
        [john james sarah jessica :as people] ["John" "James" "Sarah" "Jessica"] 
        multiG (DirectedMultigraph. (ClassBasedEdgeFactory. RelationEdge))]
;;each person is a vertex in our graph   
(apply add-vertices! multiG people)   
;;apparently, John likes everyone        
(doseq [p people]
   (add-edge! multiG [john p (RelationEdge. john p friend)]))        
;;James doesn't really like John        
(add-edge! multiG [james john (RelationEdge. james john enemy)]) 
;;Jessica likes Sarah and James
(add-edge! multiG [jessica james (RelationEdge. jessica james friend)])
(add-edge! multiG [jessica sarah (RelationEdge. jessica sarah friend)])
;But Sarah doesn't really like James
(add-edge! multiG [sarah james (RelationEdge. sarah james enemy)])      
  (doseq [e (.edgeSet multiG)]
   (when (= (str e) "friend") (println (.getSource e) "likes" (.getTarget e))) 
   (when (= (str e) "enemy")  (println (.getSource e) "hates" (.getTarget e))))) )

(defn enju->map [^String fname]   
(with-open [rdr (clojure.java.io/reader fname)]
(-> 
(reduce 
  (fn [m s]
   (if (seq s)
    (let [[predicate predicate-base predicate-POS predicate-base-POS predicate-pos predicate-type relation-label 
           argument argument-base argument-POS argument-base-POS  argument-pos :as cols] 
            (clojure.string/split s #"\t")]
   (assoc m (ordered-set predicate argument)
             {:predicate predicate
              :predicate-base predicate-base 
              :predicate-POS  predicate-POS 
              :predicate-base-POS predicate-base-POS 
              :predicate-pos predicate-pos
              :predicate-type predicate-type
              :relation-label relation-label
              :argument argument
              :argument-base argument-base
              :argument-POS argument-POS
              :argument-base-POS argument-base-POS 
              :argument-pos argument-pos})) m)) 
  {} (line-seq rdr)) 
(dissoc (ordered-set nil "Empty line")))) ) 

(defn map->graph 
([^java.util.Map M ^Graph G]
 (reduce-kv  
   (fn [^Graph g SET v] 
     (let [pred (first SET) arg (second SET)]
      (add-vertices! g pred arg) 
      (add-edge! g [pred arg] (RelationEdge. pred arg (:relation-label v)))))
  G M))
([^java.util.Map M] 
  (map->graph M (SimpleDirectedGraph. RelationEdge))) )

(defn recognised? [^String s]
  (re-find #"concentrations?|midazolam|didanosine|tenofovir|bioavailability|[0-9]{1,2}.([0-9]{1,4})?|(%|per\s?cent)" s))

(defn walk-graph 
([^Graph G] 
  (walk-graph G (.getTarget (some #(when (= "ROOT" (str %)) %) (.edgeSet G)))))  
([^org.jgrapht.Graph G ^String start-vertex-name]
   (let [start (some #(when (= start-vertex-name %) %) (.vertexSet G))
         all-edges (.edgeSet G)
         edges-from-start (.edgesOf G start)
         juxted-fn (juxt #(.getSource %) str #(.getTarget %))] 
    (for [e all-edges :when (or (recognised? (.getSource e)) 
                                (recognised? (.getTarget e))
                                (= start (.getSource e))
                                (= start (.getTarget e)))]   
      (juxted-fn e)))) )


 






