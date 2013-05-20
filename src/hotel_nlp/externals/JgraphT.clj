(ns hotel_nlp.externals.JgraphT
  (:import [org.jgrapht.graph SimpleDirectedGraph DirectedMultigraph DefaultEdge ClassBasedEdgeFactory]
           [hotel_nlp.externals RelationEdge]))


(defn add-vertices! "Add the given vertice-labels [vs] to the graph g."
  [^org.jgrapht.Graph g & vs]
  (doseq [v vs]
    (.addVertex g v)) g)

(defn add-edge! [^org.jgrapht.Graph g [source target e]]
  (if (nil? e)
     (.addEdge g source target)
     (.addEdge g source target e)))

(defn add-edges! 
  "Add the given edges [es] to the graph g. Each edge should be a seq with at least 2 elements [from, start]."
  [^org.jgrapht.Graph g & es]
  (doseq [[from to e] es]
    (if (nil? e) (.addEdge g from to)
      (.addEdge g from to e))) g)

(def dummy (SimpleDirectedGraph. RelationEdge))
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
   (when (= (str e) "friend") (println (.getV1 e) "likes" (.getV2 e))) 
   (when (= (str e) "enemy")  (println (.getV1 e) "hates" (.getV2 e))))) )

(defn enju->map [^String fname]   
(with-open [rdr (clojure.java.io/reader fname)]
(reduce 
  (fn [m s]
    (let [[predicate predicate-base predicate-POS predicate-base-POS predicate-pos predicate-type relation-label 
           argument argument-base argument-POS argument-base-POS  argument-pos :as cols] 
            (clojure.string/split s #"\t")
            predicate-word (get cols 0) 
            argument-word (get cols 7)]
   (assoc m #{predicate-word argument-word} 
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
              :argument-pos argument-pos}))) 
  {} (line-seq rdr))) )


