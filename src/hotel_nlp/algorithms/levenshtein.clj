(ns hotel_nlp.algorithms.levenshtein
  (:require [hotel_nlp.helper :refer [matrix]]))

(def ^:dynamic *substitution-cost* 2)  ;+2 cost for substitution usually (per Juramfky's lecture-slides and book) 

(definline dist-step [pred d index m-weight]
 `(let [[i# j#] ~index]
    (assoc ~d ~index
      (cond (zero? (min i# j#))  (max i# j#)
            (~pred i# j#) (get ~d [(dec i#) (dec j#)])
        :else   (min
                     (inc (get ~d [(dec i#) j#])) ;+1 cost for deletion 
                     (inc (get ~d [i# (dec j#)])) ;+1 cost for insertion
                     (+ ~m-weight (get ~d [(dec i#) (dec j#)]))))))) 

       
(defn levenshtein-distance 
 "Calculates the Levenshtein-distance between two Strings."
([s1 s2 m-weight]
  (let [m (count s1)
        n (count s2)
        pred (fn [i j]  
                (=
                  (get s1 (dec i))
                  (get s2 (dec j))))
       step #(dist-step pred %1 %2 m-weight)
       distance-matrix (reduce step {} (matrix m n))]        
 (get distance-matrix [m n])))
([s1 s2] 
  (levenshtein-distance s1 s2 *substitution-cost*)))

(defn fuzzy-match "Fuzzy string matching that scores according to the edit-distance."
([s ss tolerance m-weight]
 (filter #(<= (levenshtein-distance s % m-weight) tolerance) ss))
([s ss tolerance]
 (fuzzy-match s ss tolerance *substitution-cost*)) 
([s ss]
 (fuzzy-match s ss 5)) 
([ss] 
  (fuzzy-match (first ss) (rest ss))) )
