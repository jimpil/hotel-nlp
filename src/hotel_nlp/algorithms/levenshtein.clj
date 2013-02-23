(ns hotel_nlp.algorithms.levenshtein
  (:require [hotel_nlp.helper :refer [matrix]]))



(definline dist-step [pred d index m-weight]
 `(let [[i# j#] ~index]
    (assoc ~d ~index
      (cond (zero? (min i# j#))  (max i# j#)
            (~pred i# j#) (get ~d [(dec i#) (dec j#)])
        :else   (min
                     (inc (get ~d [(dec i#) j#])) ;+1 cost for deletion 
                     (inc (get ~d [i# (dec j#)])) ;+1 cost for insertion
                     (+ ~m-weight (get ~d [(dec i#) (dec j#)]))))))) ;+2 cost for substitution usually (per Juramfky's lecture-slides)

       
(defn levenshtein-distance 
 "Calculates the Levenshtein distance between two Strings."
([s1 s2 m-weight]
  (let [m (inc (count s1)) ; do not skip the last character
        n (inc (count s2)) ; do not skip the last character
        pred (fn [i j]  (=
                          (get s1 (dec i))
                          (get s2 (dec j))))
       step #(dist-step pred %1 %2 m-weight)
       distance-matrix (reduce step {} (matrix m n))]        
 (get distance-matrix [(dec m) (dec n)])))
([s1 s2] (levenshtein-distance s1 s2 2))) 
