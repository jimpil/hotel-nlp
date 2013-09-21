(ns hotel_nlp.algorithms.levenshtein
  (:require [clojure.test :refer [with-test is testing test]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *substitution-cost* 2)  ;+2 cost for substitution usually (per Juramfky's lecture-slides and book) 

(definline dist-step [pred d index m-weight]
 `(let [[i# j# :as idx#] ~index]
    (assoc ~d idx#
      (cond (zero? (min i# j#))  (max i# j#)
            (~pred i# j#) (get ~d [(dec i#) (dec j#)])
        :else   (min
                     (inc (get ~d [(dec i#) j#])) ;+1 cost for deletion 
                     (inc (get ~d [i# (dec j#)])) ;+1 cost for insertion
                     (+ ~m-weight (get ~d [(dec i#) (dec j#)]))))))) 

(with-test       
(defn levenshtein-distance 
 "Calculates the 'Levenshtein-distance' between two Strings using efficient bottom-up dynamic programming. 
  If s1 & s2 are of equal length and m-weight = 1, the 'Hamming-distance' is essentially calculated."
([^String s1 ^String s2 m-weight]
  (let [m (.length s1)
        n (.length s2)
        pred (fn [i j]  
                (=
                  (.charAt s1 (dec i))
                  (.charAt s2 (dec j))))
       step #(dist-step pred %1 %2 m-weight)
       distance-matrix (reduce step {} (for [i (range (inc m)) 
                                             j (range (inc n))] 
                                          [i j]))]        
 (get distance-matrix [m n])))
([^String s1 ^ String s2] 
  (levenshtein-distance s1 s2 *substitution-cost*)))
  
(testing "Levenshtein distance metric on the wikipedia examples"
 (is (= 6  (levenshtein-distance "toned" "roses")))
 (is (= 2  (levenshtein-distance "it" "is")))
 (is (= 1  (levenshtein-distance "it" "is" 1)))
 (is (= 5  (levenshtein-distance "kitten" "sitting")))
 (is (= 3  (levenshtein-distance "kitten" "sitting" 1))) ;;wikipedia actully uses -1 for substitution
 (is (= 26 (levenshtein-distance "abcdefghijklmnopqrstuvwxyz" "")))
(let [alphabet "abcdefghijklmnopqrstuvwxyz"
      ralphabet (->> alphabet reverse (apply str))] 
 (is (= 50 (levenshtein-distance alphabet ralphabet))))
))  

(defn fuzzy-match 
"Fuzzy string matching that scores according to the edit-distance."
([s ss tolerance m-weight]
 (filter #(<= (levenshtein-distance s % m-weight) tolerance) ss))
([s ss tolerance]
 (fuzzy-match s ss tolerance *substitution-cost*)) 
([s ss]
 (fuzzy-match s ss 5)) 
([ss] 
  (fuzzy-match (first ss) (rest ss))) )
 
