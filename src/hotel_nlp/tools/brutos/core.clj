(ns hotel_nlp.tools.brutos.core
   (:require [clojure.pprint :refer [pprint]]
             [hotel_nlp.helper    :as help]
             [clojure.math.combinatorics :as combi] 
             [clojure.core.reducers :as r]
             #_[clojure.test :refer [with-test is testing run-tests]]) )
             
(def token-types
 {:numbers (->> 10 range (apply str) seq) 
  :letters {:upper (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            :lower (seq "abcdefghijklmnopqrstuvwxyz")} 
  :punctuation (seq ".,?!-")
  :arithmetic  (seq "+-*/%=")
  :fancy       (seq "@#$^&()~")} ) 
  
(definline generic-pred [tester candidate]
 `(when-let [x# (~tester ~candidate)] x#))               
             
             
(defn brute-force "Tries a brute-force attack of a collection of the specified size. check-pred should return the match when it finds one, otherwise nil." 
([target-size check-pred & possibilities]
(let [all-poss (reduce #(into % (get-in token-types (-> %2 list flatten) :numbers)) #{} possibilities)
      perms (combi/selections all-poss target-size)
      answers (help/pool-map check-pred perms 4)] 
 (some #(when % %) answers)))
([check-pred possibilities] 
  (apply brute-force 4 check-pred possibilities)) )
  
(defn pred-builder [^String target-value]
 (partial generic-pred 
   #(when (= % (seq target-value)) 
     (apply str %))))  
  
(def PIN-pred "a dummy predicate for PINs. Returns the matching PIN"
 (pred-builder "64275555"))

;(brute-force PIN-pred [:numbers])
;(brute-force 8 PIN-pred :numbers) 
;(brute-force 8 PIN-pred :numbers [:letters :lower] :fancy)               
