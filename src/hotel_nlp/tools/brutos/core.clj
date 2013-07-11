(ns hotel_nlp.tools.brutos.core
   (:require ;[clojure.pprint :refer [pprint]]
             [hotel_nlp.helper    :as help]
             [clojure.math.combinatorics :as combi] 
             ;[clojure.core.reducers :as r]
             ;[clojure.test :refer [with-test is testing run-tests]]
  ))
             
(def token-types
 {:numbers (->> 10 range (apply str) seq) 
  :letters {:upper (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            :lower (seq "abcdefghijklmnopqrstuvwxyz")} 
  :punctuation (seq ".,?!-")
  :arithmetic  (seq "+-*/%=")
  :fancy       (seq "@#$^&()~")} ) 
  
(definline generic-pred [tester candidate]
 `(when-let [x# (~tester ~candidate)] x#))               
             
             
(defn brute-force 
"Tries a brute-force attack of a collection of the specified size. 'check-pred' should return the match when/if it finds one, otherwise nil." 
([target-size check-pred & possibilities]
(let [all-poss (reduce #(into % (get-in token-types (-> %2 list flatten) :numbers)) #{} possibilities)] 
  (some identity (help/mapr check-pred (combi/selections all-poss target-size)))))
([{:keys [target-size check-pred possibilities]}] 
  (apply brute-force target-size check-pred possibilities)) )
  
  
(defn pred-builder [^String target-value]
 (partial generic-pred 
   #(when (= % (seq target-value)) (apply str %))))  

(comment 
  
(def PIN-pred "a dummy predicate for PINs. Returns the matching PIN"
 (pred-builder "64271938"))

(brute-force {:target-size 8 
              :check-pred PIN-pred 
              :possibilities [:numbers]}) 

(brute-force 8 PIN-pred :numbers [:letters :lower] :fancy)  

)             
