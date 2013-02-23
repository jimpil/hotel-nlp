(ns hotel_nlp.tools.namegen.core
    (:require [hotel_nlp.helper :as ut]
              [hotel_nlp.concretions.regexes :as re] 
              [clojure.java.io :as io]
              [clojure.core.reducers :as r]))

(def drugbank-singletons "All the single-word entries found in drugbank."
  (filter ut/singleton-token? (ut/normaliser (io/resource "dictionaries/DRUGBANK/DRUGBANK-TERMS.txt"))))

(defn name-parts [names] ;;get some basic syllable statistics
(let [lead  (transient []) 
      inner (transient []) 
      trail (transient [])]
(doseq [n names] 
  (when-let [pat1 (re-find re/_LEAD n)]
     (conj! lead pat1))
   (when-let [pats (re-seq re/_INNER n)]
     (doseq [p pats] 
       (conj! inner p)))
   (when-let [pat2 (re-find re/_TRAIL n)]
      (conj! trail pat2)))
  [(frequencies (persistent! lead)) 
   (frequencies (persistent! inner)) 
   (frequencies (persistent! trail))])) ;;a vector of 3 maps   

(defn freq-desc [freqs bottom-limit] ;(keep the ones with at least five occurences)
 (mapv first 
   (take-while #(> (second %) bottom-limit) (sort-by second > freqs))))
 

(defn namegen [nparts & {:keys [alt-lead alt-trail limit] :or {limit 5}}] 
 (let [[leads inners trails] (mapv #(freq-desc % limit) nparts)
       random-int (rand-int 3)
       syllables (if (> (rand) 0.85) (+ 2 random-int) (inc random-int))] 
 (loop [name (or alt-lead (rand-nth leads))
        ss syllables]
   (if (zero? ss) (str name  (or alt-trail (rand-nth trails)))
     (recur (str name (rand-nth inners)) (dec ss)))))) 
     
(def drugen "Generates a single drug name that looks like a name fom drugbank (orthographically)."
(partial namegen (name-parts drugbank-singletons))) ;; (drugen :alt-trail "ine")  (drugen :alt-lead "card")

(defn randrugs 
([]  (repeatedly drugen))   ;;infinite amount of drug-names
([t] (repeatedly t drugen)) ;;limited amount of drug-names
([t pred]    ;;limited amount of drug-names with some restriction imposed
  (if (< t 11) (remove pred (randrugs t))  ;;don't bother folding for less than 11 elements
    (ut/fold-into-vec 15 (r/remove pred (vec (randrugs t))))))
([t pred & more] 
 (let [ress (repeatedly t #(apply drugen more))] 
 (if (< t 11)  (remove pred ress)
   (ut/fold-into-vec 15 (r/remove pred (vec ress)))))))
   
   
