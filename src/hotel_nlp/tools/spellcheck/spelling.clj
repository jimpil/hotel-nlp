(ns hotel_nlp.tools.spellcheck.spelling
  (:require [hotel_nlp.helper :as help]
            [hotel_nlp.algorithms.levenshtein :refer [levenshtein-distance]]))

;;a quite literal translation of Peter Norvig's spell checker
;;taken from the Wiki (http://en.wikibooks.org/wiki/Clojure_Programming/Examples/Norvig_Spelling_Corrector) - apparently his majesty Rich wrote this!

(def gutenberg (help/slurp-resource "corpora-train/spelling/big.txt")) ;;the text Peter Norvig used himself
(def test-cases ["acess" "accesing" "accommodation" "acount" "adress" "supposidly"])
(defn words [^String text] 
  (vec (re-seq #"[a-z]+" (.toLowerCase text))));;pretty basic tokenization
 
(defn train [features]
 (persistent!
  (reduce (fn [model f] (assoc! model f (inc (get model f 1)))) 
     (transient {}) 
     features)))
 
(def ^:dynamic *nwords* (train (words gutenberg))) 
 
(defn edits1 [word]
 (let [alphabet "abcdefghijklmnopqrstuvwxyz" n (count word)]
  (distinct (concat
      (for [i (range n)] (str (subs word 0 i) (subs word (inc i)))) 
      (for [i (range (dec n))]
        (str (subs word 0 i) (nth word (inc i)) (nth word i) (subs word (+ 2 i)))) 
      (for [i (range n) c alphabet] (str (subs word 0 i) c (subs word (inc i))))
      (for [i (range (inc n)) c alphabet] (str (subs word 0 i) c (subs word i)))))))
 
(definline known [words nwords] 
`(set (for [w# ~words :when (~nwords w#)]  w#)))
 
(definline known-edits2 [word nwords] 
`(set (for [e1# (edits1 ~word) e2# (edits1 e1#) :when (~nwords e2#)]  e2#)))

;;my code follows
 
(definline correct [nwords word]
`(let [ ed2s#  (future (known-edits2 ~word ~nwords))
        ed1s#  (known (edits1 ~word) ~nwords)      
        knowns#  (known [~word] ~nwords)
        candidates#  [knowns# ed1s# @ed2s# [~word]]]                 
  (apply max-key #(get ~nwords % 1) (some seq candidates#)))) ;;get the first non-empty list
  
(def mem-correct (memoize #(correct *nwords* %))) 

(defn correct-with-dic [dic word]
(loop [entries (seq dic)
       corrections []]
  (if (empty? entries) corrections ;;could be empty if no correction was found
  (let [f (first entries)] 
   (recur (next entries) (if (>= 2 (levenshtein-distance f word 1))
                                  (conj corrections f) corrections) )))))

;(time (doall (for [s test-cases] (mem-correct s)))
 
(defn correct-document "Process an entire document and get a map with the corrections found." 
 [filename]
(let [tokens (words (slurp filename))
      corrections (transient (hash-map))]
(doseq [[w c] (help/rmap #(vector % (mem-correct %)) tokens 50 )]
  (when (not= w c) ;;don't care about non-mistakes
   (assoc! corrections w c))) 
(persistent! (assoc! corrections :count (count tokens)))) )

(comment

(->   
(reduce 
  (fn [init [w c]] 
    (if (not= w c) 
    (assoc! corrections w c) 
    init))
(transient {}) (help/rmap #(vector % (mem-correct %)) tokens 50 ))
(assoc! :count (count tokens))
persistent!) 

)
    
;You can train on a different corpus in your namespace and use dynamic binding to re-assign *nwords*:

;(ns my-ns (:require [hotel_nlp.tools.spellcheck.spelling :refer [mem-correct train words]]))
;(def my-corpus (slurp "path-to-my-corpus"))
;(binding [*nwords* (train (words my-corpus))]
; (mem-correct "misstake"))
    
  
