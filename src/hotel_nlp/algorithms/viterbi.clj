(ns hotel_nlp.algorithms.viterbi
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :refer [split]]
            [hotel_nlp.algorithms.ngrams :refer [ngrams*]]
            [hotel_nlp.helper   :refer [read-resource let-timed third fourth map-difference]]))
 
(defrecord HMM ;;example
[states observations init-probs emission-probs state-transitions]
 Object
 (toString [this] 
   (str "Number of states: " (count (:states this)) "\n"
        "Number of observations: " (count (:observations this)) "\n"
        "Init probs: "     (:init-probs this) "\n"
        "Emission probs: " (:emission-probs this) "\n"
        "Transitions probs: " (:state-transitions this)))) 
        
;(defrecord TokenTagPair [token tag]) 
;(def ^:dynamic *n* 2)                                           

 
(defn make-hmm 
([states obs init-probs emission-probs state-transitions]
  (HMM. states obs
    ;(if (vector? states) states (vec states))
    #_(if (vector? obs)     obs (vec obs)) init-probs emission-probs state-transitions))
([states init-probs emission-probs state-transitions] ;;no observations yet 
  (make-hmm states nil init-probs emission-probs state-transitions )))
         

(declare smoothing-weights)

(defn- build-init-map [ks n]
 (if (zero? n) 0
  (zipmap ks (repeat (build-init-map ks (dec n))))))

(defn tables [^String corpus-file & {:keys [tt-sep n] 
                                     :or {tt-sep #"\|" n 2}}]     
(let [token-tag-pairs  (with-open [^java.io.Reader r (clojure.java.io/reader corpus-file)]
                              (doall
                                (for [tt-pair (line-seq r) :let [[token tag] (split tt-pair tt-sep)]]
    				 {:token token :tag tag})))
      tokens (map :token token-tag-pairs)
      ;smoothing-weights (future (when (= n 3) (smoothing-weights tokens)))
      tags   (map :tag  token-tag-pairs) ;;add start-of-sentence tag
      ngrams (ngrams* tags n)  ;;trigrams  (ngrams* tags 3)
      ngram-frequencies (frequencies ngrams) 
      tag-groups  (group-by :tag token-tag-pairs) ;; 
      tag-frequency  (frequencies tags)
      tagset (keys tag-frequency)
      ;states (case n 3 (map reverse (ngrams* tags 2))  2 tagset)  ; 
      ;token-groups (group-by :token token-tag-pairs)
      tags-per-token  (persistent! 
                        (reduce-kv #(assoc! %1 %2 (group-by :token %3)) (transient {}) tag-groups)) ;(with-meta %3 (group-by :token %3)) 
                       #_(reduce #(update-in %1 (mapv :tag %2)  (fnil conj []) (:token (second %2)) ) {} (ngrams* token-tag-pairs 2))
      init-map  (build-init-map tagset n)
      tra-table (reduce-kv #(update-in % %2 + %3 ) init-map ngram-frequencies)] 
[tags-per-token tag-frequency (count tokens) tra-table] ))
;;all we need is here (emmissions, starts, token-count, transitions, unique tags,(for smoothing) graph-length)        

   
(defn proper-statistics [[ems starts all tra-table :as em-tables]]
(let [em-probs (reduce (fn [s [tag token c]] (assoc-in s [tag token] (/ c all))) {} 
                  (for [[k1 v1] ems [k2 v2] v1] [k1 k2 (count v2)]))
     start-probs (reduce-kv #(assoc % %2 (/ %3 all)) {} starts)
     trans-probs (reduce (fn [s [tag1 tag2 c]] (assoc-in s [tag1 tag2] (/ c (get starts tag2)))) {}
                  (for [[k1 v1] tra-table [k2 v2] v1] [k1 k2 v2]) ) ]
 [(remove nil? (keys trans-probs)) start-probs em-probs trans-probs]))  
 
 (defn- scale-by ;;adopted from contrib
  "Multiply each entry in dist by the scale factor s and remove zero entries."
  [dist s]
  (reduce-kv 
    (fn [m k p] 
      (when (pos? p) (assoc m k (* p s)))) 
   {} dist))
	  
	  
	  
(defn normalize ;from contrib
  "Convert a weight map (e.g. a map of counter values) to a distribution
   by multiplying with a normalization factor. If the map has a key
   :total, its value is assumed to be the sum over all the other values and
   it is used for normalization. Otherwise, the sum is calculated
   explicitly. The :total key is removed from the resulting distribution."
  [weights]
  (let [total (:total weights)
	      w (dissoc weights :total)
	      s (/ 1 (if (nil? total) (apply + (vals w)) total))]
    (scale-by w s)))	  	  
 
(defn- smoothing-weights 
"The 'deleted-interpolation' algorithm for smoothing when combining unigram, bigram & trigram probabilities (per Brants[2000]). "
[tokens]
(let [w1 (atom 0) w2 (atom 0)  w3 (atom 0) t-count (count tokens)
      unigrams tokens
      unigram-count t-count ;(count unigrams)
      unigram-freqs (frequencies unigrams)
      unigram-probs (persistent! (reduce-kv #(assoc! % %2 (/ %3 unigram-count)) (transient {}) unigram-freqs))
      bigrams  (ngrams* tokens 2)
      ;bigram-count (count bigrams)
      bigram-freqs  (frequencies bigrams)
      bigram-probs  (persistent! (reduce-kv #(assoc! % %2 (/ %3 (get unigram-freqs (first %2)))) (transient {}) bigram-freqs))
      trigrams (ngrams* tokens 3)
      ;trigram-count (count trigrams)
      trigram-freqs (frequencies trigrams)
      trigram-probs (persistent! (reduce-kv #(assoc! % %2 (/ %3  (get bigram-freqs (take 2 %2)))) (transient {}) trigram-freqs))
      tri-f (fn [trigram] (try  (/ (dec (get trigram-freqs trigram)) ;;(cons last (drop-last trigram) ) 
                                   (dec (get bigram-freqs (drop-last trigram))))
                          (catch ArithmeticException ae 0)))
      bi-f  (fn [trigram] (try  (/ (dec (get bigram-freqs  (drop 1 trigram)))
                                   (dec (get unigram-freqs (second trigram))))
                          (catch ArithmeticException ae 0)))
      un-f  (fn [trigram] (try  (/ (dec (get unigram-freqs (last trigram)))
                                   (dec t-count))
                          (catch ArithmeticException ae 0)))]                                                                                            
(doseq [trgm trigrams]
  (let [res (first 
            (apply max-key second 
               {:t (tri-f trgm)
     		        :b (bi-f  trgm) 
                :u (un-f  trgm)}))]
  (case res
        :u  (swap! w1 + (get trigram-freqs trgm))
        :b  (swap! w2 + (get trigram-freqs trgm))
        :t  (swap! w3 + (get trigram-freqs trgm)))))
{:weights (normalize {:w1 @w1 :w2 @w2 :w3 @w3}) ;; need distribution (w1+w2+w3=1)
 :uni-probs unigram-probs :bi-probs  bigram-probs :tri-probs trigram-probs})) 
         
 
(defn argmax [coll]
  (loop [s (map-indexed vector coll)
         maximum (first s)]
    (if (empty? s)  maximum
      (let [[idx elt] (first s)
            [max-indx max-elt] maximum]
        (if (> elt max-elt)
          (recur (next s) (first s))
          (recur (next s) maximum))))))
 
(defn init-alphas [hmm obs]
  (mapv (fn [x] ;(println x)
         (* (get (:init-probs hmm) x) 
            (get-in (:emission-probs hmm) [x obs] 1.0E-6)))
   (:states hmm)) )


(defn forward [hmm alphas obs]
  (mapv (fn [state1]
         (* (reduce (fn [sum state2] ;(println state2)
             (+ sum (* (get alphas (.indexOf ^clojure.lang.APersistentVector (:states hmm) state2)) 
                       (get-in (:state-transitions hmm) [state2 state1] 1.0E-6)))) ;;NEED SMOOTHING HERE
                    0
            (:states hmm))   ;(:states hmm)
          (get-in (:emission-probs hmm) [state1 obs] 1.0E-6)))
     (:states hmm)) )
 
(defn delta-max [hmm deltas obs]
 (mapv (fn [state1]
        (* (apply max (mapv (fn [state2]
                              (* (get deltas (.indexOf ^clojure.lang.APersistentVector (:states hmm) state2))
                                 (get-in (:state-transitions hmm) [state2 state1] 1.0E-6 #_(weighted-sum (list state2 state1) *smoothing-weights*)))) ;;AND HERE
                            (:states hmm)))
            (get-in (:emission-probs hmm) [state1 obs] 1.0E-6))) ;;AND HERE
     (:states hmm)))

 
(defn backtrack [paths deltas]
  (loop [path (rseq paths) ;;reverse in constant-time 
         term (first (argmax deltas))
         backtrack '()] 
    (if (empty? path) (cons term backtrack) 
      (recur (next path) (get (first path) term) (cons term backtrack)))))
 
(defn update-paths [hmm deltas]
  (mapv (fn [state1]
         (first (argmax (mapv (fn [state2]
                                  (* (get deltas (.indexOf ^clojure.lang.APersistentVector (:states hmm) state2))
                                     (get-in (:state-transitions hmm) [state2 state1] 1.0E-6))) ;;AND HERE
                                (:states hmm)))))
        (:states hmm)))
       
(definline paths->states [paths states]
  `(for [p# ~paths] (get ~states p# )))     
 
(defn viterbi 
([hmm]
(let [observs (:observations hmm)
      states (:states hmm)
      states (cond-> states 
               ((complement vector?) states) vec)
      hmm (assoc hmm :states states)] ;;states MUST be a vector
  (loop [obs (next observs)
         alphas (init-alphas hmm (first observs)) 
         deltas  alphas
         trellis []]
    (if (empty? obs) 
      [(paths->states (backtrack trellis deltas) states)  (reduce +' alphas)]
      (recur (next obs)
        (forward hmm alphas   (first obs))
        (delta-max hmm deltas (first obs))
        (conj trellis (update-paths hmm deltas)))))))
([hmm [t1 t2 & more :as tokenized-sentence]] 
  (viterbi (assoc hmm :observations tokenized-sentence))))


;------------------------------------------------------------------------------------------------             
;------------------<EXAMPLES>---------------------------------------------------------------------
(comment     ; wiki-example

;Consider a primitive clinic in a village. People in the village have a very nice property that they are either healthy or have a fever. 
;They can only tell if they have a fever by asking a doctor in the clinic. The wise doctor makes a diagnosis of fever by asking patients how they feel.
;Villagers only answer that they feel normal, dizzy, or cold. Suppose a patient comes to the clinic each day and tells the doctor how she feels. 
;The doctor believes that the health condition of this patient operates as a discrete Markov chain.
;There are two states, "Healthy" and "Fever", but the doctor cannot observe them directly, that is, they are hidden from him. 
;On each day, there is a certain chance that the patient will tell the doctor he has one of the following feelings, depending on his health condition: "normal", "cold", or "dizzy".
;Those are the observations. The entire system is that of a hidden Markov model (HMM).
;The doctor knows the villager's general health condition, and what symptoms patients complain of with or without fever on average. 
;In other words, the parameters of the HMM are known.
;The patient visits three days in a row and the doctor discovers that on the first day he feels normal, on the second day he feels cold, on the third day he feels dizzy. 
;The doctor has a question: what is the most likely sequence of health condition of the patient would explain these observations? This is answered by the Viterbi algorithm.

(def observations [:normal :cold :dizzy])
(def states [:healthy :fever])
(def start-probs {:healthy 0.6 
                  :fever 0.4})
(def emission-probs {:healthy {:normal  0.5  :cold 0.4 :dizzy 0.1}
                     :fever   {:normal  0.1  :cold 0.3 :dizzy 0.6}})
(def transition-probs {:healthy  {:healthy 0.7 :fever  0.3}
                       :fever    {:healthy 0.4 :fever  0.6}})
;;CONSTRUCT THE HIDDEN-MARKOV-MODEL                                            
(def hmm (HMM. states observations start-probs emission-probs transition-probs))                     
;;run viterbi
(viterbi hmm)
;=>[(:healthy :healthy :fever) 0.03628] ;;correct!

;;what this means is that, the observations ['normal', 'cold', 'dizzy'] were most likely generated by states ['Healthy', 'Healthy', 'Fever'].
;In other words, given the observed activities, the patient was most likely to have been healthy both on the first day when he felt normal as well as on the second day 
;when he felt cold, and then he contracted a fever the third day. The sum of probabilities along that path are provided as the 2nd element in the vector. 

;;EXAMPLE 2: (taken from Borodovsky & Ekisheva 2006, pp: 80-81)

(def states [:H :L]) ;high-low
(def observations (vec "GGCACTGAA"))
(def start-probs {:H 0.5 
                  :L 0.5})
(def emission-probs {:H {\A 0.2  \C 0.3 \G 0.3 \T 0.2}
                     :L {\A 0.3  \C 0.2 \G 0.2 \T 0.3}})
(def transition-probs {:H  {:L 0.5 :H  0.5}
                       :L  {:H 0.4 :L  0.6}})
;;CONSTRUCT THE HIDDEN-MARKOV-MODEL                                            
(def hmm (HMM. states observations start-probs emission-probs transition-probs))                     
;;run viterbi 
(viterbi hmm)
;=>[(:H :H :H :L :L :L :L :L :L) 3.791016E-6]  ;;correct!


;;EXAMPLE 3: (taken from J&M 2nd ed., sec:5.5.3)
;;assume a very simplified subset of POS-tag classes [VB, TO, NN, PPSS] and 4 words
(def states [:VB :TO :NN :PPSS]) ;;normally we'd have  all the possible tags (e.g. 36 for PENN)
(def observations ["I" "want" "to" "race"]) ;;from their example - any sentence would do
(def start-probs {:VB 0.019 
                  :TO 0.0043
                  :NN 0.041
                  :PPSS 0.067})
                     
(def emission-probs {:VB   {"want" 0.0093  "race" 0.00012}
                     :TO   {"to" 0.99}
                     :NN   {"want" 0.000054  "race" 0.00057}
                     :PPSS {"I" 0.37 }})                     

(def transition-probs {:VB   {:VB 0.0038 :TO 0.0345 :NN 0.047 :PPSS 0.07}
                       :TO   {:VB 0.83 :TO 0 :NN 0.00047 :PPSS 0}
                       :NN   {:VB 0.0040 :TO 0.016 :NN 0.087 :PPSS 0.0045}
                       :PPSS {:VB 0.23 :TO 0.00079 :NN 0.0012 :PPSS 0.00014}})

;;CONSTRUCT THE HIDDEN-MARKOV-MODEL                                            
(def hmm (HMM. states observations start-probs emission-probs transition-probs))                     
;;run viterbi on it
(viterbi hmm)
;=>[(:PPSS :VB :TO :VB) 1.8087296E-10]   ;;correct!

;;EXAMPLE 4 -- with automatic extraction of probabilities from a given corpus
;(def a-corpus (slurp "formitsos_line.txt")) 
;;OR normally-> (slurp "formitsos_line.txt")
(def probs (read-resource "corpora-train/BROWN-NLTK/brown-probs.txt"))  ;(proper-statistics (tables (slurp (clojure.java.io/resource "corpora-train/BROWN-NLTK/nltk_brown_pos.txt")) :n 3))
(def hmm (let [[states  starts  emms trans] probs]    ;{states :states starts :init-probs  emms :emission-probs trans :state-transitions} when reading probs from file
          (make-hmm states starts emms trans)))
(viterbi hmm ["At" "least" "4" "people" "were" "killed" "as" "a" "tsunami" "hit" "the" "Japanese" "island"  "."])
;=>[("DT" "NN" "VBD" "IN" "DT" "JJ" "NN" "CC" "PPSS" "DOD" "NEG" "VB" "PPS" "TERM") 1.6070133E-18]  ;;correct!

;["French" "jets" "bomb" "rebel" "bases" "and" "depots" "in" "northern" "Mali" "in" "an" "effort" "to" "cut" "of" "supply" "lines"  "."]
;["Plants" "need" "light" "and" "water" "to" "grow" "."]
;["Police" "investigating" "allegations" "of" "child" "abuse" "at" "a" "guest" "house" "in" "west" "London" "arrest" "two" "men"  "."]
;["Mr." "Brown" "lost" "this" "election" "by" "a" "small" "margin" "."]
;["The" "fox" "jumped" "over" "the" "lazy" "dog" "and" "I" "did" "not" "notice" "it" "!"]  
;["The" "US" "central" "intelligence" "agency" "has" "been" "operating" "a" "secret" "airbase" "in" "Saudi" "Arabia" "."]
;["At" "least" "4" "people" "were" "killed" "as" "a" "tsunami" "hit" "the" "Japanese" "island"  "."]       
;["They" "met"  "in" "the" "pub" "late" "at" "night" "."]


(def tri-probs ;;the trigram probabilities/statistics
  (proper-statistics (tables (slurp (clojure.java.io/resource "corpora-train/BROWN-NLTK/nltk_brown_pos.txt")) :n 3)))
(let [[states  starts  emms trans smws] probs3]
(def hmm3 (make-hmm states starts emms trans)) 
(binding [*smoothing-weights* smws *n* 3] 
 (viterbi hmm3 ["All" "Plants" "need" "light" "and" "water" "to" "grow" "."])))



(def states [:VB :TO :NN :PPSS :ADV])
(def observations ["I" "want" "to" "dance" "today" "."]) 
(def start-probs {:VB 0.019 
                  :TO 0.0043
                  :NN 0.041
                  :PPSS 0.067
                  :ADV 0.0039})

(def emission-probs {:VB   {"want" 0.0093  "dance" 0.00012}
                     :TO   {"to" 0.99}
                     :NN   {"want" 0.000054  "dance" 0.00057 "today" 0.000061}
                     :PPSS {"I" 0.37 }
                     :ADV  {"today" 0.51}})             







) 
;"The" "Fulton" "County" "Grand" "Jury" "said" "Friday" "an" "investigation" "of" "Atlanta's" "recent"  "primary" "election" "produced" "``" "no" "evidence" "''" "that" "any" "irregularities" "took" "place" ". "

