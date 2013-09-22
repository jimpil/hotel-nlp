(ns hotel_nlp.algorithms.SmithWaterman
  (:require [clojure.test :refer [with-test is testing]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
	                          
(defn- scoring-fn [s1 s2 match mis m [i j :as index]]
 (if (or (zero? i) (zero? j)) ;;the key values in map are k=[i j] and value=[score [where it came from i,j] char1 char2]
     (assoc m index [0 [0 0] "-" "-"]) ;;top row and first col are 0
      (let [d (first (get m [(dec i) (dec j)])) ;;score match/mismatch (diagonal)
            u (first (get m [(dec i) j])) ;;score deletion (above)
            l (first (get m [i (dec j)])) ;;score insertion (left)
            aa1 (subs s1 i (inc i)) ;;current char in s1
            aa2 (subs s2 j (inc j)) ;;current char in s2
            score (max (if (= aa1 aa2) ;;chooses how to calc current [i j] in matrix by choosing from d, u, l.
                         (+ d match)
                         (+ d mis))
                       (+ u mis) 
                       (+ l mis))]
            (assoc m index ;;insertion of the best score into the matrix
               (cond
                  (and (= d (max d u l)) (or (= (+ d mis) score) (= (+ d match) score)))
                    [score [(dec i) (dec j)] aa1 aa2]
                  (and (= u (max d u l)) (= (+ u mis) score))
                    [score [(dec i) j] aa1 "-"]
                  (and (= l (max d u l)) (= (+ l mis) score))
                    [score [i (dec j)] "-" aa2]
                  :else   [0 [0 0] "-" "-"])))))	                             
       
(defn- trace-back 
[score start-loc H]
(loop [loc start-loc
        aln_s1 ""
        aln_s2 ""]
 (let [[n [i j :as idx] c1 c2] (get H loc)] ;;stores the next location [i j] to go to in H
    (if-not (zero? (+ i j))
     (recur idx
       (str c1 aln_s1) ;;builds strings up from the right to left
       (str c2 aln_s2))
         (if (= "-" c1 c2)
     [score aln_s1 aln_s2]
     [score (str c1 aln_s1) (str c2 aln_s2)]))))) 
           

(with-test 
(defn smith_waterman 
"Find the best local allignment between these two seqs (character-seqs or String objects) using Smith–Waterman algorithm."
[seq1 seq2 & {:keys [match-weight mismatch-weight]
              :or {match-weight 2
                   mismatch-weight -1}}]
  (let [^String s1 (apply str "-" seq1)
        ^String s2 (apply str "-" seq2)
        match match-weight  ;;match
        mis mismatch-weight ;;mismatch or gap
        all-locs (for [i (range (.length s1)) 
                       j (range (.length s2))] 
                    [i j])
        H  (reduce #(scoring-fn s1 s2 match-weight mismatch-weight %1 %2) {} all-locs) ;;creates score matrix
        start (last (sort-by first (vals H))) ;;finds highest value in matrix
        start-locs (map first (filter #(= start (val %)) H))] ;;starts traceback from this highest value
    (map #(trace-back (first start) % H) start-locs)))
;;the wiki example test case
(testing "Smith-Waterman local allignment on the wikipedia example"
(is (= '([5 "AC-CTAA" "GCTC-AA"]) (smith_waterman "ACCTAAGG" "GGCTCAATCA"))) ;taken from a presentation
(is (= '([12 "A-CACACTA" "AGCACAC-A"]) (smith_waterman "ACACACTA" "AGCACACA"))) ;;2 strings
(is (= '([12 "A-CACACTA" "AGCACAC-A"]) (smith_waterman '(\A \C \A \C \A \C \T \A) '(\A \G \C \A \C \A \C \A)))) ;;2 char-seqs
(is (= '([12 "A-CACACTA" "AGCACAC-A"]) (smith_waterman '(\A \C \A \C \A \C \T \A) "AGCACACA"))) ;;1 string and a char-seq 
))    
    
    
