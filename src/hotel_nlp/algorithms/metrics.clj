(ns hotel_nlp.algorithms.metrics
  (:require  [clojure.core.reducers :as r] 
             [clojure.test :refer [with-test is testing]])) 
             
;;pretty much what Edmund showed at the Conj!  :)           


;(def lps (repeatedly 1e6 rand)) ;;1,000,000 random numbers in a Clojure lazy-seq
;(def vps (vec lps))             ;;1,000,000 random numbers in a Clojure vector
;(def aps (double-array  vps))   ;;1,000,000 random numbers in a Java array

 
(defn- logN "Calculate the log of x with base b." 
[b ^double x] 
 (/ (Math/log x) (Math/log b)))
  

(defn- entropy* 
"Calculate the entropy of a probability (a number between 0 and 1)."
[p] {}
(if (zero? p) 1.0
  (* p (logN 2 p)))) ;;log-base 2 for convenience

(defn- entropy-sum 
"Calculate the entropy for the given probability distribution."
[ps] 
  (* -1 
   (apply + (mapv entropy* ps))))
   
(defn- entropy-asum
"Calculate the entropy for the given probability distribution. Same as 'entropy-sum' but for arrays. " 
^double [^doubles aps]
(* -1 
(areduce aps i res 0.0 (+ res (entropy* (aget aps i))))))

(defn- entropy-rsum 
"Same as 'entropy-sum' but for potentially parallel using reducers."
[ps]
(* -1
(r/fold + (r/map entropy* ps))))

(defn entropy
"Calculate the entropy of the specified probability distribution. 
 Optionally, you can specify the intermediate data-representation to work with.
 Options include :j-array, :cl-vector or :reducers."
[distribution & {:keys [using] 
                 :or {using :reducers}}]
 (* -1
   (case using
    :j-array   (entropy-asum (if (.isArray (.getClass distribution)) distribution (double-array distribution))) 
    :cl-vector (entropy-sum  (if (vector? distribution) distribution  (vec distribution))) 
    :reducers  (entropy-rsum (if (vector? distribution) distribution  (vec distribution))) 
      (throw (IllegalStateException. "Unsupported intermediate data-storage!")))))
      
(defn- zipmap++
"Same as clojure.core/zipmap but doesn't omit entries (inserts dummy values wherever necessary to make the 2 seqs of equal length).
 If dummy keys are inserted the resulting map won't be very useful for manual processing but only for fixed comparisons."
[keys vals]
  (let [diff (- (count keys) 
                (count vals))
       absdiff  (Math/abs diff)               
       keys  (if (or (zero? diff) 
                     (pos? diff)) keys
                 (reduce (fn [c _] (conj c (gensym "autoValK"))) keys (range absdiff)))
       vals  (if (or (zero? diff)  
                     (neg? diff)) vals
                 (reduce (fn [c _] (conj c (gensym "autoValV"))) vals (range  absdiff)))]
    (zipmap keys vals)))      
      

(defn confusion-matrix
"Calculates the confusion matrix given all the elements (ground-truth) and all the predictions elements (predictions).
The elements are expected to be in pairs of the type [element binary-class] -> [x :YES]  or [y :NO]. 
Absence of prediction should be represented with [e :NO] or whatever other keyword you prefer via the :b-classes parameter.
Also, the relevant and predictions element must be in the sctrict order they appeared or predictions, respectively."  
[total predictions & {:keys [b-classes] 
                      :or {b-classes [:YES :NO]}}] ;;positive class comes first
(assert (= (count total) (count predictions)) "'total' & 'predictions' should have the same size!")                       
 (let [M (reduce-kv #(if (= %2 %3) 
                       (if (= (second %3) (first b-classes))             
                          (update-in % [:TRUE :positive] conj %3)
                          (update-in % [:TRUE :negative] conj %3)) 
                       (if (= (second %3) (first b-classes))             
                          (update-in % [:FALSE :positive] conj %3)
                          (update-in % [:FALSE :negative] conj %3))) {:TRUE {:positive [] 
                                                                             :negative []}
                                                                      :FALSE {:positive [] 
                                                                             :negative []}} (zipmap total predictions))]
 {:TP (get-in M [:TRUE :positive]) 
  :TN (get-in M [:TRUE :negative])
  :FP (get-in M [:FALSE :positive]) 
  :FN (get-in M [:FALSE :negative])}))
      
(with-test      
(defn IR-metrics 
"Given a confusion-matrix (a regular map containing the keys :TP, :TN, :FP, :FN) calculates the most common IE metrics.
The map can contain the raw numbers or the actual elementsin which case the seqs will be counted internally." 
 [confusionM] ;;confusion-matrix (a regular map)
  (let [tp (:TP confusionM) 
        tp (if (number? tp) tp (count tp))
        fp  (:FP confusionM)
        fp (if (number? fp) fp (count fp))
        tn (:TN confusionM)
        tn (if (number? tn) tn (count tn))
        fneg (:FN confusionM)
        fneg (if (number? fneg) fneg (count fneg)) 
        P  (/ tp (+ tp fp))
        R  (/ tp (+ tp  fneg))
        A  (/ (+ tp tn) 
              (+ tp tn fp fneg))
        SP (/ tn (+ tn fp))      
        F1 (* 2
             (/ (* P R) 
                (+ P R)))] 
  {:precision P 
   :recall R 
   :fscore F1
   :accuracy A
   :specificity SP}))
   
(testing "IR-metrics"
(is (== 0.3   (:precision (IR-metrics {:TP 60 :FN 40 :TN 9760 :FP 140}))))
(is (== 0.982 (:accuracy  (IR-metrics {:TP 60 :FN 40 :TN 9760 :FP 140}))))
(is (== 0.6   (:recall (IR-metrics {:TP 60 :FN 40 :TN 9760 :FP 140}))))   ))   
         
      
