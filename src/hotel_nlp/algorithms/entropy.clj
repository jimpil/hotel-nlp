(ns hotel_nlp.algorithms.entropy
  (:require  [clojure.repl :refer [pst]] 
             [clojure.core.reducers :as r])) 
             
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
