(ns hotel_nlp.tools.normalito.core
   (:require ;[clojure.pprint :refer [pprint]]
             [clojure.core.memoize :as memo]
             [hotel_nlp.helper    :as help] 
             [clojure.core.reducers :as r]) )
;-----------------------------------------------------------------------------------------------------------------------------------             
;----------------------------<EXPERIMENTAL CODE>------------------------------------------------------------------------------------             

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol Normalisable
(normalise [this transformer]))        

;;High-performance extension points for all major Clojure data-structures including arrays [ints, floats, longs & doubles]
;;whatever collection type you pass in, the same type you will get back unless nothing covers it, in which case a lazy-seq will be most likely returned.
;;If you pass a non-persistent java.util.List object, you'll get a persistent vector back.
;;Nested colelctions work as well.
(extend-protocol Normalisable     
Number
(normalise [this transform] (transform this))
String
(normalise [this stem] (stem this))
  
java.util.List ;;if this fires, we're dealing with a Java list-like collection - return a java.util.ArrayList
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
  (reduce 
    (fn [^java.util.List l x] 
     (doto l 
      (.add (normalise x #(transform % this))))) 
  (java.util.ArrayList. (.size this)) this)) )   
  
clojure.lang.IPersistentCollection;;if this fires, we don't know the type so we'll return a lazy-seq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform ) this)
  (map (fn [x] (normalise x #(transform % this))) this)) )

clojure.lang.PersistentList
(normalise
[this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
  (->> (mapv (fn [x] (normalise x #(transform % this))) this)
     rseq
    (into '()))) ) 
    
clojure.lang.LazySeq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform) this)
   (map (fn [x] (normalise x #(transform % this))) this)) )
      
clojure.lang.IPersistentVector
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
  (if (> 1124 (count this))     
   (mapv (fn [x] (normalise x #(transform % this))) this)
   (into [] (r/foldcat (r/map (fn [x] (normalise x #(transform % this))) this))))) ) 
     
clojure.lang.IPersistentSet ;;sets are typically not ordered so ordering will dissapear after processing
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (persistent!        
   (reduce (fn [ts x] (conj! ts (normalise x #(transform % this)))) (transient #{}) this))) )
   
clojure.lang.IPersistentMap ;;assuming a map with collections for keys AND values (a dataset perhaps?)
(normalise [this transform]
 (persistent!        
   (reduce-kv #(assoc! %1 (normalise %2 transform) 
                          (normalise %3 transform)) (transient {}) this)))   )

;;do the same for popular primitive array types   
(extend-protocol Normalisable   
(Class/forName "[D")  
(normalise [this transform]
 (amap ^doubles this idx ret (double (normalise (aget ^doubles this idx) #(transform % this))))))
 
(extend-protocol Normalisable   
(Class/forName "[[D")  ;;2d double-arrays are very common
(normalise [this transform]
 (into-array (mapv #(normalise % transform) this))))     
   
(extend-protocol Normalisable   
(Class/forName "[F")  
(normalise [this transform]
 (amap ^floats this idx ret (float (normalise (aget ^floats this idx) #(transform % this))))))
 
(extend-protocol Normalisable   
(Class/forName "[[F")  ;;2d float-arrays are very common
(normalise [this transform]
 (into-array (mapv #(normalise % transform) this))))   
   
(extend-protocol Normalisable   
(Class/forName "[J")  
(normalise [this transform]
 (amap ^longs this idx ret (long (normalise (aget ^longs this idx) #(transform % this))))))
 
(extend-protocol Normalisable   
(Class/forName "[[J")  ;;2d long-arrays are very common
(normalise [this transform]
 (into-array (mapv #(normalise % transform) this))))   
      
(extend-protocol Normalisable   
(Class/forName "[I")  
(normalise [this transform]
 (amap ^ints this idx ret (int (normalise (aget ^ints this idx) #(transform % this))))))
 
(extend-protocol Normalisable   
(Class/forName "[[I")  ;;2d int-arrays are also very common
(normalise [this transform]
 (into-array (mapv #(normalise % transform) this))))   
  
(extend-protocol Normalisable   
(Class/forName "[Ljava.lang.Object;")  
(normalise [this transform]
 (amap #^"[Ljava.lang.Object;" this idx ret (normalise (aget #^"[Ljava.lang.Object;" this idx) #(transform % this)))))
 
(extend-protocol Normalisable   
(Class/forName "[Ljava.lang.String;")  
(normalise [this transform]
 (amap #^"[Ljava.lang.String;" this idx ret (normalise (aget #^"[Ljava.lang.String;" this idx) #(transform % nil)))))  
   
   
;;this is how client code would look like
(def in-range-needs  (memo/ttl (juxt #(apply max %) #(apply min %)) :ttl/threshold 3000)) ;the upper/lower data limits

;;IN-RANGE formula  
(defn in-range-formula 
"The most common normalisation technique (for numbers)." 
([x coll [bn tn]]  
(let [[ti bi] (in-range-needs coll)]  
(+ bn
   (* (/ (- x  bi) 
         (- ti bi)) 
      (- tn bn))) ))
([x coll] 
  (in-range-formula x coll [-1 1])) )

;typical in-range transformers
(def transform-in-range1 "In-range [-1 1] transformer." in-range-formula)
(def transform-in-range5 "In-range [-5 5] transformer." #(in-range-formula %1 %2 [-5 5]))

;;RECIPROCAL formula
(defn reciprocal-formula 
"Reciprocal normalization is always normalizing to a number in the range between 0 and 1.
 It should only be used to normalize numbers greater than 1. In particular, do NOT pass in 0 as the first arg."
([x _ _]
 (if (> x 1)  (/ 1 x) 
   (throw (IllegalArgumentException. "Reciprocal-formula cannot be used with values less than 1."))))
([x _]
(reciprocal-formula x _ nil))
([x] 
 (reciprocal-formula x nil)) )

;typical reciprocal tranformer 
(def transform-reciprocal "Reciprocal transformer." reciprocal-formula) 
 
(defn divide-by-value-formula
"Normalises by dividing all elements by the given value." 
([x _ div-val]
   (/ x div-val))
([x _]
  (divide-by-value-formula x nil 10)) )       

;typical divide-by-value transformers
(def transform-by-value10 "Divide-by-10 transformer."  divide-by-value-formula)
(def transform-by-value2 "Divide-by-2 transformer."  #(divide-by-value-formula %1 %2 2))

(defn porter-formula 
"The normalisation 'formula' for Porter's algorithm." 
([^String s _ lang-or-obj]
  (help/porter-stem s lang-or-obj))
([^String s _] 
  (porter-formula s  nil "english"))
([^String s] 
  (porter-formula s nil)) )

;typical porter transformers
(def transform-by-porter "Porter's normalisation transformer for english." porter-formula)
(def transform-by-porter-reuse "Porter's normalisation transformer that reuses the same object." #(porter-formula %1 (help/porter-stemmer "english") %2))

;MULTIPLICATIVE formula
(defn multiplicative-needs* ^double [coll] 
  (/ 1.0 (Math/sqrt (reduce #(+ % (* %2 %2)) coll)))) ;;produces the multiplicative normalisation factor
  
(def multiplicative-needs (memo/ttl multiplicative-needs* :ttl/threshold 3000)) ;;configure the caching threshold according to your needs 

(defn multiplicative-formula 
([x coll _]
 (let [mfactor (multiplicative-needs coll)]
    (* x mfactor)))
([x coll]
 (multiplicative-formula x coll nil) ))


;Z-AXIS formula 
(defn z-axis-needs* [coll]
 (let [n (count coll)
       vlength (Math/sqrt (reduce #(+ % (* %2 %2)) 0 coll)) 
       zfactor (/ 1.0 (Math/sqrt n))
       synthetic-field (* zfactor (Math/sqrt (- n (* vlength vlength))))]
 [zfactor (if (or (Double/isInfinite synthetic-field)
                  (Double/isNaN synthetic-field)) 0 synthetic-field)]))
   
(def z-axis-needs (memo/ttl z-axis-needs* :ttl/threshold 3000)) ;;configure the cashing threshold according to your needs 
 
(defn z-axis-formula 
([x coll _]
 (let [[zfactor synthetic-field]  (z-axis-needs coll)] 
  (+ synthetic-field (* x zfactor))))
([x coll] 
 (z-axis-formula x coll nil)) )     

 
   
