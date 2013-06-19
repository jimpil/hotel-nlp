(ns hotel_nlp.tools.budas.core
   (:require ;[clojure.pprint :refer [pprint]]
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
(normalise [this transform] 
  (transform this))
 
String
(normalise [this stem] 
  (stem this))
  
java.util.List ;;if this fires, we're dealing with a Java list-like collection - return a vector
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
   (mapv (fn [x] (normalise x #(transform % [top bottom]))) this))) ) 
  
clojure.lang.IPersistentCollection;;if this fires, we don't know the type so  we'll return a lazy-seq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform ) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
   (map (fn [x] (normalise x #(transform % [top bottom]))) this))) )

clojure.lang.PersistentList
(normalise
[this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
  (->> (mapv (fn [x] (normalise x #(transform % [top bottom]))) this)
     rseq
    (into '())))) ) 
    
clojure.lang.LazySeq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
   (map (fn [x] (normalise x #(transform % [top bottom]))) this))) )
      
clojure.lang.IPersistentVector
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
  (if (> 1124 (count this))     
   (mapv (fn [x] (normalise x #(transform % [top bottom]))) this)
   (into [] (r/foldcat (r/map (fn [x] (normalise x #(transform % [top bottom]))) this)))))) ) 
     
clojure.lang.IPersistentSet ;;sets are typically not ordered so ordering will dissapear after processing
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (persistent!        
   (reduce (fn [ts x] (conj! ts (normalise x #(transform % [top bottom])))) (transient #{}) this)))) )
   
clojure.lang.IPersistentMap ;;assuming a map with collections for keys AND values (a dataset perhaps?)
(normalise [this transform]
 (persistent!        
   (reduce-kv #(assoc! %1 (normalise %2 transform) 
                          (normalise %3 transform)) (transient {}) this)))   )

;;do the same for popular primitive array types   
(extend-protocol Normalisable   
(Class/forName "[D")  
(normalise [this transform]
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (amap ^doubles this idx ret (double (normalise (aget ^doubles this idx) #(transform % [top bottom])))))))  
   
(extend-protocol Normalisable   
(Class/forName "[F")  
(normalise [this transform]
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (amap ^floats this idx ret (float (normalise (aget ^floats this idx) #(transform % [top bottom])))))))
   
(extend-protocol Normalisable   
(Class/forName "[J")  
(normalise [this transform]
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (amap ^longs this idx ret (long (normalise (aget ^longs this idx) #(transform % [top bottom])))))))
      
(extend-protocol Normalisable   
(Class/forName "[I")  
(normalise [this transform]
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (amap ^ints this idx ret (int (normalise (aget ^ints this idx) #(transform % [top bottom])))))))
  
(extend-protocol Normalisable   
(Class/forName "[Ljava.lang.Object;")  
(normalise [this transform]
 (let [top    (delay (apply max this))
       bottom (delay (apply min this))]
 (amap #^"[Ljava.lang.Object;" this idx ret (normalise (aget #^"[Ljava.lang.Object;" this idx) #(transform % [top bottom]))))))
   
   
;;this is how client code would look like   
(defn in-range-formula 
"The most common normalisation technique (for numbers)." 
([x [tn bn] [ti bi]] 
(+ bn
   (* (/ (- x (force bi)) 
         (- (force ti) (force bi))) 
      (- tn bn))) )
([x [ti bi]] 
  (in-range-formula x [1 -1] [ti bi]))
([x] 
  (in-range-formula x [1 -1] [10 -10])) )

;typical in-range transformers
(def transform-in-range1 "In-range [1 -1] transformer." in-range-formula)
(def transform-in-range5 "In-range [5 -5] transformer." #(in-range-formula %1 [5 -5] %2))

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
([x div-val _]
   (/ x div-val))
([x _]
  (divide-by-value-formula x 10 nil)) )       

;typical divide-by-value transformer
(def transform-by-value "Divide-by-value transformer."  divide-by-value-formula)

(defn porter-formula 
"The normalisation 'formula' for Porter's algorithm." 
([^String s lang-or-obj _]
  (help/porter-stem s lang-or-obj))
([^String s _] 
  (porter-formula s "english" nil))
([^String s] 
  (porter-formula s "english")) )

;typical porter transformers
(def transform-by-porter "Porter's normalisation transformer for english." porter-formula)
(def transform-by-porter-reuse "Porter's normalisation transformer that reuses the same object." #(porter-formula %1 (help/porter-stemmer "english") %2)) 
   
