(ns hotel_nlp.tools.normalito.core
   (:require ;[clojure.pprint :refer [pprint]]
             [clojure.core.memoize :as memo]
             [hotel_nlp.helper    :as help] 
             [clojure.core.reducers :as r]) )
;-----------------------------------------------------------------------------------------------------------------------------------             
;----------------------------<EXPERIMENTAL CODE>------------------------------------------------------------------------------------             

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defprotocol DataSet "A dataset should be able to perform these basic operations."
(normalise [this transformer])
(getMean [this])
(getVariance [this] 
             [this sample?])
(getStdDeviation [this] 
                 [this sample?])
(getCorrelation [this other] 
                [this other sample?])) 
                
#_(def ^:private commons "Common implementations across extension-points." 
  {:mean '(getMean [this] (help/avg this))
   :variance '(getVariance 
                ([this]         (getVariance this nil)) 
                ([this sample?] (help/variance this sample? (count this))))
   :std '(getStdDeviation 
           ([this]         (Math/sqrt (getVariance this)))
           ([this sample?] (Math/sqrt (getVariance this sample?)))) 
   :correlation  '(getCorrelation 
                    ([this other]         (getCorrelation this other nil)) 
                    ([this other sample?] (help/corr-coefficient this other sample?)))})                       

;;High-performance extension points for all major Clojure data-structures including arrays [ints, floats, longs & doubles]
;;in general, whatever collection type you pass in, the same type you will get back unless nothing covers it (in which case a lazy-seq will be most likely returned)
;;or it is not safe to do so (i.e operations that are not 'safe' for integers). 
;;This has a performance consequence when for example normalisation is performed on an array of ints. In order to do all arithmetic operations correctly, ints have to 
;;be converted to doubles which involves an extra array initialisation step. The same applies when asking the standard-deviation of ints for instance. 
;;For this reason always prefer persistent collections (vectors can do parallel normalisation) or arrays of doubles for maximum serial performance.    
;;Nested collections work as well. Up to 2 dimensions are supported for primitive arrays and a single dimension for boxed arrays.

;; NORMALISATION INPUT/OUTPUT TYPES
;-----------------------------------
; Number -> Number
; String -> String
; Collection -> whatever concrete type was passed in  (just-in-case extension & for interop)
; Map -> IPersistentMap  (just-in-case extension)
; IPersistentCollection -> LazySeq  (just-in-case extension)
; PersistentList -> PersistentList  (slower than the rest as it requires 2 passes)
; LazySeq -> LazySeq
; IPersistentVector -> IPersistentVector
; IPersistentSet -> IPersistentSet
; IPersistentMap -> IPersistentMap
; double-array1D -> double-array1D
; double-array2D -> double-array2D
; float-array1D -> float-array1D
; float-array2D -> float-array2D
; long-array1D -> double-array1D
; long-array2D -> double-array2D
; int-array1D -> float-array1D
; int-array2D -> float-array2D
; Double-array -> Double-array
; Float-array -> Float-array
; Long-array -> Double-array
; Integer-array -> Float-array
;-----------------------------------

(extend-protocol DataSet     
Number
(normalise [this transform]  (transform this))
(getMean [this] (throw (IllegalStateException. "Cannot calculate the mean of a single number!")))  
(getVariance 
  ([this]   (throw (IllegalStateException. "Cannot calculate the variance of a single number!")))
  ([this _] (throw (IllegalStateException. "Cannot calculate the variance of a single number!"))))  
(getStdDeviation 
  ([this]   (throw (IllegalStateException. "Cannot calculate the standard-deviation of a single number!")))
  ([this _] (throw (IllegalStateException. "Cannot calculate the standard-deviation of a single number!"))))  
(getCorrelation 
  ([this other]   (throw (IllegalStateException. "Cannot calculate the correlation-coefficient of a single number!")))
  ([this other _] (throw (IllegalStateException. "Cannot calculate the correlation-coefficient of a single number!"))))      
  
String
(normalise [this stem] (stem this))
(getMean [this]  (throw (IllegalStateException.  "'Mean' only makes sense for numbers!")))
(getVariance 
  ([this]   (throw (IllegalStateException.  "'Variance' only makes sense for numbers!")))
  ([this _] (throw (IllegalStateException.  "'Variance' only makes sense for numbers!"))))  
(getStdDeviation 
  ([this]   (throw (IllegalStateException.  "'Standard-deviation' only makes sense for numbers!")))
  ([this _] (throw (IllegalStateException.  "'Standard-deviation' only makes sense for numbers!"))))  
(getCorrelation 
  ([this other]   (throw (IllegalStateException.  "'Correlation-coefficient' only makes sense for numbers!")))  
  ([this other _] (throw (IllegalStateException.  "'Correlation-coefficient' only makes sense for numbers!"))))     

java.util.Collection ;;if this fires, we're dealing with a Java Collection - return whatever was passed in
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)  
  (reduce 
    (fn [^java.util.Collection c x] 
     (doto c 
      (.add (normalise x #(transform % this)))))  
  (clojure.lang.Reflector/invokeConstructor (class this) (to-array [(.size this)]))  this))) ;;all Collections have a ctor accepting the initial size by convention
(getMean [this]  (help/avg this (.size this)))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (.size this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil))
  ([this other sample?] (help/corr-coefficient this other sample?)))   
  
clojure.lang.ASeq  ;;if this fires, we don't know the type but it doesn't matter - return a lazy-seq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform ) this)
  (map (fn [x] (normalise x #(transform % this))) this)) )
(getMean [this] (help/avg this))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (count this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?)))    

clojure.lang.PersistentList
(normalise
[this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
  (->> (mapv (fn [x] (normalise x #(transform % this))) this)
     rseq
    (into '()))) ) 
(getMean [this] (help/avg this))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (.size this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) 
      
    
clojure.lang.LazySeq
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(map #(normalise % transform) this)
   (map (fn [x] (normalise x #(transform % this))) this)) )
(getMean [this] (help/avg this))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (.size this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) 
  
clojure.lang.APersistentVector
(normalise [this transform]
(if (instance? java.util.Collection (first this))
  (mapv #(normalise % transform) this) 
  (into [] (r/foldcat (r/map (fn [x] (normalise x #(transform % this))) this)))) ) ;;opportunity for parallelism
(getMean [this] (help/avg this))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (count this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?)))       
            
     
clojure.lang.APersistentSet ;;sets are typically not ordered so ordering will dissapear 
(normalise [this transform]
(if (instance? java.util.Collection (first this))
(mapv #(normalise % transform) this)
 (persistent!        
   (reduce (fn [ts x] 
             (conj! ts (normalise x #(transform % this)))) 
     (transient #{}) this))) )
(getMean [this] (help/avg this))
(getVariance 
  ([this]         (getVariance this nil)) 
  ([this sample?] (help/variance this sample? (count this)))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) 
  
  
java.util.Map  ;;again, a just-in-case extension point that delegates to persistent maps
(normalise [this transform]
 (normalise (into {} this) transform))
(getMean [this] (help/avg (into {} this)))
(getVariance 
  ([this]         (getVariance (into {} this) nil)) 
  ([this sample?] (getVariance (into {} this) sample?))) 
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance  this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation (into {} this) other nil)) 
  ([this other sample?] (getCorrelation (into {} this) other sample?)))               
   
clojure.lang.APersistentMap ;;assuming a map with collections for keys AND values (a dataset perhaps?)
(normalise [this transform]
 (persistent!        
   (reduce-kv #(assoc! %1 (normalise %2 transform) 
                          (normalise %3 transform)) (transient {}) this)))
(getMean [this]
  (persistent!        
   (reduce-kv #(assoc! %1 (help/avg %2) 
                          (help/avg %3)) (transient {}) this)))
(getVariance 
  ([this] (getVariance this nil))
  ([this samples?] 
  (persistent!        
   (reduce-kv #(assoc! %1 (help/variance %2 samples?) 
                          (help/variance %3 samples?)) (transient {}) this))))
(getStdDeviation
  ([this] (getStdDeviation this nil)) 
  ([this samples?]
  (persistent!        
   (reduce-kv #(assoc! %1 (Math/sqrt (getVariance %2 samples?)) 
                          (Math/sqrt (getVariance %3 samples?))) (transient {}) this))))
(getCorrelation 
  ([this _] (getCorrelation this _ nil))
  ([this _ samples?] ;;there is no 'other' dataset; each map-entry holds 2 data-sets
   (persistent! 
     (reduce-kv #(conj! % (help/corr-coefficient %2 %3 samples?)) (transient []) this))))  ) 

;;do the same for popular primitive array types   
(extend-protocol DataSet   
(Class/forName "[D")  
(normalise [this transform]
  (amap ^doubles this idx ret (double (normalise (aget ^doubles this idx) #(transform % this)))))
(getMean [this] 
  (/ (areduce ^doubles this i ret 0.0 (+ ret (aget ^doubles this i))) 
     (alength ^doubles this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (let [mean (getMean this)
        intermediates (amap ^doubles this idx ret (Math/pow (- (aget ^doubles this idx) mean) 2))]  
    (/ (areduce ^doubles intermediates i ret 0.0 (+ ret (aget ^doubles intermediates i))) 
       (if sample? (dec (alength ^doubles this)) 
                        (alength ^doubles this))))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )
 
(extend-protocol DataSet   
(Class/forName "[[D")  ;;2d double-arrays are very common
(normalise [this transform]  (into-array (map #(normalise % transform) this)))
(getMean [this] (into-array (map #(getMean %) this)))
(getVariance 
  ([this]  (getVariance this nil))
  ([this sample?] (into-array (map #(getVariance % sample?) this))))
(getStdDeviation 
  ([this] (getStdDeviation this nil))
  ([this sample?] (into-array (map #(getStdDeviation % sample?) this))))
(getCorrelation 
  ([this _] (getCorrelation this _ nil))
  ([this _ sample?] ;;we already have many arrays-let's assume user is asking for the correlation of the 1st array with repsect to all the rest
  (mapv #(help/corr-coefficient [(first this) %] sample?) (next this))))  ) 
   
(extend-protocol DataSet   
(Class/forName "[F")  
(normalise [this transform]
 (amap ^floats this idx ret (float (normalise (aget ^floats this idx) #(transform % this)))))
(getMean [this] 
  (/ (areduce ^floats this i ret (float 0) (float (+ ret (aget ^floats this i)))) 
     (alength ^floats this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength ^floats this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )
 
(extend-protocol DataSet   
(Class/forName "[[F")  ;;2d float-arrays are very common
(normalise [this transform]
  (into-array (map #(normalise % transform) this)))
(getMean [this]
  (into-array (map #(getMean %) this)))
(getVariance 
  ([this]  (getVariance this nil))
  ([this sample?] (into-array (map #(getVariance % sample?) this))))
(getStdDeviation 
  ([this] (getStdDeviation this nil))
  ([this sample?] (into-array (map #(getStdDeviation % sample?) this))))
(getCorrelation 
  ([this _] (getCorrelation this _ nil))
  ([this _ sample?] ;;we already have many arrays-let's assume user is asking for the correlation of the 1st array with repsect to all the rest
  (mapv #(help/corr-coefficient [(first this) %] sample?) (next this))))  )   
   
(extend-protocol DataSet   
(Class/forName "[J")  
(normalise [this transform]
 (normalise (double-array this) transform))
(getMean [this] 
  (/ (areduce ^longs this i ret 0 (+ ret (aget ^longs this i))) 
     (alength ^longs this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength ^longs this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )
 
(extend-protocol DataSet   
(Class/forName "[[J")  ;;2d long-arrays are very common
(normalise [this transform]
 (into-array (map #(normalise % transform) this)))
(getMean [this]
  (into-array (map #(getMean %) this)))
(getVariance 
  ([this]  (getVariance this nil))
  ([this sample?] (into-array (map #(getVariance % sample?) this))))
(getStdDeviation 
  ([this] (getStdDeviation this nil))
  ([this sample?] (into-array (map #(getStdDeviation % sample?) this))))
(getCorrelation 
  ([this _] (getCorrelation this _ nil))
  ([this _ sample?] ;;we already have many arrays-let's assume user is asking for the correlation of the 1st array with repsect to all the rest
  (mapv #(help/corr-coefficient [(first this) %] sample?) (next this))))  )   
      
(extend-protocol DataSet 
(Class/forName "[I")  
(normalise [this transform]
 (normalise (float-array this) transform))  
(getMean [this] 
  (/ (areduce ^ints this i ret (int 0) (+ ret (aget ^ints this i))) 
     (alength ^ints this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength ^ints this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )
  

(extend-protocol DataSet   
(Class/forName "[[I")  ;;2d int-arrays are also very common
(normalise [this transform]
 (into-array (map #(normalise % transform) this))) 
(getMean [this]
  (into-array (map #(getMean %) this)))
(getVariance 
  ([this]  (getVariance this nil))
  ([this sample?] (into-array (map #(getVariance % sample?) this))))
(getStdDeviation 
  ([this] (getStdDeviation this nil))
  ([this sample?] (into-array (map #(getStdDeviation % sample?) this))))
(getCorrelation 
  ([this _] (getCorrelation this _ nil))
  ([this _ sample?] ;;we already have many arrays-let's assume user is asking for the correlation of the 1st array with repsect to all the rest
  (mapv #(help/corr-coefficient [(first this) %] sample?) (next this))))  )    
  
(extend-protocol DataSet   
(Class/forName "[Ljava.lang.Double;")  
(normalise [this transform]
 (amap #^"[Ljava.lang.Double;" this idx ret (normalise (aget #^"[Ljava.lang.Double;" this idx) #(transform % this))))
(getMean [this] 
  (/ (areduce #^"[Ljava.lang.Double;" this i ret 0.0 (+ ret (aget #^"[Ljava.lang.Double;" this i))) 
     (alength #^"[Ljava.lang.Double;" this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength #^"[Ljava.lang.Double;" this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )     
  
(extend-protocol DataSet  
(Class/forName "[Ljava.lang.Long;")  
(normalise [this transform] ;;returns a double-array
  (normalise (double-array this) transform))
(getMean [this] 
  (/ (areduce #^"[Ljava.lang.Long;" this i ret (Long. 0) (+ ret (aget #^"[Ljava.lang.Long;" this i))) 
     (alength #^"[Ljava.lang.Long;" this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength #^"[Ljava.lang.Long;" this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) ) 
  
(extend-protocol DataSet   
(Class/forName "[Ljava.lang.Integer;")  
(normalise [this transform] ;;returns a float-array
 (normalise (float-array this) transform))  
(getMean [this] 
  (/ (areduce #^"[Ljava.lang.Integer;" this i ret (int 0) (int (+ ret (aget #^"[Ljava.lang.Integer;" this i)))) 
     (alength #^"[Ljava.lang.Integer;" this)))
(getVariance 
  ([this]  (getVariance this nil))     
  ([this sample?] 
  (help/variance sample? this (alength #^"[Ljava.lang.Integer;" this))))
(getStdDeviation 
  ([this]         (Math/sqrt (getVariance this)))
  ([this sample?] (Math/sqrt (getVariance this sample?))))  
(getCorrelation 
  ([this other]         (getCorrelation this other nil)) 
  ([this other sample?] (help/corr-coefficient this other sample?))) )  
  
(extend-protocol DataSet   
(Class/forName "[Ljava.lang.String;")  
(normalise [this transform]
 (amap #^"[Ljava.lang.String;" this idx ret (normalise (aget #^"[Ljava.lang.String;" this idx) #(transform % this))))
(getMean [this] (throw (IllegalStateException.  "'Mean' only makes sense for a collection or array of numbers!")))
(getVariance 
  ([this]   (throw (IllegalStateException.  "'Variance' only makes sense for numbers!")))
  ([this _] (throw (IllegalStateException.  "'Variance' only makes sense for numbers!"))))  
(getStdDeviation 
  ([this]   (throw (IllegalStateException.  "'Standard-deviation' only makes sense for numbers!")))
  ([this _] (throw (IllegalStateException.  "'Standard-deviation' only makes sense for numbers!"))))  
(getCorrelation 
  ([this other]   (throw (IllegalStateException.  "'Correlation-coefficient' only makes sense for numbers!")))  
  ([this other _] (throw (IllegalStateException.  "'Correlation-coefficient' only makes sense for numbers!")))) ) 
   
   
;;this is how client code would look like
(def in-range-needs (memo/ttl (juxt #(apply min %) #(apply max %)) :ttl/threshold 3000)) ;the upper/lower data limits

;;IN-RANGE formula  
(defn in-range-formula 
"The most common normalisation technique (for numbers)." 
([x coll [bn tn] tlimits]  
(let [[bi ti] (or tlimits (in-range-needs coll))]  
(+ bn
   (* (/ (- x  bi) 
         (- ti bi)) 
      (- tn bn))) ))
([x coll bottom-top] 
  (in-range-formula x coll bottom-top nil))
([x coll] 
  (in-range-formula x coll [-1 1])) )

;typical in-range transformers
(def transform-in-range1 "In-range [-1 1] transformer." in-range-formula)
(def transform-in-range5 "In-range [-5 5] transformer." #(in-range-formula %1 %2 [-5 5]))
(def transform-in-rangeLAZY "in-range transformer [-1 1] for a big lazy-seq which we can't afford to apply min/max." #(in-range-formula %1 %2 [-1 1] [-50000 50000]))
;(normalise (range -5000000 5000000) transform-in-rangeLAZY)

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
  (divide-by-value-formula x nil 100)) )       

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
(def transform-by-porter-reuse "Porter's normalisation transformer that reuses the same object." #(porter-formula %1 %2 (help/porter-stemmer "english")))

;MULTIPLICATIVE formula
(defn multiplicative-needs* ^double [coll] 
  (/ 1.0 (Math/sqrt (reduce #(+ %1 (* %2 %2)) coll)))) ;;produces the multiplicative normalisation factor
  
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
   
(def z-axis-needs (memo/ttl z-axis-needs* :ttl/threshold 3000)) ;;configure the caching threshold according to your needs 
 
(defn z-axis-formula 
([x coll _]
 (let [[zfactor synthetic-field]  (z-axis-needs coll)] 
  (+ synthetic-field (* x zfactor))))
([x coll] 
 (z-axis-formula x coll nil)) )  
 
 
(comment
;benchmarks

(def sdata (range -500 500))
(def bdata (range -50000 50000))
(def vdata (vec sdata))
(def adata (double-array vdata))

(time (mapv #(transform-in-range5 % vdata ) vdata)) ;;30ms
(time (map #(transform-in-rangeLAZY % bdata [-50000 50000]) bdata))

(time (amap ^doubles adata idx ret (transform-in-range5 (aget ^doubles adata idx))))

(normalise (java.util.Vector. [-5 -4 -3 -2 -1 1 2 3 4 5]))

)    

 
   
