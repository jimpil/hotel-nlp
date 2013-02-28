(ns hotel_nlp.concretions.models
    (:require [clojure.string  :refer [split-lines split blank?]]
              [clojure.pprint  :refer [pprint print-table]]
              [hotel_nlp.protocols :refer :all]
              [hotel_nlp.helper    :as help]
              [hotel_nlp.algorithms.viterbi :as vit]
              [hotel_nlp.algorithms.levenshtein :as lev]
              [hotel_nlp.algorithms.ngrams :refer [ngrams*]]
    )
)


;;some preparation first to make our life easier later on
(extend-type String
 IStemmable
 (stem 
  ([this] (stem this "english"))
  ([this lang]
     (let [stemmer (help/porter-stemmer lang)]
        (doto stemmer 
                (.setCurrent this) 
                (.stem)) 
          (.getCurrent stemmer))))
 (getRoot [this _ dic] (get dic this "NOT-FOUND!"))
 IDistance
 (getDistance
   ([this other] 
      (lev/levenshtein-distance this other))
   ([this other mismatch-weight] 
      (lev/levenshtein-distance this other mismatch-weight)))
 IGram
  (ngrams [this n] 
    (ngrams* this n));;character n-grams
 ITaggable
 (tag [this _ t-scheme] (str (:opening t-scheme) (:middle t-scheme) this (:closing t-scheme)))) 
  

 ; Allignable
 ;  (allignSW [this other] 
 ;    (alli/smith_waterman this other)))
     
(extend-type clojure.lang.IPersistentCollection
 IStemmable
 (stem
  ([this] (stem this "english"))
  ([this lang]
    (let [stemmer (help/porter-stemmer lang)]
      (map #(do 
              (doto stemmer 
                  (.setCurrent %) 
                  (.stem))
              (.getCurrent stemmer)) this))))
 IGram
  (ngrams [this n]
    (if (help/two-d? this) (map #(ngrams % n) this) 
      (ngrams* this n)))) ;token n-grams

(extend-type nil
 IStemmable
  (stem                             
  ([_ ] nil)
  ([_ _] nil)))
  
  
;------------------------------------------------------------------------------------------------
(declare addC removeC replaceC)
  
(defrecord Workflow [components] ;;a seq of components - the workflow can itself be a component
 IWorkflow
 (getComponents [this] components) ;;just return all components
 (appendComponent [this c]   (conj (vec components) c))
 (addComponent [this pos c]  (addC components pos c))
 (removeComponent [this pos] (removeC components pos))
 (replaceComponent [this pos replacement] (replaceC components pos replacement))
 (deploy [_ text intermediates?] 
    ((if intermediates? reductions reduce) 
      (fn [init c] (run c init)) text components))
 (deploy [this text] (deploy this text false))
 ;(deploy [this] (deploy this))                         
IComponent
 (link [this pos other]
   (Workflow. (help/link this pos other)))
 ;(run [this] (deploy this))   
 (run [this text]  (reduce #(run %2 %) text components))
 ;(run [this text & more] (deploy this text (first more))) 
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (deploy this arg))
  (applyTo [this args]
    (apply deploy this args)) ) 
    
(defn- addC [cs pos c]
  (Workflow. (help/insert-at cs pos c))) 
  
(defn- removeC [cs pos]
  (Workflow. (help/remove-at cs pos)))
  
(defn- replaceC [cs pos c]
{:pre [(pos? pos) (<= pos (count cs))]} 
  (Workflow. (assoc (vec cs) (dec pos) c)))       

;----------------------------------<REGEX-MODELS>-----------------------------------------------------
(defrecord RE-Segmenter [regex input output] 
ISegmenter
(segment [_ text] 
  (split text regex))
IComponent 
(getIOTypes [_] {:input  input 
                 :output output}) 
(link [this pos other]
  (help/linkage this pos other))
(run [this s] 
(if (string? s) 
  (segment this s) 
  (map #(segment this %) s)))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (segment this arg))
  (applyTo [this args]
    (apply segment this args))   )

(defrecord RE-Combiner [input output] 
ISticker
(stick [_ text] 
  (help/join text))
(stick [_ text separator] 
  (help/join text separator))
IComponent 
(getIOTypes [_] {:input  input 
                 :output output}) 
(link [this pos other]
  (help/linkage this pos other))
(run [this strings] (stick this strings))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (stick this arg))
  (applyTo [this args]
    (apply stick this args))  )
  
(defrecord RE-Tokenizer [regex input output]
ITokenizer
(tokenize [_ sentence] 
  (re-seq regex sentence))
IComponent
(getIOTypes [_] {:input  input 
                 :output output}) 
(link [this pos other]
  (help/linkage this pos other)) 
(run [this sentence] 
  (if (string? sentence) 
  (tokenize this sentence) 
  (map #(tokenize this %) sentence)))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (tokenize this arg))
  (applyTo [this args]
    (apply tokenize this args))  ) 
;----------------------------------------------------------------------------------------------------

(defrecord Ngrams [n input output]
IComponent
(getIOTypes [_] {:input  input 
                 :output output})  
(run [_ s]  (ngrams s n))
(link [this pos other] 
 (help/linkage this pos other))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (run this arg))
  (applyTo [this args]
    (apply run this args)) )


(defrecord PorterStemmer [lang input output]
IStemmable
(stem [_ token] 
  (stem token lang)) ;delegate to string for this
(getRoot [_ token dic] 
  (getRoot token _ dic)) ;delegate to string for this
IComponent 
(getIOTypes [_] {:input  input 
                 :output output}) 
(link [this pos other]
  (help/linkage this pos other))
(run [this s] ;(apply stem this s))) 
  (if (help/two-d? s) (map #(stem this %) s) 
  (stem this s)))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (stem this arg))
  (invoke [this arg1 arg2]
    (getRoot this arg1 arg2))  
  (applyTo [this args]
    (apply stem this args))  )
  
(defrecord LevenshteinDistance [input output]
IDistance
(getDistance [_ s1 s2] 
  (getDistance s1 _ s2))       ;;delegate to string for this
(getDistance [_ s1 s2 weight] 
  (getDistance s1 _ s2 weight)) ;;delegate to string for this 
IComponent
(getIOTypes [_] {:input  input 
                 :output output})  
(run [this args] 
  (apply getDistance this args)) ;;args must be a seq
(link [this pos other] 
 (Workflow. (help/link this pos other)))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg1 arg2]
    (getDistance this arg1 arg2))
  (invoke [this arg1 arg2 arg3]
    (getDistance this arg1 arg2 arg3))   
  (applyTo [this args]
    (apply getDistance this args)) )  
  
;----------------------------------------<PARALLEL MAPPERS>----------------------------------------------  
(defrecord MapReduceMapper [partition-no]
IParallelMapper
 (par-map [_ f coll] 
  (cond 
    (ifn? f) (help/mapr f coll partition-no)
    (satisfies? IComponent f) (help/mapr #(run f %) coll partition-no)
  :else (throw (IllegalArgumentException. "You can only map regular functions or IComponents..."))
   )))

(defrecord ForkJoinMapper [fj-size]
IParallelMapper
(par-map [_ f coll] (help/rmap f coll fj-size)))

(defrecord PoolMapper [thread-no]
IParallelMapper
(par-map [_ f coll] (help/pool-map f coll thread-no)))

(defrecord SemiLazyMapper []
IParallelMapper
(par-map [_ f coll] (pmap f coll)))  
;--------------------------------------------------------------------------------------------------------
#_(defrecord PDFripper [source destination]
IRipper
(rip [this start-page end-page] 
  (help/pdf->text source :s-page start-page :e-page end-page))
IExecutable
(execute [this args] (apply help/pdf->text source args))) ;args should be like {:s-page 5 :e-page 100}
;------------------------------------------------------------------------------------------------------

;;provide extracted probabilities in meta-data key :probs
(defrecord HMM-POS-tagger [prob-extractor ;(comp vit/proper-statistics vit/tables) - a fn which will return a seq containing '(:states :inits :emmission :transition)
                          algo]           ; vit/viterbi 
IProbabilistic
(observe [_ tagged-corpus] 
 (let [[states starts emms trans] (prob-extractor tagged-corpus)]   ;; we got corpus - need to extract probabilities
  (vit/make-hmm states starts emms trans))) ;;returns a map/record of probabilities (the actual model)
(observe [this tagged-corpus probs]  
 (if (and (nil? tagged-corpus) ;pass nil to start from scratch
          (not (nil? probs))) ;;if we don't have corpus, we must have probabilities
   (let [{states :states 
         starts  :init-probs  
         emms    :emission-probs
         trans   :state-transitions} probs]    ;; we got probabilities, not corpus - no need to extract anything
     (vit/make-hmm states starts emms trans))  ;;we got an actual corpus   
 (help/deep-merge-with #(if (vector? %) % (+ % %2)) probs (observe this tagged-corpus)))) ;TODO  ;;cannot add new states at the moment, only update probabilities
IModel                                        
 (predict [this probs tokens]
  (if (help/two-d? tokens)  
  (map #(try (algo probs %)
         (catch ClassCastException cce ;;someone is trying to use the raw map containing the sets!  
          (predict this (observe this nil probs) %))) tokens) 
   (try (algo probs tokens)
   (catch ClassCastException cce ;;someone is trying to use the raw map containing the sets!  
     (predict this (observe this nil probs) tokens)))))
IComponent  
(run [this tokens]
 (let [ps (-> this meta :probs)]
  (if (help/two-d? tokens)  
   (map (partial predict this ps)  tokens)
   (predict this ps tokens))) ) 
(link [this pos other] 
 (Workflow. (help/link this pos other))) ) ;;construct and pass the map containing the vectors instead
 
;----------------------------------------------------------------------------------------------------------

#_(defrecord PAnnotator [data-file write-mode par-strategy]
IAnnotator
 (annotate [this e-type]
  (help/annotate {}))


)
   

    
