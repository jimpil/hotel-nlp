(ns hotel_nlp.externals.uima
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :refer [resource]]
            [hotel_nlp.helper :as help]
            [hotel_nlp.concretions.models] 
            [hotel_nlp.concretions.artefacts :as art])
  (:import [org.apache.uima UIMAFramework]
  		     [org.apache.uima.jcas JCas]
           [org.apache.uima.jcas.tcas Annotation]
  	       [org.apache.uima.resource ResourceSpecifier ResourceManager]
  	       [org.apache.uima.util XMLInputSource CasPool]
  	       [org.apache.uima.analysis_engine AnalysisEngine]
           [org.uimafit.component JCasAnnotator_ImplBase]
           [org.uimafit.pipeline SimplePipeline]
           [org.uimafit.util  JCasUtil CasUtil]
           ;[org.uimafit.type Sentence Token AnalyzedText]
           [org.uimafit.component.initialize ConfigurationParameterInitializer]
           [org.uimafit.factory JCasFactory TypeSystemDescriptionFactory AnalysisEngineFactory AggregateBuilder CollectionReaderFactory]
           [hotel_nlp.concretions.models  RE-Tokenizer RE-Segmenter]
           [org.apache.uima.examples.tagger HMMTagger HMMModelTrainer]
           [org.apache.uima.examples.tagger.trainAndTest Token CorpusReader MappingInterface ModelGeneration]
           [org.apache.uima TokenAnnotation SentenceAnnotation]
           [hotel_nlp.externals UIMAProxy]
  )
)

(def ^:dynamic *type-system* (TypeSystemDescriptionFactory/createTypeSystemDescription))
;(ResourceManager/setExtensionClassPath (. (Thread/currentThread) getContextClassLoader) "" true)
(def dynamic-classloader (. (Thread/currentThread) getContextClassLoader))


(defn alt-implementation 
 "Specify an uima implementation other than the default one.
 If you choose to do this, make sure to call this fn before doing anything else." 
 [^String fully-qualified-class-name]
 (.setProperty (System/getProperties) "uima.framework_impl" fully-qualified-class-name))

(definline xml-parser "Get the current XML parser." []
 `(UIMAFramework/getXMLParser))

(definline logger "Get the current logger." []
 `(UIMAFramework/getLogger))

(defn ^ResourceManager resource-manager 
  ([] (UIMAFramework/newDefaultResourceManager))
  ([^String data-path] (doto (resource-manager) (.setDataPath data-path)))
  ([^String data-path parent-loader] (doto (resource-manager data-path ) (.setExtensionClassPath parent-loader "" true))))

(defn resource-manager-exp 
"UIMA does not support injections of pre-existing object instances unless they have been created within the context of UIMA (Clojure records aren't for example).
 One could stat playing with ClassLoaders to essentially inject whatever instances he/she wants but that is tricky and could lead to other runtime problems. 
 Starting from uimafit 1.4.0 there is some experimental code (an extended ResourceManager) that allows exactly that. This manager can set its externalContext using
 a Map<String,Object> where we're mapping names object-instances. "
  [context-map]
  (doto (org.uimafit.util.SimpleNamedResourceManager.) 
     (.setAutoWireEnabled true)
     (.setExternalContext context-map)))

(defn ^ResourceSpecifier xml-resource 
"Parses an xml-descriptor file and returns the ResourceSpecifier object."
 [xml-descriptor-loc]
  (let [source (XMLInputSource. xml-descriptor-loc)]
	(.parseResourceSpecifier (xml-parser) source)))

(defn ^JCas jcas   
"Create a JCas, given an Analysis Engine (ae)." 
[^AnalysisEngine ae] 
 (.newJCas ae))

(defn process-serial 
"Process a bunch of documents using the specified analysis engine and post-processing function (in sequence).
This fn should accept a jcas and should be able to pull the processed data out of it and do whatever it's meant to do."
[^AnalysisEngine ae post-process-fn & documents]
 (let [jcas (jcas ae)]
 (help/with-resources [e ae] #(.destroy %)
 (mapv 
 	(fn [text]
 	  (help/with-resources [j jcas] #(.reset %)
 	    (.setDocumentText j text) 
 	    (.process e j) 
 	    (post-process-fn j)))  
 	documents))))

(defn process-parallel 
"Process a bunch of documents using the specified analysis engine and post-processing function (in parallel).
 This fn should accept a jcas and should be able to pull the processed data out of it and do whatever it's meant to do." 
 [^AnalysisEngine ae post-process-fn ^Integer thread-no & documents]
  (let [cas-pool (CasPool. thread-no ae)]
    (pmap 
    	(fn [text]
    	  (let [cas (.getCas cas-pool 0)]
    	  (try  
    	  	(.setDocumentText cas text) 
    	  	(.process ae cas)
    	  	(post-process-fn cas)
    	  (catch Exception e (.printStackTrace e)) 
    	  (finally (.releaseCas cas-pool cas))))) 
    documents)))


(defn produce-primitive
"Produce UIMA components from your objects without writing any XML descriptors, via uima-fit."
 [& os]
   (map #(AnalysisEngineFactory/createPrimitive (class %) *type-system* (to-array [])) os))


(defn produce
 "Produce UIMA components according to some ResourceSpecifier objects (which you get from calling (xml-resource some.xml)."
[what specifier config-map & {:keys [resource-manager] 
                              :or {resource-manager (doto (UIMAFramework/newDefaultResourceManager) 
                                                      (.setExtensionClassPath dynamic-classloader "" true))}}]
(let [min-params {"TIMEOUT_PERIOD" 0 
                  ;"NUM_SIMULTANEOUS_REQUESTS" (int par-requests)
                  "RESOURCE_MANAGER" resource-manager}
      additional-params (merge min-params config-map)] 
(case what
	:analysis-engine  (UIMAFramework/produceAnalysisEngine specifier additional-params)                           
	:cas-consumer     (UIMAFramework/produceCasConsumer specifier additional-params)
	:cas-initializer  (UIMAFramework/produceCasInitializer specifier additional-params)
	:collection-processing-engine (UIMAFramework/produceCollectionProcessingEngine specifier additional-params)
	:collection-reader (UIMAFramework/produceCollectionReader specifier additional-params)
  :primitive-ae (produce-primitive specifier)))) ;;'specifier' here really means 'object-instance' and not xml as we're going through uima-fit



#_(defn ufit-pipeline [jcas annotators]
  (SimplePipeline/runPipeline jcas (into-array AnalysisEngine annotators))
    (for [t (JCasUtil/select jcas Token)] (.getTag t)))

(defn squeeze-jcas [^JCas jcas & classes] 
 (for [c classes
       x (JCasUtil/select jcas c)]
    x))

(defn inject-annotation! [^JCas jc [^Class type  begin end]]
  (let [cas (.getCas jc)
        type-system (.getTypeSystem jc) 
        type (CasUtil/getAnnotationType cas type)]   
 (.addFsToIndexes cas 
    (.createAnnotation cas type begin end))))

(defn select-annotations [^JCas jc ^Class klass start end]
  (JCasUtil/selectCovered jc klass start end))

(defn calculate-indices 
  [original matches]
  (let [matcher (fn ^java.util.regex.Matcher [p] 
                  (re-matcher (re-pattern p) original))]   
  (with-local-vars [cpos 0]
  (reduce 
    #(assoc %1 %2 
       (let [ma1 (doto (matcher %2) (.find @cpos))
             endpos (.end ma1)]
        (var-set cpos endpos) 
        [(.start ma1) endpos])) 
    {} matches))))


(defn uima-compatible 
  "Given a component and a function to extract the desired input from the JCas, 
  returns a UIMA compatible oblect that wraps the original component. For now the component must be able to act as a function.
  The fn  'jcas-input-extractor' must accept 2 arguments [JCas, UIMAContext]." 
  ([component jcas-input-extractor jcas-writer config-map]
   (produce :analysis-engine  
    (AnalysisEngineFactory/createPrimitiveDescription UIMAProxy 
       (to-array  [UIMAProxy/PARAM_ANNFN  (class component)  
                   UIMAProxy/PARAM_EXTFN  (class jcas-input-extractor)
                   UIMAProxy/PARAM_POSTFN (class jcas-writer)])) config-map))
  ([component jcas-input-extractor jcas-writer] 
    (uima-compatible component jcas-input-extractor jcas-writer {})) )

;(resource-manager-exp {"pojo" art/reg-tok})

; (JCasFactory/createJCas (TypeSystemDescriptionFactory/createTypeSystemDescription))
; (doto *1 (.setDocumentText  "My name is Jim and I like pizzas !"))
;(.process my-ae *1)

(def sample "All plants need light and water to grow!")
(defn hmm-postag
 "A HMM POS-tagger capable of working not only with bigrams(n=2) but also trigrams(n=3).
 The 3rd overload is the important one. It expects the path to the model you want to use and a map from sentences (String) to tokens (seq of Strings).
 If you don't have the tokens for the sentences simply pass nil or an empty seq and UIMA's basic WhiteSpaceTokenizer will be used to create the tokens.
 Theoretically you can mix non-empty token-seqs with  empty ones in the same map - however if that is the case, the tokens that were supplied will be ignored
 only to be re-tokenized by the WhiteSpaceTokenizer. Therefore, it is not suggested that you do that. In fact, it not suggested to use the WhiteSpaceTokenizer 
 for any serious work at all. In addition, you can also pass an extra key :n with value 2 or 3, to specify whether you want bigram or trigram modelling.  
 The other 2 overloads are there to simply deal with a single sentence."
([path-to-model whole-sentence tokens n]  
  (hmm-postag path-to-model {whole-sentence tokens :n n}))  
([path-to-model whole-sentence tokens] 
  (hmm-postag path-to-model whole-sentence tokens 3)) 
([path-to-model sentence-token-map]
  (let [config {"NGRAM_SIZE" (int (or (:n sentence-token-map) 3))
                "ModelFile"  path-to-model}
        proper-map  (dissoc sentence-token-map :n)       ;;remove the [:n x] entry as it will interfere...
        need-aggregate? (some empty? (vals proper-map))  ;;...here
        tagger (if need-aggregate?
                 (produce :analysis-engine (-> "HmmTaggerAggregate.xml" resource xml-resource) config)
                 (produce :analysis-engine (-> "HmmTagger.xml" resource xml-resource) config))       
        jc (jcas tagger) ;;create a JCas from the tagger who knows about the annotation types
        pos-tag (fn [^String s ^JCas jc]
                    (.setDocumentText jc s)
                    (.process tagger jc) 
                      (mapv (fn [^TokenAnnotation ta] (.getPosTag ta)) 
                        (select-annotations jc TokenAnnotation 0 (count s))))]
 (if need-aggregate? 
  (reduce-kv 
    (fn [init sentence tokens]
     (conj init (pos-tag sentence jc))) [] proper-map)        
  (reduce-kv 
    (fn [init sentence tokens] 
     (conj init (do (inject-annotation! jc [SentenceAnnotation 0 (count sentence)])
                    (doseq [[_ [b e]] (calculate-indices sentence tokens)]
                      (inject-annotation! jc [TokenAnnotation b e])) 
                  (pos-tag sentence jc))))
   [] proper-map)))) )

(def postag-brown (partial hmm-postag "/home/sorted/clooJWorkspace/hotel-nlp/resources/pretrained_models/BrownModel.dat"))


(defn load-model [^String filename]
  (HMMTagger/get_model filename)) 

(def my-corpus-reader
 (reify CorpusReader
  (^java.util.List read_corpus [_ ^String filename ^MappingInterface mapi]  ;;mapi can be nil if we don't want any mapping
    (let [ppairs (map #(clojure.string/split % #"/") 
                    (clojure.string/split (slurp filename) #"\n"))]
  (doall (for [[token tag] ppairs]
            (Token. token tag))))) ) )


 #_(defn map->properties 
  ([property-value-map]
    (reduce-kv 
      #(doto %1 (.setProperty %2 %3))
     (java.util.Properties.) property-value-map))
  ([] (map->properties {"MODEL_FILE" ""
                        ;"DO_MAPPING" "true"
                        ;"MAPPING" ""
                        "FILE" "EXAMPLE.dat"
                        "CORPUS_READER"  (-> my-corpus-reader class .getName)
                        "GOLD_STANDARD" ""
                        "N" "3"})))

(comment

(defn extend-uima []
 (extend-type org.apache.uima.analysis_component.JCasAnnotator_ImplBase 
  IComponent
  (run [this jcas] 
    (.process this jcas))) ) 

(load-file "src/hotel_nlp/externals/uima.clj")
(use 'hotel_nlp.externals.uima)
;(require '[hotel_nlp.helper :as help])
(def my-tokenizer #(hotel_nlp.concretions.artefacts/reg-tok %)) ;this artefact can act as a fn but is not of type IFn so we need to wrap it
(defn extractor [t context] (.getDocumentText t))
(defn post-fn [jc res original-input]  
   (inject-annotation! jc [Annotation 0 (count original-input)]) ;the entire sentence annotation    
     (doseq [[_ [b e]] (calculate-indices original-input res)]
      (inject-annotation! jc [Annotation b e])) )



(uima-compatible my-tokenizer extractor post-fn)
(def my-ae *1)
(def sample "My name is Jim and I like pizzas !")
(def jc (JCasFactory/createJCas))
(.setDocumentText jc  sample)
 (.process my-ae jc)
 hotel_nlp.externals.UIMAProxy/resultMap

 (select-annotations jc Annotation 0 (count sample))

 ;--------- 





(.init
 (ModelGeneration. 
   (.read_corpus my-corpus-reader "/home/sorted/clooJWorkspace/hotel-nlp/resources/corpora-train/BROWN-NLTK/nltk_brown_pos.txt" nil) 
   "brown-partial-model.dat"))


;(.newInstance (Class/forName (class my-corpus-reader) true dynamic-classloader))


(defrecord UIMA-HMMTagger [^CorpusReader corpus-reader]   
IProbabilistic
(observe [_  tagged-corpus out-file] 
  (.init
   (ModelGeneration. 
     (.read_corpus corpus-reader tagged-corpus nil)  out-file)))
IModel                                        
 (predict [_ model sentence-tokens-map]
  (hmm-postag model sentence-tokens-map))
IComponent  
(run [this tokens]
 (let [ps (-> this meta :model-file)]
  (if (help/two-d? tokens)  
   (map (partial predict this ps)  tokens)
   (predict this ps tokens))) ) 
(link [this pos other] 
 (Workflow. (help/link this pos other))) ) ;;construct and pass the map containing the vectors instead

 
;"home/sorted/clooJWorkspace/hotel_nlp/resources/corpora-train/BROWN-NLTK/nltk_brown_pos.txt" 
(.init (ModelGeneration. (.read_corpus my-corpus-reader (.getPath (resource "corpora-train/BROWN-NLTK/nltk_brown_pos.txt")) nil)  "EXAMPLE.dat"))
)
