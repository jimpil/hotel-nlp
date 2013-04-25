(ns hotel_nlp.externals.uima
  (:require [clojure.pprint :refer [pprint]]
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
 One could stat playing with ClassLoaders to essentially inject whatever instances he/she wants but that is tricky and could leave to other runtime problems. 
 Starting from uimafit 1.4.0 there is some experimental code (an extended ResourceManager) that allows exactly that. This manager can set its externalContext using
 a Map<String,Object> where we're mapping names object-instances. "
  [context-map]
  (doto (org.uimafit.util.SimpleNamedResourceManager.) 
     (.setAutoWireEnabled true)
     (.setExternalContext context-map)))

(defn ^ResourceSpecifier  xml-resource 
"Parses an xml-descriptor file and returns the ResourceSpecifier object."
 [^String xml-descriptor]
  (let [source (XMLInputSource. xml-descriptor)]
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


(defn produce ;(produce :analysis-engine [(xml-resource "dummy-descriptor.xml")] :par-requests 3)
 "Produce UIMA components according to some ResourceSpecifier objects (which you get from calling (xml-resource some.xml)."
[what specifier & {:keys [resource-manager par-requests timeout] 
                   :or   {timeout 0 resource-manager (doto (UIMAFramework/newDefaultResourceManager) 
                                                       (.setExtensionClassPath dynamic-classloader "" true))}}]
(let [min-params {"TIMEOUT_PERIOD" (int timeout) 
                  ;"NUM_SIMULTANEOUS_REQUESTS" (int par-requests)
                  "RESOURCE_MANAGER" resource-manager}
      additional-params (if par-requests (assoc min-params "NUM_SIMULTANEOUS_REQUESTS" (int par-requests)) min-params)]
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
        type (CasUtil/getAnnotationType cas type)  #_(.getType type-system type-name)]   
 (.addFsToIndexes cas 
    (.createAnnotation cas type begin end))))

(defn select-annotations [^JCas jc ^Class klass start end]
  (JCasUtil/selectCovered jc klass start end))



(defn uima-compatible 
  "Given a component and a function to extract the desired input from the JCas, 
  returns a UIMA compatible oblect that wraps the original component. For now the component must be able to act as a function.
  The fn referred to by 'jcas-input-extractor-string' must accept 2 arguments [JCas, UIMAContext]." 
  [component jcas-input-extractor jcas-writer]
 ; (let [compfn (fn [& args] (apply component args))] 
   (produce :analysis-engine  
    (AnalysisEngineFactory/createPrimitiveDescription UIMAProxy  ;;ALL vars HAVE TO BE IN THE SAME NAMESPACE
       (into-array String  [UIMAProxy/PARAM_ANNFN  (-> component class .getName)  
                            UIMAProxy/PARAM_EXTFN  (-> jcas-input-extractor  class .getName)
                            UIMAProxy/PARAM_POSTFN (-> jcas-writer  class .getName)]))))



#_(let [compa  (uima-compatible art/reg-tok  squeeze-jcas)
      hack   (resource-manager-exp {"pojo" compa})
      desc   (AnalysisEngineFactory/createPrimitiveDescription (class compa) (to-array []))]
 (produce :analysis-engine desc :resource-manager hack))


#_(produce :analysis-engine 
  (-> art/reg-tok 
   (uima-compatible  #(.getDocumentText %) )  ;squeeze-jcas
   class
   (AnalysisEngineFactory/createPrimitiveDescription (to-array [UIMAProxy/PARAM_NS "uimaclj.core.test"
                                                                UIMAProxy/PARAM_ANNFN "uimaclj.core.test"
                                                                UIMAProxy/PARAM_EXTFN  "uimaclj.core.test"]))))
;(resource-manager-exp {"pojo" art/reg-tok})

; (JCasFactory/createJCas (TypeSystemDescriptionFactory/createTypeSystemDescription))
; (doto *1 (.setDocumentText  "My name is Jim and I like pizzas !"))
;(.process my-ae *1)


(comment

(defn extend-uima []
 (extend-type org.apache.uima.analysis_component.JCasAnnotator_ImplBase 
  IComponent
  (run [this jcas] 
    (.process this jcas))) ) 

(load-file "src/hotel_nlp/externals/uima.clj")
(use 'hotel_nlp.externals.uima)
(require '[hotel_nlp.helper :as help])
(def my-tokenizer #(hotel_nlp.concretions.artefacts/reg-tok %)) ;this artefact can act as a fn but is not of type IFn so we need to wrap it
(defn extractor [t _] (.getDocumentText t))
(defn post-fn [jc res original-input]  
 (let [i  (atom 0)]
   (inject-annotation! jc [Annotation 0 (count original-input)]) ;the entire sentence annotation    
 (doseq [^String x res]
   (let [size (count x)] 
  (inject-annotation! jc [Annotation @i (+ @i size)]) ;the token annotations
  (swap! i + (inc size)))
 ) ))
(uima-compatible my-tokenizer extractor post-fn)
(def my-ae *1)
(def sample "My name is Jim and I like pizzas !")
(def jc (org.uimafit.factory.JCasFactory/createJCas))
(.setDocumentText jc  sample)
 (.process my-ae jc)
 hotel_nlp.externals.UIMAProxy/resultMap

 (select-annotations jc Annotation 0 (count sample))

 ;---------

 (def uima-hmm-tagger (doto (HMMTagger.) (.initialize )))


)
