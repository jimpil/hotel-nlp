(ns hotel_nlp.externals.uima
  (:require [clojure.pprint :refer [pprint]]
            [hotel_nlp.helper :as help]
            [hotel_nlp.concretions.models] 
            [hotel_nlp.concretions.artefacts :as art])
  (:import [org.apache.uima UIMAFramework]
  		     [org.apache.uima.jcas JCas]
  	       [org.apache.uima.resource ResourceSpecifier ResourceManager]
  	       [org.apache.uima.util XMLInputSource CasPool]
  	       [org.apache.uima.analysis_engine AnalysisEngine]
           [org.uimafit.component JCasAnnotator_ImplBase]
           [org.uimafit.pipeline SimplePipeline]
           [org.uimafit.util  JCasUtil]
           ;[org.uimafit.type Sentence Token AnalyzedText]
           [org.uimafit.factory TypeSystemDescriptionFactory AnalysisEngineFactory AggregateBuilder CollectionReaderFactory]
           [hotel_nlp.concretions.models  RE-Tokenizer RE-Segmenter] 
  )
)

(defn alt-implementation 
 "Specify an uima implementation other than the default one.
 If you choose to do this, make sure to call this fn before doing anything else." 
 [^String fully-qualified-class-name]
 (.setProperty (System/getProperties) "uima.framework_impl" fully-qualified-class-name))

(definline xml-parser "Get the current XML parser." []
 `(UIMAFramework/getXMLParser))

(definline logger "Get the current logger." []
 `(UIMAFramework/getLogger))

(defn resource-manager 
  (^ResourceManager [] (UIMAFramework/newDefaultResourceManager))
  ([^String data-path] (doto (resource-manager) (.setDataPath data-path))))

(defn ^ResourceSpecifier xml-resource 
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


(defn produce ;(produce :analysis-engine [(xml-resource "dummy-descriptor.xml")] :par-requests 3)
 "Produce UIMA components according to some ResourceSpecifier objects (which you get from 'xml-resource')."
[what specifiers & {:keys [par-requests timeout] :or {timeout 0}}] ;wait forever by default
(for [^ResourceSpecifier sp specifiers] 
(case what
	:analysis-engine (if par-requests (UIMAFramework/produceAnalysisEngine sp par-requests timeout)
		                              (UIMAFramework/produceAnalysisEngine sp))
	:cas-consumer    (if par-requests (UIMAFramework/produceCasConsumer sp par-requests timeout)
		                              (UIMAFramework/produceCasConsumer sp))
	:cas-initializer (if par-requests (UIMAFramework/produceCasInitializer sp par-requests timeout)
									  (UIMAFramework/produceCasInitializer sp))
	:collection-processing-engine (if par-requests (UIMAFramework/produceCollectionProcessingEngine sp par-requests timeout)
		 						                   (UIMAFramework/produceCollectionProcessingEngine sp))
	:collection-reader (if par-requests (UIMAFramework/produceCollectionReader sp par-requests timeout) 
		 								(UIMAFramework/produceCollectionReader sp)))) )

(defn ufit-produce
"Produce UIMA components from your objects without writing any XML descriptors, via uima-fit."
 [& objects]
 (let [type-system (TypeSystemDescriptionFactory/createTypeSystemDescription)
       jcas (org.uimafit.factory.JCasFactory/createJCas type-system)]
   (map #(AnalysisEngineFactory/createPrimitiveDescription (class %) type-system) objects)))

#_(defn ufit-pipeline [jcas annotators]
  (SimplePipeline/runPipeline jcas (into-array AnalysisEngine annotators))
    (for [t (JCasUtil/select jcas Token)] (.getTag t)))

(defn squeeze-jcas [^JCas jcas & classes] 
 (for [c classes
       x (JCasUtil/select jcas c)]
    x))

(defn middle-man [actor tagger input-extractor]
 (proxy [org.uimafit.component.JCasAnnotator_ImplBase][]
   (process [^JCas jc] 
    (let [xs (input-extractor jc)] 
      (actor xs))))) ;;assuming actor is a fn for now
