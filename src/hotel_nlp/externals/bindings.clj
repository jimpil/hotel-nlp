(ns hotel_nlp.externals.bindings
    (:require [hotel_nlp.protocols :refer [IComponent IWorkflow appendComponent run deploy link]]
              [hotel_nlp.concretions.models :as models]
              [hotel_nlp.helper :as help]         
    )
    (:import  [hotel_nlp.concretions.models Workflow]    
              ;[opennlp.tools.namefind TokenNameFinder]
              [opennlp.tools.util Span]
              [edu.stanford.nlp.pipeline Annotator AnnotationPipeline StanfordCoreNLP Annotation]
    )
    
)

(def KNOWN-LIBRARIES [:opennlp :stanfordnlp-core :gimli :gate :lingpipe])
  
#_(def RE-NER
(OPENNLP-NER. 
  (opennlp.tools.namefind.RegexNameFinder. (into-array [#"\d+" #"\w+ive?"]))))
  
 #_(-> (opennlp.tools.namefind.DictionaryNameFinder. 
        (opennlp.tools.namefind.Dictionary. 
         (java.io.FileInputStream. "drugbank-opennlp.txt")))         
   (run  ["azestapine" "treatment" "is" "10" "times" "more" "effective" "."]))
   
#_(-> (opennlp.tools.namefind.NameFinderME. 
       (opennlp.tools.namefind.TokenNameFinderModel.
        (java.io.FileInputStream. (clojure.java.io/file 
                                  (clojure.java.io/resource "pretrained_models/opennlp/en-ner-person.bin")))))
  (run ["Mr." "Anderson" "won" "election" "over" "his" "opponent" "Sir." "Brown" "."]))
  
  
     
     
;-------------------------------------------->OPENNLP<---------------------------------------------------------
(definline spans->strings 
"Convert an array of Spans in to an array of Strings." 
 [span-array token-array]
 `(Span/spansToStrings ~span-array ~token-array))

(defn- extend-opennlp-ner 
"Entry point for extending openNLP TokenNameFinder implementations to some key protocols (e.g. IComponent).
 Call this in your own namespace to make all openNLP-NER components symbiotic with cluja components." 
[]
(extend-type opennlp.tools.namefind.TokenNameFinder ;; all NER modules of openNLP
IComponent
(run 
([this token-seq] 
  (run this token-seq nil))
([this token-seq context] 
 (let [array (into-array ^String token-seq)] 
      (if (nil? context) (.find this array) 
                         (.find this array context))  
       array)))
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) ) )
  
(defn- extend-opennlp-pos []
(extend-type opennlp.tools.postag.POSTagger ;; all POS-tagging modules of openNLP
IComponent
(run 
([this token-seq] 
  (run this token-seq nil))
([this token-seq context]
 (if (help/string-array? token-seq) (.tag this token-seq context)    
   (map #(.tag this (if (help/string-array? %) % (into-array %)) context) token-seq))))
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) ) )  
  
(defn- extend-opennlp-tokenizers 
[]
(extend-type opennlp.tools.tokenize.Tokenizer ;; all Tokenizing modules of openNLP
IComponent
(run 
([this s] 
  (run this s false))
([this s offsets?]
  (case offsets?
    true  (if (help/string-array? s) (map #(.tokenizePos this %) s) (.tokenizePos this s))  ;;will return a Span[]
    false (if (help/string-array? s) (map #(.tokenize this %)    s) (.tokenize this s)))))   ;;will return a String[])) ;;will return a String[]
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)



(defn- extend-opennlp-sentDetectors 
[]
(extend-type opennlp.tools.sentdetect.SentenceDetector ;; all Sentence-Detection modules of openNLP
IComponent
(run 
([this s] 
  (run this s false))
([this s offsets?]
  (if offsets? (.sentDetectPos this s) ;;will return a Span[]
               (.sentDetect this s)))) ;;will return a String[]
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)

   
(defn- extend-opennlp-stemmers
[]
(extend-type opennlp.tools.stemmer.Stemmer ;; all stemming modules of openNLP
IComponent
(run 
([this s] 
  (.stem this s))) ;;will return a String
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)

(defn- extend-opennlp-parsers [] ;;all Parsing modules of openNLP
(extend-type opennlp.tools.parser.Parser
IComponent
(run 
([this tokens]  
  (run this tokens nil))
([this tokens nums] 
  (if nums  (.parse this tokens nums) ;;will return a Parse[] 
            (.parse this tokens))))   ;;will return a Parse object
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)

(defn- extend-opennlp-chunkers []
(extend-type opennlp.tools.chunker.Chunker ;; all Chunking modules of openNLP
IComponent
(run [this tokens tags]
  (.chunk this (into-array ^String tokens) 
               (into-array ^String tags))) ;;will return a String[]
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)

;(defn- extend-openlp-coref []
;(extend-type opennlp.tools.coref.Linker 
;IComponent
;(run [this text]
;)


(def OPENNLP-extensions 
"All the available extensions for openNLP so far paired with the fns to call in your own namespace in order to register the extension points."
  {:ner         extend-opennlp-ner
   :pos         extend-opennlp-pos
   :tokenize    extend-opennlp-tokenizers
   :parse       extend-opennlp-parsers
   :sent-detect extend-opennlp-sentDetectors
   :stem        extend-opennlp-stemmers
   :chunk       extend-opennlp-chunkers
   ;:coref      nil 
   }) ;TODO
   
(def opennlp-simple-tok (opennlp.tools.tokenize.SimpleTokenizer/INSTANCE))   

(defn opennlp-me-ssplit 
([] (opennlp-me-ssplit "pretrained_models/opennlp/en-sent.bin"))
([model-resource-path] 
 (opennlp.tools.sentdetect.SentenceDetectorME. 
   (opennlp.tools.sentdetect.SentenceModel.
   (java.io.FileInputStream. (clojure.java.io/file 
   (clojure.java.io/resource model-resource-path)))))))

(defn opennlp-me-pos 
([] (opennlp-me-pos "pretrained_models/opennlp/en-pos-maxent.bin"))
([model-resource-path] 
 (opennlp.tools.postag.POSTaggerME. 
   (opennlp.tools.postag.POSModel.
   (java.io.FileInputStream. (clojure.java.io/file 
   (clojure.java.io/resource model-resource-path)))))))

(defn opennlp-me-ner 
([] (opennlp-me-ner "pretrained_models/opennlp/en-ner-person.bin"))
([model-resource-path] 
 (opennlp.tools.namefind.NameFinderME. 
 (opennlp.tools.namefind.TokenNameFinderModel.
  (java.io.FileInputStream. (clojure.java.io/file 
  (clojure.java.io/resource model-resource-path)))))))                              
  
(defn extend-opennlp
 "Extend the specified modules from openNLP. 
 (keys OPENNLP-extensions) will give you the available options.
  Specify :all as the first module to extend everything." 
 [& modules] 
  (if (= (first modules) :all) 
   (doseq [[_ f] OPENNLP-extensions] (f)) ;;extend everything we've got so far!
   (doseq [m modules]
     (if-let [f (get OPENNLP-extensions m)] (f)  ;;extension exists - invoke it
      (throw (IllegalArgumentException. (str "Module " m " is not a valid openNLP module or it has not been extended yet..."))))))) 
;--------------------------------------------------------------------------------------------------------------------------------------------- 
;----------------------------------------------------->STANFORDNLP-core<---------------------------------------------------------------------------  
  
;edu.stanford.nlp.pipeline.Annotator 
;The backbone of the CoreNLP package is formed by two classes: Annotation and Annotator. Annotations are the data structure which hold the results of annotations. Annotations are basically maps, from keys to bits of the annotation, such as the parse, the part-of-speech tags, or named entity tags. Annotators are a lot like functions, except that they operate over Annotations instead of Objects. They do things like tokenize, parse, or NER tag sentences. Annotators and Annotations are integrated by AnnotationPipelines, which create sequences of generic Annotators. Stanford CoreNLP inherits from the AnnotationPipeline class, and is customized with NLP Annotators 

(def stanford-corenlp-annotators
"A mapping between the available core-nlp annotators and corresponding Annotation type classes (for the produced annotations)." 
 {:tokenize edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation
  :ssplit   edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation 
  :pos      edu.stanford.nlp.ling.CoreAnnotations$PartOfSpeechAnnotation
  :lemma    edu.stanford.nlp.ling.CoreAnnotations$LemmaAnnotation
  :token    edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation
  :ner      edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation 
  :regexner edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation
  :truecase edu.stanford.nlp.ling.CoreAnnotations$TrueCaseAnnotation
  :parse    edu.stanford.nlp.trees.TreeCoreAnnotations$TreeAnnotation
  :graph    edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations$CollapsedCCProcessedDependenciesAnnotation
  :dcoref   edu.stanford.nlp.dcoref.CorefCoreAnnotations$CorefChainAnnotation})
  
(defn new-pipeline-raw 
"Create a raw pipeline from Annotators. 
 Preferably use the 'new-coreNLP' fn passing a properties file as shown on their website." 
 [& annotators]
{:pre [(every? #(instance? Annotator %) annotators)]}
 (let [apl (AnnotationPipeline.)]
   (doseq [a annotators]
     (.addAnnotator apl a))
  apl))
  
(definline new-coreNLP 
"The proper way of creating stanford pipelines. Need to pass a property file for which a helper fn exists ('new-properties')." 
[props]
 `(StanfordCoreNLP. ~props))   
  
(defn new-properties 
"Helper fn for creating properties-file the way stanfordnlp expects. 
 For a pipeline of annotators pass nil as the first argument and the annotator-modules you wish to link in String format.
 Hint: use (keys stanford-corenlp-annotators) to see the available annotator-modules." 
[^String property & annotators]
 (doto (java.util.Properties.)
  (.setProperty (or property "annotators") (apply str (interpose ", " annotators)))))
  
(defn squeeze-annotation 
"An attempt to squeeze as much as possible from the Annotation object in a friendly structure to work with from Clojure (a map of course!).
 nils will be in place of what was not asked fromt he anootations process in the first place." 
 [^edu.stanford.nlp.pipeline.Annotation annotation]
 (let [sentences   (.get annotation edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation)
       token-anns  (for [s    sentences 
                         tok (.get s edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation)] tok)
       tokens (for [t token-anns] (.get t edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation))
       pos (for [t token-anns] (.get t edu.stanford.nlp.ling.CoreAnnotations$PartOfSpeechAnnotation))
       lemmas  (for [t token-anns]   (.get t edu.stanford.nlp.ling.CoreAnnotations$LemmaAnnotation))
       entities (for [t token-anns]  (.get t edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation))
       parse-tree (for [s sentences] (.get s edu.stanford.nlp.trees.TreeCoreAnnotations$TreeAnnotation))
       dependency-graph (for [s sentences] (.get s edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations$CollapsedCCProcessedDependenciesAnnotation))
       coref (.get annotation edu.stanford.nlp.dcoref.CorefCoreAnnotations$CorefChainAnnotation)] 
  {:sentences (for [s sentences] (.get s edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation))
   :tokens tokens
   :lemmas lemmas
   :pos pos 
   :ner entities
   :tree parse-tree
   :graph dependency-graph
   :coref coref})) 
   
(def ^:private co-stub
'(run [this ^String text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann)))     
      

(defn extend-stanford-core 
"A single fn to extend to all stanford-corenlp modules (or 'annotators' as they call them). "
[]
(extend-type edu.stanford.nlp.pipeline.AnnotationPipeline
IWorkflow
(deploy [this ^String text] 
  (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
(appendComponent [this co]  
  (.addAnnotator this (reify edu.stanford.nlp.pipeline.Annotator 
  			(annotate [this annotation] 
  			  (run co annotation))))) )
(extend-protocol IComponent  ;;enumerate them individually - there is no other way to have them NOT satisfy IWorkflow
edu.stanford.nlp.pipeline.POSTaggerAnnotator      
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.PTBTokenizerAnnotator  
(run [this text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.WordsToSentencesAnnotator 
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann)) 
edu.stanford.nlp.pipeline.CleanXmlAnnotator       
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.MorphaAnnotator         
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann)) 
edu.stanford.nlp.pipeline.NERCombinerAnnotator   
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.RegexNERAnnotator       
(run [this text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann)) 
edu.stanford.nlp.pipeline.TrueCaseAnnotator       
(run [this text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.ParserAnnotator         
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
edu.stanford.nlp.pipeline.DeterministicCorefAnnotator 
(run [this  text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann)) )   			  
)
  
#_(extend-type edu.stanford.nlp.pipeline.StanfordCoreNLP
IComponent
 (run [this ^String text] 
  (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
    (.annotate this ann) ann)))
     
;(def props (new-properties "annotators" "tokenize" "ssplit" "pos" "lemma" "regexner" "parse" "dcoref")) 
;(def annotator (new-coreNLP props)) 


 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
   
