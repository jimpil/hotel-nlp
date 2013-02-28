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
        (java.io.FileInputStream. (java.io.File. 
                                  (clojure.java.io/resource "pretrained_models/opennlp/en-ner-person.bin")))))
  (run ["Mr." "Anderson" "won" "election" "over" "his" "opponent" "Sir." "Brown" "."])) 
     
     
;-------------------------------------------->OPENNLP<---------------------------------------------------------
(defn spans->strings 
"Convert an array of Spans in to an array of Strings." 
 [^"[Lopennlp.tools.util.Span;" span-array ^"[Ljava.lang.String;" token-array]
 (Span/spansToStrings  span-array  token-array))
 
(defn parse->string [topParses]
(if (seq? topParses)  
  (doseq [p topParses] (.show ^opennlp.tools.parser.Parse p)) 
(.show ^opennlp.tools.parser.Parse topParses))) 

(defn chunk->spans [^opennlp.tools.chunker.Chunker chunker ^"[Ljava.lang.String;" tokens ^"[Ljava.lang.String;" tags]
(.chunkAsSpans chunker tokens tags))


(defn- extend-opennlp-ner 
"Entry point for extending openNLP TokenNameFinder implementations to some key protocols (e.g. IComponent).
 Call this in your own namespace to make all openNLP-NER components symbiotic with cluja components." 
([string-extractor]
(extend-type opennlp.tools.namefind.TokenNameFinder ;; all NER modules of openNLP
IComponent
(run 
([this token-seq] 
  (run this token-seq nil))
([this token-seq context]
<<<<<<< Updated upstream
(let [pf (or string-extractor (fn [spans _] spans))] ;;decide what fn to use up-front
 (if-not context
 (if (help/two-d? token-seq) 
 (map #(pf (.find this (into-array ^String %)) (into-array ^String %)) token-seq)
  (let [^"[Ljava.lang.String;" tok-array (into-array ^String token-seq)] 
=======
(let [pf (if spans? (fn [spans _] spans) spans->strings)] ;;decide what fn to use up-front
 (if-not context
 (if (help/two-d? token-seq) 
 (map #(pf (.find this (into-array ^String %)) (into-array ^String %)) token-seq)
  (let [tok-array (into-array ^String token-seq)] 
>>>>>>> Stashed changes
      #(pf (.find this tok-array) tok-array)))
 (if (help/two-d? token-seq) 
 (map #(pf (.find this (into-array ^String %) context) (into-array ^String %)) token-seq)
   (let [tok-array (into-array ^String token-seq)] 
      #(pf (.find this tok-array context) tok-array)))  
 ))) )
(link [this pos other] 
  (help/linkage this pos other)) ))
([] (extend-opennlp-ner nil)) ) 
  
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
  (help/linkage this pos other)) ) )  
  
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
    false (if (help/string-array? s) (map #(.tokenize this %)    s) (.tokenize this s)))))   ;;will return a String[]
(link [this pos other] 
  (help/linkage this pos other)) )
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
  (help/linkage this pos other)) )
)

   
(defn- extend-opennlp-stemmers
[]
(extend-type opennlp.tools.stemmer.Stemmer ;; all stemming modules of openNLP
IComponent
(run 
([this s] 
  (.stem this s))) ;;will return a String
(link [this pos other] 
  (help/linkage this pos other)) )
)

(defn- -parse "I hate this code!" [obj tokens nums]
(if nums 
(let [^String sentence (apply str (interpose " " tokens))]
(.parse ^opennlp.tools.parser.Parser obj       
 (loop [start 0 i 0
 	p (opennlp.tools.parser.Parse. sentence 
           		      (opennlp.tools.util.Span. 0 (.length sentence)) 
            			 opennlp.tools.parser.AbstractBottomUpParser/INC_NODE 0.0 0)
        ts tokens]
   (if (empty? ts) p
   (recur (+ (inc start) (count (first ts))) (inc i) (doto p (.insert
                                              (opennlp.tools.parser.Parse. sentence 
                                                (opennlp.tools.util.Span. start (+ start (count (first ts)))) 
                                                 opennlp.tools.parser.AbstractBottomUpParser/TOK_NODE 0.0 i))) (next ts)))) nums)  )
(let [^String sentence (apply str (interpose " " tokens))]
 (.parse ^opennlp.tools.parser.Parser obj       
 (loop [start 0 i 0
 	p (opennlp.tools.parser.Parse. sentence 
           		      (opennlp.tools.util.Span. 0 (.length sentence)) 
            			 opennlp.tools.parser.AbstractBottomUpParser/INC_NODE 0.0 0)
        ts tokens]
   (if (empty? ts) p
   (recur (+ (inc start) (count (first ts))) (inc i) (doto p (.insert
                                              (opennlp.tools.parser.Parse. sentence
                                                (opennlp.tools.util.Span. start (+ start (count (first ts)))) 
                                                  opennlp.tools.parser.AbstractBottomUpParser/TOK_NODE 0.0 i))) (next ts))))))))

(defn- extend-opennlp-parsers [] ;;all Parsing modules of openNLP
(extend-type opennlp.tools.parser.Parser
IComponent
(run 
([this tokens]
(if (help/two-d? tokens)
 (map  #(run this %) tokens)  
 (-parse this tokens nil)))
([this tokens nums]
(if (help/two-d? tokens) 
  (map  (if (> nums 1) #(run this % nums) #(run this %)) tokens) 
  (-parse this tokens nums))))
(link [this pos other] 
  (help/linkage this pos other)) ) 
)

(defn- extend-opennlp-chunkers 
([span-extractor]
(extend-type opennlp.tools.chunker.Chunker ;; all Chunking modules of openNLP
IComponent
(run [this tokens tags]
 (let [pf (or span-extractor #(.chunk ^opennlp.tools.chunker.Chunker  %1 ^"[Ljava.lang.String;" %2 ^"[Ljava.lang.String;" %3))]
 (if (and (help/two-d? tokens) (help/two-d? tags)) (map #(run this % %2) tokens tags)     
  (pf this (if (help/string-array? tokens) tokens (into-array ^String tokens)) 
            (if (help/string-array? tags)   tags   (into-array ^String tags)))))) ;;will return a String[]
(link [this pos other] 
  (help/linkage this pos other)) ))
([] (extend-opennlp-chunkers nil))  
)

(defn- extend-openlp-coref []
(extend-type opennlp.tools.coref.Linker 
IComponent
(run [this ^opennlp.tools.parser.Parse parse [entity spans sent-no]] 
 (if (try (seq? parse) (catch IllegalArgumentException ile false))  
 (.getEntities this  (into-array 
   (reduce #(let [temp %] (java.util.Collections/addAll % %2) %) (java.util.ArrayList.) 
     (map #(run this %1 [entity (nth spans %2) %2]) parse (range)))))   
 (do (println (seq spans)) ;(opennlp.tools.parser.Parse/addNames entity spans (.getTagNodes  parse))
 (for [s spans]  
        (opennlp.tools.parser.Parse/addNames entity s (.getTagNodes  parse)))
(let [extents (.getMentions (.getMentionFinder this) (opennlp.tools.coref.mention.DefaultParse. parse sent-no))] 
(doseq [^opennlp.tools.coref.mention.Mention ex extents]
 (when (nil? (.getParse ex))
   (let [dfp  (opennlp.tools.parser.Parse. (.getText parse) (.getSpan ex) "NML" 1.0 0)]
     (.insert parse dfp)
     (.setParse ex (opennlp.tools.coref.mention.DefaultParse. dfp sent-no))))) extents))))
(link [this pos other] 
  (help/linkage this pos other))))


(def OPENNLP-extensions 
"All the available extensions for openNLP so far paired with the fns to call in your own namespace in order to register the extension points."
  {:ner         extend-opennlp-ner
   :pos         extend-opennlp-pos
   :tokenize    extend-opennlp-tokenizers
   :parse       extend-opennlp-parsers
   :sent-detect extend-opennlp-sentDetectors
   :stem        extend-opennlp-stemmers
   :chunk       extend-opennlp-chunkers
   :coref       extend-openlp-coref 
   }) ;TODO
   
(def opennlp-simple-tok (opennlp.tools.tokenize.SimpleTokenizer/INSTANCE))   

(defn opennlp-me-ssplit 
([] (opennlp-me-ssplit "resources/pretrained_models/opennlp/en-sent.bin"))
([model-resource-path] 
 (opennlp.tools.sentdetect.SentenceDetectorME. 
   (opennlp.tools.sentdetect.SentenceModel.
   (java.io.FileInputStream. 
   (java.io.File.  model-resource-path))))))

(defn opennlp-me-pos 
([] (opennlp-me-pos "resources/pretrained_models/opennlp/en-pos-maxent.bin"))
([model-resource-path] 
 (opennlp.tools.postag.POSTaggerME. 
   (opennlp.tools.postag.POSModel.
   (java.io.FileInputStream.
    (java.io.File. model-resource-path))))))

(defn opennlp-me-ner 
([] (opennlp-me-ner "resources/pretrained_models/opennlp/en-ner-person.bin"))
([model-resource-path] 
 (opennlp.tools.namefind.NameFinderME. 
 (opennlp.tools.namefind.TokenNameFinderModel.
  (java.io.FileInputStream. (java.io.File.  model-resource-path))))))
  
(defn opennlp-me-chunk 
([] (opennlp-me-chunk "resources/pretrained_models/opennlp/en-chunker.bin")) 
([model-path] 
 (opennlp.tools.chunker.ChunkerME.
   (opennlp.tools.chunker.ChunkerModel.
    (java.io.FileInputStream. (java.io.File. model-path)))))) 
  
(defn opennlp-me-parse 
([] (opennlp-me-parse "resources/pretrained_models/opennlp/en-parser-chunking.bin")) 
([model-path] 
 (opennlp.tools.parser.ParserFactory/create
   (opennlp.tools.parser.ParserModel.
    (java.io.FileInputStream. (java.io.File. model-path)))))) 
    
(defn opennlp-me-coref 
([] (opennlp-me-coref "resources/pretrained_models/opennlp/coref/opennlp.sourceforge.net/models-1.4/english/coref/")) 
([^String models-path] 
 (opennlp.tools.lang.english.TreebankLinker. models-path  opennlp.tools.coref.LinkerMode/TEST)))                                 
  
#_(defn extend-opennlp
 "Extend the specified modules from openNLP. 
 (keys OPENNLP-extensions) will give you the available options.
  Specify :all as the first module to extend everything." 
 [& modules]  [& {:keys [modules] :or {modules :all}}]
  (if (= (first modules) :all) 
   (doseq [[_ f] OPENNLP-extensions] (f)) ;;extend everything we've got so far!
   (doseq [m modules]
     (if-let [f (get OPENNLP-extensions m)] (f)  ;;extension exists - invoke it with no args
      (throw (IllegalArgumentException. (str "Module " m " is not a valid openNLP module or it has not been extended yet..."))))))) 
      
(defn extend-opennlp
 "Extend the specified modules from openNLP. 
 (keys OPENNLP-extensions) will give you the available options.
  Specify :all as the first module to extend everything." 
 [& {:keys [modules] 
     :or {modules :all}}]
  (if (= modules :all) 
   (doseq [[_ f] OPENNLP-extensions] (f)) ;;extend everything we've got so far!
   (doseq [[m & params] modules]
     (if-let [f (get OPENNLP-extensions m)] (apply f params)  ;;extension exists - invoke it
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
[^java.util.Properties props]
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
 (let [^java.util.List sentences   (.get annotation edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation)
       token-anns  (for [s    sentences 
                         tok (.get ^edu.stanford.nlp.util.CoreMap s edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation)] tok)
       tokens (for [t token-anns] (.get ^edu.stanford.nlp.ling.CoreLabel t edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation))
       pos (for [t token-anns] (.get ^edu.stanford.nlp.ling.CoreLabel t edu.stanford.nlp.ling.CoreAnnotations$PartOfSpeechAnnotation))
       lemmas  (for [t token-anns]   (.get ^edu.stanford.nlp.ling.CoreLabel t edu.stanford.nlp.ling.CoreAnnotations$LemmaAnnotation))
       entities (for [t token-anns]  (.get ^edu.stanford.nlp.ling.CoreLabel t edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation))
       parse-tree (for [s sentences] (.get ^edu.stanford.nlp.ling.CoreLabel s edu.stanford.nlp.trees.TreeCoreAnnotations$TreeAnnotation))
       dependency-graph (for [s sentences] (.get ^edu.stanford.nlp.util.CoreMap s edu.stanford.nlp.trees.semgraph.SemanticGraphCoreAnnotations$CollapsedCCProcessedDependenciesAnnotation))
       coref (.get annotation edu.stanford.nlp.dcoref.CorefCoreAnnotations$CorefChainAnnotation)] 
  {:sentences (for [s sentences] (.get ^edu.stanford.nlp.util.CoreMap s edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation))
   :tokens tokens
   :lemmas lemmas
   :pos pos 
   :ner entities
   :tree parse-tree
   :graph dependency-graph
   :coref coref})) 

;;inspired from clojure/core/protocols.clj    
(def ^:private co-stub
'(run [this ^String text] 
   (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))) 

(defn- emit-IComponent-impls* [syms]
 (apply concat
  (map
    (fn [s]
      [(symbol (str "edu.stanford.nlp.pipeline." s)) co-stub])
    syms)) ) 

(defmacro ^:private emit-IComponent-impls [& syms]
  `(extend-protocol IComponent
     ~@(emit-IComponent-impls* syms)))
     
<<<<<<< Updated upstream
(defn extract-annotators [^StanfordCoreNLP stanford-pipe individuals?]
=======
(defn extract-annotators [stanford-pipe individuals?]
>>>>>>> Stashed changes
 (let [properties  (.getProperties stanford-pipe)
       an-string   (.getProperty ^java.util.Properties properties "annotators")
       individuals (.split ^String an-string "[, \t]+")] ;;split the string exactly how they split it 
 (if-not individuals? an-string
 (for [an-name individuals]
  (edu.stanford.nlp.pipeline.StanfordCoreNLP/getExistingAnnotator an-name)))))             
      

(defn extend-stanford-core 
"A single fn to extend to all stanford-corenlp modules (or 'annotators' as they call them). "
[]
(extend-type edu.stanford.nlp.pipeline.StanfordCoreNLP
IWorkflow
(deploy [this ^String text] 
  (let [ann (edu.stanford.nlp.pipeline.Annotation. text)]
     (.annotate this ann) ann))
(appendComponent [this co]  ;;appending components is generally not supported by stanfordNLP pipes- you will most likely get a concrete Workflow object back
(let [ann-string  (extract-annotators this false)]
 (cond 
 (string? co)  	;;this is the only case where you will get a StanfordCoreNLP Object back 
  (new-coreNLP (doto (java.util.Properties.) 
                (.setProperty "annotators" (str ann-string ", " co))))
(instance? edu.stanford.nlp.pipeline.Annotator co) (Workflow. (vector (extract-annotators this true) co))   	 
:else (Workflow. (vector (extract-annotators this true) (reify edu.stanford.nlp.pipeline.Annotator 
  	                                                   (annotate [this annotation] 
  	                                                    (run co annotation))))))))
(getComponents [this]
 (extract-annotators this true)) )
(emit-IComponent-impls ;nice trick to avoid enumerating all the identical implementations
  POSTaggerAnnotator PTBTokenizerAnnotator WordsToSentencesAnnotator 
  CleanXmlAnnotator MorphaAnnotator NERCombinerAnnotator RegexNERAnnotator 
  TrueCaseAnnotator ParserAnnotator DeterministicCorefAnnotator) )
     
;(def props (new-properties "annotators" "tokenize" "ssplit" "pos" "lemma" "regexner" "parse" "dcoref")) 
;(def annotator (new-coreNLP props)) 
;------------------------------------------>GATE-EMBEDED<---------------------------------------------------
(definline gate-init [^String annie-dir]
`(do (Gate/init)  
   (.registerDirectories (Gate/getCreoleRegister) 
        (doto (java.io.File. ~annie-dir) (.toURL)))))

(defn extend-gate []
(extend-type gate.Executable
IComponent
(run 
([this] 
 (.execute this)
 (.getAnnotations ^gate.Document (.getDocument ^gate.creole.AbstractLanguageAnalyser this)))
(run [this _] 
 (run this)))
(link [this pos other] 
  (Workflow. (hotel_nlp.helper/link this pos other))) )
)
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
   
