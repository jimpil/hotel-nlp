(ns hotel_nlp.tools.PAnnotator.core 
  (:require [clojure.core.reducers :as r] 
            [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [split-lines split blank?]]
            [hotel_nlp.helper :as ut]
            [hotel_nlp.concretions.regexes :as reg]
  )      
  (:import [java.io File FileFilter]
           [java.util.regex Pattern PatternSyntaxException])
  (:gen-class :name Annotator
              :main true
              :methods [^:static [process [java.util.Map] void]
                        ^:static [process [String String java.util.List] String]]) 
)  



     
(def ^:dynamic *cpu-no* (.. Runtime getRuntime availableProcessors))
(def fj-chunk-size (atom 5))
(def global-dic    (atom nil))
(def sentence-segmentation-regex reg/sentence-segmentation-regex)
(def token-regex                 reg/token-regex)                        
(def openNLP-NER-tags ut/openNLP-NER-tags)
(def stanfordNLP-NER-tags ut/stanfordNLP-NER-tags) 
(def plain-NER-tags ut/plain-NER-tags) 

;;PERFORM TOKENIZATION ON YOUR SHELL                    
;for TOKEN in `cat some-file.txt`; do echo $TOKEN; done
;for TOKEN in `cat some-file.txt.txt`; do echo -e $TOKEN '\tO' >> some-file.txt.tok; done
                                              
(def custom-NER-tags (atom {})) 
                                                                                                                
(defn- s-split 
"Segments the given string into distinct sentences delimited by a newline." 
[^String s]
 (ut/join 
  (split s sentence-segmentation-regex))) 

(defn split-sentences [filename]
(s-split  (slurp filename)))

(defn simple-tokenize 
"An extremely simple tokenizer that splits on any non-alphanumeric character.
 Inline punctuation is mostly preserved.  Returns a lazy-seq of whole-word tokens. 
 Optionally, you can apply stemming to all the returned tokens. Strings know how to stem themselves. Simply use the 'getRoot' fn.
 usage: (simple-tokenize \"the fox jumped over the lazy dog\" :stemmer getRoot)   OR 
        (simple-tokenize \"Wir besuchen meine Tante in Berlin.\" :stemmer #(getRoot % \"german\")) 
 in case you want to specify a language other than English."
[sentence & {:keys [rgx stemmer] 
             :or {rgx token-regex}}]
(let [tokens (re-seq rgx sentence)]
  (if stemmer (stemmer tokens) tokens)))
                                                                    
                           
(defmulti format-tag  keyword)
(defmethod format-tag :plain-NER [_]
 #(str (:opening plain-NER-tags) % 
       (:middle  plain-NER-tags) %2 
       (:closing plain-NER-tags)))
(defmethod format-tag :openNLP-NER [_]
 #(str (:opening openNLP-NER-tags) % 
       (:middle openNLP-NER-tags)  %2 
       (:closing openNLP-NER-tags)))                    
(defmethod format-tag :stanfordNLP-NER [_]
 #(str %2 \tab %)) 
(defmethod format-tag :custom-NER [o]
(if (= (first (:order @custom-NER-tags)) :token)
 #(str (:opening @custom-NER-tags) %2 
       (:middle  @custom-NER-tags) %  
       (:closing @custom-NER-tags))
 #(str (:opening @custom-NER-tags) % 
       (:middle  @custom-NER-tags) %2 
       (:closing @custom-NER-tags))))                                                                                

  
(defn- mapping-fn
"Returns a fn that will take care mapping with this stratefy. 
 Supported options include :serial, :lazy, :lazy-parallel, :pool-parallel, :map-reduce, :fork-join & fork-joinHP(high-performance)." 
[strategy]
(case strategy
  :serial mapv
  :lazy   map
  :lazy-parallel pmap
  :pool-parallel ut/pool-map
  :map-reduce    ut/mapr
  :fork-join     ut/rmap
  :fork-joinHP   ut/rhmap
 (throw (IllegalArgumentException. "Mapping strategy not recognised!"))))
  
  
(defn- file-write-mode 
"Returns a fn that will take care writing files in this mode. 
 Supported options include :merge-all, :per-file & :on-screen." 
[mode target-f]
(case mode
  :merge-all   (fn [target content]
                 (do (spit target content :append true) 
                     (spit target "\n\n" :append true)))
  :per-file   (fn [target content] 
                (let [fname (str target-f "/"  target  ".pann")]
                   (spit fname content)))
  :on-screen   (fn [_ content] (print content "\n\n")) )) 
           
      
(defn- sort-input [data-s ffilter]
(let [data-seq (-> data-s slurp read-string)
      fitem (first data-seq)]
(if (sequential? fitem) data-s ;;the usual happens
  (let [^File directory (io/file fitem) ;;the whole directory
        f-seq (if-let [ff ffilter] 
                (.listFiles directory (ut/file-filter ff)) 
                (.listFiles directory))
        f-seq-names (for [f f-seq :when #(.isFile ^File f)] (.getPath ^File f))]
 (reset! global-dic (ut/unite (mapv ut/normaliser (next data-seq))))     
 (mapv #(vec (concat (list %) (next data-seq))) f-seq-names)))) ) ;;build the new 2d vector        
      

(defn annotate 
 "Overloaded function. 
  First one takes a filename f, an entity-type (e.g. \"person\") and some dictionaries 
  and annotates the text in f with openNLP compatible sgml tags for that entity-type using the provided dictionaries.
  Second one takes a map with 3 keys. The :files+dics key should point to a (2d) seq that contains other seqs 
  where the 1st item is the file we want to annotate and the rest are the available dictionaries. The :target key
  specifies the file to write the resulting annotated text and :entity-type describes the entities (nil falls-back 
  to \"default\").  Unless you have a single file to process, you should start with the 2nd overload as in the example below.
  The output file will contain new-line delimiters  per processed document.  
  e.g. 
    (annotate {:entity-type  \"drug\" :target \"test.txt\"  ;;resulting sgml: <START:drug> ... <END>
               :file+dics [[\"train/invitro_train-raw.txt\" \"train/invitro-train-names-distinct.txt\"] 
                           [\"train/invivo_train-raw.txt\"  \"train/invivo-train-names-distinct.txt\"]]})"
(^String [^String f ^String entity-type dics lib segment]
  (let [dic (if-let [gb @global-dic] gb
            (ut/unite (mapv ut/normaliser dics)))
        file-name (.getName (File. f))   
        format-fn  (format-tag lib)
        contents (slurp f)]      
       (println (str "Processing document: " f)) 
           (loop [text (cond 
                         (satisfies? hotel_nlp.protocols/IComponent segment) (hotel_nlp.protocols/run segment contents)
                         (satisfies? hotel_nlp.protocols/IWorkflow segment) (hotel_nlp.protocols/deploy segment contents)
                         (fn? segment) (segment contents)
                         (nil? segment) contents
                         :else (throw (IllegalArgumentException. "Can only use functions or components for sentence splitting!")))       
                  names dic]
  	     (if-let [name1 (first names)] 
           (recur 
            (try 
             (.replaceAll 
        	(re-matcher 
                (re-pattern (str "(?i)\\b+" (Pattern/quote name1) "+\\b")) text)  
               (format-fn entity-type name1)) 
             (catch PatternSyntaxException _ 
             (do (println (str "--- COULD NOT BE PROCESSED! --->" name1)) text)))
             (next names)) [file-name text]))  )) 
([{:keys [files+dics entity-type target target-folder consumer-lib strategy write-mode file-filter segmenter]
   :or {entity-type "DRUG" 
        target "target-file.txt"
        target-folder "ANNOTATION_TARGET"
        consumer-lib "openNLP-NER"
        strategy   "lazy-parallel"
        write-mode "merge-all"
        file-filter "txt"
        segmenter s-split}}]
 (let [f+ds (sort-input files+dics file-filter)
       annotations ((mapping-fn (keyword strategy)) ;;will return a mapping function
                       #(let [[f a] (annotate (first %) entity-type  (next %) (keyword consumer-lib) segmenter)] 
                            (vector f (ut/space-out a (var-get (ns-resolve 'hotel_nlp.helper (symbol (str consumer-lib "-tags")))))))
                               (cond (or (string? f+ds) 
                                         (ut/url? f+ds)) (ut/file->data f+ds) 
                                     (vector? f+ds)  f+ds
                                   :else (throw (IllegalArgumentException. "Weird data-format encountered! Cannot proceed..."))) )
       wfn  (do (when (= write-mode "per-file") (ut/create-folder! target-folder)) 
              (file-write-mode (keyword write-mode) target-folder)) ;will return a writing function
       wmd  (if (= write-mode "merge-all") (constantly target) identity)]      
  (doseq [[f a] annotations] 
    (wfn (wmd f) a)))) )
    
#_(cond-> f+ds 
  (or (string? f+ds) 
      (ut/url? f+ds)) ut/file->data
  (vector? f+ds)      identity  )     
   
      
(def -process annotate)

(comment
(def HELP_MESSAGE 
"\nINSTRUCTIONS: java -cp PAnnotator-uberjar.jar Annotator  -d <DATA-FILE>* 
                                                         -t <TARGET-FILE>** 
                                                         -e <ENTITY-TYPE>*** 
                                                         -par serial OR lazy OR lazy-parallel OR pool-parallel OR fork-join****
                                                         -wm  merge-all OR per-file OR on-screen*****
                                                         -fl  \"openNLP-NER\" OR \"stanfordNLP-NER\" OR \"plain-NER\"
                                                         -ct  \"{:opening \"_\" :closing \"_\" :middle \":\"}\"
                                                         -chu an integer value typically from 2 to 6 \n
*must be a file with a 2D clojure seqable of the form: [[input1 dic1 dic2 dic3 dic4] 
                                                        [input2 dic5 dic6 dic7] 
                                                        [input3 dic8 dic9]]
*defaults to 'data-file.txt' 
**defaults to 'target-file.txt'
***optional argument - defaults to \"default\"
****optional argument - defaults to 'lazy-parallel'
*****optional argument - defaults to 'merge-all'")

(defn -main [& args]
  ;(pos-tag "target-file.txt" true)   
  (let [[opts argus banner] 
        (cli args
      ["-h" "--help" "Show help/instructions." :flag true :default false]
      ["-d" "--data" "REQUIRED: The data-file (e.g. \"data.txt\")" :default "data-file.txt"]
      ["-e" "--entity-type" "REQUIRED: The type of entity in question (e.g. \"river\")" :default "default"]
      ["-t" "--target" "REQUIRED: The target-file (e.g. \"target.txt\")" :default "target-file.txt"]
      ["-tfo" "--target-folder" "Specify a target folder. Only useful if write-mode is set 'to per-file' (defaults to ANNOTATED/)." :default "ANNOTATED"] 
      ["-ct" "--custom-tags" "Specify your own tags. (e.g \"{:opening \"_\" :closing \"_\" :middle \":\" :order [:token :entity]}\")"]
      ["-par" "--parallelism" "Set a parallelisation strategy (other options are: serial, lazy, pool-parallel, map-reduce & fork-join)." :default "lazy-parallel"]
      ["-chu" "--chunking" "Specify the number where recursive tree splitting should stop in a fork-join task (defaults to 4)." :default 4]
      ["-wm" "--write-mode" "Set file-write-mode (other options are: per-file & on-screen)." :default "merge-all"]
      ["-ff" "--file-filter" "Specify a file-filter when processing an entire folder (there is NO default - all files will be processed)."]
      ["-fl" "--for-lib" "Apply a predefined tagging format (other options are: stanfordNLP-NER, plain-NER & custom-NER)." :default "openNLP-NER"]
      ["-tok" "--tokens" "Extracts (and optionally stems) tokens from the supplied string or file. Activate stemming with the -ste flag."]
      ["-ste" "--stemming" "Activates porter-stemming. Only useful when paired with the -tok switch which returns the tokens." :flag true :default false]
      ["-lang" "--language" "Set the stemming language. Only useful when paired with the -tok and -ste switches." :default "english"]
      ["-allign" "--allignment" "Perform local allignement (Smith-Waterman) between 2 sequences."]
      ["-dist" "--edit-distance" "Calculate the edit-distance (Levenshtein) between 2 words."]
      ["-rpdf"  "--ripdf" "Extract the contents from a pdf file and write them to a plain txt file."]
      ["-dname" "--drugname" "Generate a finite list (you provide how many) of randomly assembled name(s) that look like drugs (orthographically)."]
      )]
    (when (:help opts)
      (println HELP_MESSAGE "\n\n" banner)
      (System/exit 0))
    (when-let [source (:ripdf opts)]
      (pdf->txt source)
      (System/exit 0))
    (when-let [how-many  (:drugname opts)] 
      (let [n (Integer/parseInt how-many)]
        (println (apply randrugs n (drop 2 args)))
        (System/exit 0)))  
    (when (:allignment opts)
      (if (> 3 (count args))
       (do (println "Less than 2 arguments detected! Please provide 2 sequences to allign...")
        (System/exit 0))
   (do (println (allignSW (second args) (nth args 2))) (System/exit 0))))
   (when (:edit-distance opts)
      (if (> 3 (count args))
       (do (println "Less than 2 arguments detected! Please provide 2 words to compare...")
        (System/exit 0))
   (do (println "\nLevenshtein distance =" (getDistance (second args) (nth args 2))) (System/exit 0))))  
    (when-let [ss (:tokens opts)]  
      (if (:stemming opts) 
      (pprint (simple-tokenize (if (string? ss) ss (slurp ss)) :stemmer #(getRoot % (:language opts)))) 
      (pprint (simple-tokenize (if (string? ss) ss (slurp ss))))) 
      (System/exit 0)) 
    (when-let [cs (:chunking opts)] 
      (reset! fj-chunk-size (Integer/valueOf ^Integer cs)))    
  (do (println "\n\t\t====> \u001B[35mPAnnotator\u001B[m \u001B[32mv0.3.4\u001B[m <====\t\t\n\t\t-----------------------------\n\n")
      (println "\tRun settings:" "\n\n--Entity-Type:" (:entity-type opts)  
                                    "\n--Target-File:" (:target opts)
                                    "\n--Target-Folder:" (:target-folder opts)
                                    "\n--Data-File:" (:data opts)  "\n--Mapping strategy:" (:parallelism opts) 
                                    "\n--Fork-join tree leaf-size:" @fj-chunk-size "(potentially irrelevant for this run)" 
                                    "\n--Consumer library:" (:for-lib opts) "(potentially irrelevant for this run)"
                                    "\n--Write-Mode:" (:write-mode opts) "\n--Custom-tags:" (:custom-tags opts) "\n\n")
      (-process {:entity-type  (:entity-type opts)  
                 :target       (:target opts)
                 :target-folder (:target-folder opts)
                 :files+dics    (:data opts)
                 :file-filter  (:file-filter opts)
                 :strategy     (:parallelism opts)
                 :write-mode   (:write-mode opts)
                 :consumer-lib (if-let [cu (:custom-tags opts)]  
                                 (do (swap! custom-NER-tags merge (read-string cu)) 
                                     "custom-NER")  
                                 (:for-lib opts))})
    (case (:write-mode opts)
    "merge-all"              
    (println "--------------------------------------------------------\n"
             "SUCCESS! Look for a file called" (str "'" (:target opts) "'. \n"))
    "per-file" 
    (println "--------------------------------------------------------\n"
             "SUCCESS! Look for the file(s) under folder" (str (or (:target-folder opts) "ANNOTATED") "/ at the root of your classpath. \n")) 
     "on-screen" (println "-----------------------------------------------------\n\n => THAT WAS IT...!\n") 
     (println "write-mode can be either 'merge-all' or 'per-file' or 'on-screen'..."))              
    (shutdown-agents) 
    (System/exit 0)) ))


;;example    
(-process {
        :entity-type "DRUG"
        :files+dics (io/resource "data-folder.txt")
        :target "target-file.txt"
        :segmenter hotel_nlp.concretions.artefacts/pannotator-pluggable-sent-splitter
        ;:target-folder "ANNOTATIONS"
        :consumer-lib "openNLP-NER"
        :strategy   "pool-parallel"
        :write-mode "merge-all"
        :file-filter "txt"})    
    
)
    
