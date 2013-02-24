(ns hotel_nlp.helper
   (:require 
        [clojure.data.zip      :as zf]
        [clojure.data.zip.xml  :as zxml]  
        [clojure.xml           :as xml]
        [clojure.zip           :as zip]
        [clojure.set           :as sets]
        [clojure.xml           :as xml]
        [clojure.string        :as stu]
        [clojure.java.io       :as io]
        [clojure.core.reducers :as r]
        [clojure.test :refer [with-test is run-tests]]
        [hotel_nlp.protocols       :as pro]
     )
   (:import [java.io File FileFilter StringReader]
            [java.util.regex Pattern PatternSyntaxException]
            [org.apache.pdfbox.pdmodel PDDocument] 
            [org.apache.pdfbox.util PDFTextStripper]
            [java.util.concurrent Executors ExecutorCompletionService] [org.xml.sax InputSource SAXParseException]
            [org.tartarus.snowball.ext EnglishStemmer DanishStemmer DutchStemmer FinnishStemmer FrenchStemmer German2Stemmer GermanStemmer
            HungarianStemmer ItalianStemmer KpStemmer LovinsStemmer NorwegianStemmer PorterStemmer PortugueseStemmer RomanianStemmer
            RussianStemmer SpanishStemmer SwedishStemmer TurkishStemmer])  
)

(definline testables 
"Returns all the vars from the given namespace with the :test key present in their meta-data." 
 [nspace]
 `(for [[_# v#] (ns-map ~nspace) :when (:test (meta v#))] v#)) 
 
 ;(run-tests)

(def requirements [:string :string-seq :2d-string-seq :string-array :span-array :stanford-annotation]) 
 
(def pretrained-models {:openNLP {}
                        :stanfordNLP {}
                        :gimli    {}}) 

(def openNLP-NER-tags {:opening "<START:" 
                       :closing " <END>"
                       :middle  "> "
                       :order [:entity :token]})
                       
                     
(def stanfordNLP-NER-tags {:opening "" 
                           :middle  "" 
                           :closing "\t"
                           :order [:token :entity]
                           })

(def plain-NER-tags {:opening "__" 
                     :middle  ": " 
                     :closing "__"
                     :order [:token :entity]
                     })
(with-test 
(defn insert-at 
"Insert item in coll at position pos. Specify the position starting from 1."
[coll pos item]
{:pre [(pos? pos) (<= pos (count coll))]}
(let [v (if (vector? coll) coll (vec coll))
       left-side  (subvec v 0 (dec pos))
       right-side (subvec v (dec pos) (count v))]
  (vec (concat (conj left-side item) right-side))))

(is (= [:a :b :ME :c :d :e] (insert-at [:a :b :c :d :e] 3 :ME)))
(is (= [:a :b :c :ME :d :e] (insert-at [:a :b :c :d :e] 4 :ME)))
(is (= [:ME :a :b :c :d :e] (insert-at [:a :b :c :d :e] 1 :ME)))
(is (= [:a :b :c :d :ME :e] (insert-at [:a :b :c :d :e] 5 :ME)))
(is (thrown? AssertionError (insert-at [:a :b :c :d :e] 0 :ME)))
(is (thrown? AssertionError (insert-at [:a :b :c :d :e] 6 :ME)))
(is (thrown? AssertionError (insert-at [:a :b :c :d :e] -1 :ME)))
) 
  
(with-test   
(defn remove-at
"Remove the item at position pos from coll. Specify the position starting from 1." 
[coll pos]
{:pre [(pos? pos) (<= pos (count coll))]}
(let [v (if (vector? coll) coll (vec coll))
       left-side  (subvec v 0 (dec pos))
       right-side (subvec v pos (count v))]
 (vec (concat left-side right-side))))
 
(is (= [:a :c :d :e] (remove-at [:a :b :c :d :e] 2))) 
(is (= [:a :b :c :d] (remove-at [:a :b :c :d :e] 5)))
(is (= [:b :c :d :e] (remove-at [:a :b :c :d :e] 1)))
(is (thrown? AssertionError (remove-at [:a :b :c :d :e] 0))) 
(is (thrown? AssertionError (remove-at [:a :b :c :d :e] 6)))
(is (thrown? AssertionError (remove-at [:a :b :c :d :e] -1)))   
) 
 
(defn read-resource 
"Given a path to some resource (resources/...) , read it in using (comp read-string slurp)." 
[path]
(io!
 (binding [*read-eval* false] 
 (-> path
    io/resource
    slurp
    read-string))))         
    
(defmacro definvokable ;credit to Meikel Brandmeyer (kotarak), might come in handy
  [type fields & deftype-tail]
  (let [f        (fields 0)
        args     (repeatedly 20 gensym)
        arity    (fn [n]
                   (let [args (take n args)]
                     `(invoke [this# ~@args] ((. this# ~f) ~@args))))
        vararg   `(invoke [this# ~@args more#]
                    (apply (. this# ~f) ~@args more#))
        apply-to `(applyTo [this# args#] (apply (. this# ~f) args#))]
    `(deftype ~type
       ~fields
       clojure.lang.IFn
       ~@(map arity (range (inc 20)))
       ~vararg
       ~apply-to
       ~@deftype-tail)))     
    
(defn deep-merge-with ;from contrib - don't know where it's gone!
"Like merge-with, but merges maps recursively, applying the given fn
only when there's a non-map at a particular level.
(deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
             {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
-> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
[f & maps]
(apply
  (fn m [& maps]
     (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
  maps))      


(defn porter-stemmer 
"Depending on lang, returns the appropriate porter-stemmer instance. Using Snowball underneath."
^org.tartarus.snowball.SnowballProgram
[^String lang]
(case lang
	"danish"    (DanishStemmer.)  "dutch"   (DutchStemmer.)   "english"   (EnglishStemmer.) "finnish"   (FinnishStemmer.) 
	"french"    (FrenchStemmer.)  "german2" (German2Stemmer.) "german"    (GermanStemmer.)  "hungarian" (HungarianStemmer.)
        "italian"   (ItalianStemmer.) "kp"      (KpStemmer.)      "lovins"    (LovinsStemmer.)  "norwegian" (NorwegianStemmer.) 
	"porter"    (PorterStemmer.)  "postugese" (PortugueseStemmer.) "romanian"  (RomanianStemmer.) "russian" (RussianStemmer.) 
	"spanish"   (SpanishStemmer.) "swedish"   (SwedishStemmer.)    "turskish"  (TurkishStemmer.)
 (throw 
  (IllegalArgumentException. "Language NOT supported...Aborting!")))) 

(defmacro instantiate 
"Returns an instance of the given class. Depending on the argument list will invoke the coresponding contructor." 
[cl-name & args]
 `(eval (list 'new ~cl-name ~@args)))
 
(with-test 
(defn un-capitalize ^String [^String s]
(if (every? #(Character/isUpperCase ^Character %) s) s 
  (let [Cfirst (subs s 0 1)
        Crest  (subs s 1) ]
  (str (.toLowerCase Cfirst) Crest)))) 
(is (= "jam" (un-capitalize "Jam")))
(is (= "PH"  (un-capitalize "PH")))
(is (= "mr." (un-capitalize "Mr."))) 
(is (= "pH-0.5" (un-capitalize "PH-0.5"))) 
)  

(with-test  
(definline ->lower-case
"Converts all characters of string s to lower-case."
 [^String s]
 `(.toLowerCase ~s))
 (is (= ["don't" "talk" "to" "strangers"] (mapv ->lower-case ["DON'T" "TALK" "TO" "STRANGERS"])))
 )

(with-test 
(definline ->upper-case
"Converts all characters of string s to upper-case."
 [^String s]
 `(.toUpperCase ~s))
 (is (= ["DON'T" "TALK" "TO" "STRANGERS"] (mapv ->upper-case ["don't" "talk" "to" "strangers"])))
 )

(def normaliser 
"A basic normaliser for a list of Strings. 
 Expects a filename (what you would pass to slurp) where in each line contains 1 string (e.g. dictionary or tokenized text). 
 It will trim, un-capitalize and replace dashes with underscores on every line."
(comp (fn [untrimmed] 
       (map #(un-capitalize (.replaceAll (.trim ^String %) "-" "_")) untrimmed)) 
      stu/split-lines 
      slurp))

(with-test      
(defn singleton-token? [s]
(< (count (stu/split s #"(\s|\-)")) 2)) 

(is (singleton-token? "clojure"))
(is (singleton-token? "brain"))
(is (singleton-token? "jim123"))
(is (= false (singleton-token? "clojure-rc")))
(is (= false (singleton-token? "baby milk")))
(is (= false (singleton-token? "ph-0.15")))
)

(with-test 
(defn dim-no 
"Returns the number of dimensions for coll which must be a java.util.Collection." 
[coll]
(assert (instance? java.util.Collection coll) "Can only accept java.util.Collection")
(loop [i 1
      [f s & more] coll]
  (if-not (instance? java.util.Collection f) i 
 (recur (inc i) f))))
 
(is (= 1 (dim-no [3 4 5]))) 
(is (= 2 (dim-no [[3 4] [5 6]])))
(is (= 3 (dim-no [[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]])))
(is (= 1 (dim-no (java.util.ArrayList. 10))))
(is (= 2 (dim-no (doto (java.util.ArrayList. 10) (.add (java.util.ArrayList. 5))))))
)
 

 
(definline two-d? [coll]
`(= (dim-no ~coll) 2))

(definline three-d? [coll]
`(= (dim-no ~coll) 3))

(definline one-d? [coll]
`(= (dim-no ~coll) 1))

(defn matrix ;;construct matrix (2d, 3d or 4d)
([n m]  (for [i (range n) 
              j (range m)] 
         [i j]))
([n m z] (for [i (range n) 
               j (range m)
               k (range z)] 
         [i j k]))
([n m z h] (for [i (range n) 
                 j (range m)
                 k (range z)
                 w (range h)] 
         [i j k w])))
         
(defn range-vmatrix [dim & dim-lengths]
{:pre [(not (nil? dim))]} ;;cannot accept nil dimensions
(let [dim-no   (count dim-lengths)
      symbols  (repeatedly dim-no gensym) 
      counts   (inc dim-no) ;;include dim in count
      curr-sym (or (first symbols) (gensym))] ;;account for dim
(if (> counts 1) ;;more dimensions?
 `(vec 
    (for ~(vector curr-sym `(range ~dim))
    ~(apply range-vmatrix (first dim-lengths) (next dim-lengths)))) ;;recurse for each element
 `(vec (for ~(vector curr-sym `(range ~dim))  ~curr-sym)))))  ;;revert to plain 'for'            
                 
         
(defn zip-xml-str "Parses an .xml file and returns a zipper." [s]
(try 
 (zip/xml-zip 
   (xml/parse (InputSource. (StringReader. s))))
 (catch SAXParseException spe (println "\n** Parsing error, line "  (.getLineNumber spe)
                                                          ", uri "  (.getSystemId spe)
                                            		  "\n"      (.getMessage spe)))
 (catch Throwable t (.printStackTrace t))))
   
   
(defn extract-xml-tag 
"Reads an xml file and extracts all the content of the single tag located at the end of nodes. This is handy when extracting dictionary entries."
[fname & nodes];extracts names from a zip structure
(let [zipper   (zip-xml-str  (slurp fname))]
 (apply zxml/xml-> zipper (conj (vec nodes) zxml/text)))) ;return entities 
                        
;(extract-xml-tag "DRUGBANK-OPENNLP.xml" :entry :token) 

(defn extract-xml-blocks 
"Reads an xml file that consists of 'blocks' and extracts the content of the tags located at the end of the path and in the order
specified in groups. This is handy when the same target tag can be reached via different paths and we want to include everything while maintaining
ordering."
[fname block & groups];extracts names from a zip structure
(let [zipper   (zip-xml-str (slurp fname))
      juxt-fns   (for [g groups] 
                   #(apply zxml/xml1-> % (conj (vec g) zxml/text)))
      juxts (apply juxt juxt-fns)]   
 (mapcat juxts (zxml/xml-> zipper block))))  
 
;(ut/extract-xml-blocks "invitro_test.xml" :article [:title :sentence] [:abstract :sentence] [:abstract :annotation :sentence])   

(defn pdf->txt [^String src & {:keys [s-page e-page dest]  
                               :or {s-page 1 dest (str (first (stu/split src #"\.")) ".txt")}}]
 {:pre [(< 0 s-page) (.endsWith src ".pdf")]} 
 (print "     \u001B[31mYOU ARE PERFORMING A POTENTIALLY ILLEGAL OPERATION...\n\t PROCEED AT YOUR OWN RISK!!!\u001B[m \n Proceed? (y/n):")
 (when  (-> *in*
              (java.util.Scanner.)
              .next
              (.charAt 0)
              (= \y))
 (with-open [pd (PDDocument/load (File. src))
             wr ^java.io.BufferedWriter (io/writer dest)]
  (let [page-no (.getNumberOfPages pd)
        stripper (doto (PDFTextStripper.)
                     (.setStartPage s-page)
                     (.setEndPage (or e-page page-no)))]                
    (println " #Total pages =" page-no "\n" 
             "#Selected pages =" (- (or e-page page-no) (dec s-page)))
    (.writeText stripper pd wr)
    (println "Raw content extracted and written to" dest "...")))))  
 
(defn join 
([string-seq] (join string-seq "\n"))
([string-seq ^String separator] (stu/join separator string-seq)))

(defn link [c1 pos c2]
(let [others (if (satisfies? pro/IWorkflow c2) (pro/getComponents c2) c2)]
 (case pos 
   :before  (if (map? others) (vector c1 others) (apply vector c1 others)) 
   :after   (if (map? others) (vector others c1) (apply vector others '(c1)))
 "Not a valid position.")))
   
(defn pool-map 
"A saner, more disciplined version of pmap. 
 Submits jobs eagerly but polls for results lazily. 
 Don't use if original ordering of 'coll' matters." 
([f coll threads]
 (let [exec (Executors/newFixedThreadPool threads)
       pool (ExecutorCompletionService. exec)
       futures (try (doall (for [x coll] (.submit pool #(f x))))
               (finally (.shutdown exec)))] 
 (for [_ futures]  
   (.. pool take get)) ))
([f coll] 
  (pool-map f coll (.. Runtime getRuntime availableProcessors))))   


(defn fold-into-vec [chunk coll]
"Provided a reducer, concatenate into a vector.
 Same as (into [] coll), but parallel."
  (r/fold chunk (r/monoid into vector) conj coll))

(defn rmap
"A fork-join based mapping function that pours the results in a vector." 
[f coll fj-chunk-size]
(fold-into-vec fj-chunk-size (r/map f coll))) 

(defn mapr
"A pretty basic map-reduce style mapping function. Will partition the data according to p-size and assign a thread to each partition."  
([f coll p-size]
 (apply concat ;;concat the inner vectors that represent the partitions
   (pmap (fn [p] (reduce #(conj % (f %2)) [] p))
     (partition-all p-size coll))))
([f coll] (mapr f coll 4)))                                                                    

(defn create-folder! 
"Creates a folder at the specified path." 
[^String path]
(.mkdir (File. path))) 

(defn unite
"Takes a 2d seq and produces a united set of all entries (no duplicates)."
 [ds]
 (if (< 1 (count ds))
   (let [set-views (map set ds)]
     (apply sets/union set-views)) (first ds)))
     
(defn file-filter "Returns a FileFilter object that scans according the specified filter." 
[^String filter]
(reify FileFilter
 (accept [this f]
  (boolean 
  (re-find (re-pattern (str ".*" filter)) (.getName ^File f))))))
  
(defn url? [x]
(if (instance? java.net.URL x) true false))   
  
(defn file->data
"Read the file f back on memory safely (as much as possible). 
 Contents of f should be a clojure data-structure. Not allowed to execute arbitrary code (per #=)." 
[f]
(io!
 (binding [*read-eval* false]
 (read-string (slurp f))))) 
 
(defn space-out 
"Given some text, find all tags (according to the tagging-scheme) that are not surrounded by spaces and put spaces around them." 
 ^String [^String text t-scheme]
(when-not (or (stu/blank? (:opening t-scheme)) (stu/blank? (:closing t-scheme)))
(-> (re-matcher (re-pattern (str "(?<! )" (:opening t-scheme)) )                          ;"(?<! )<STA")  
(-> (re-matcher (re-pattern (str (apply str (next (:closing t-scheme))) "(?! )")) text)   ;"END>(?! )"
   (.replaceAll "$0 ")))
(.replaceAll " $0")))) 

(definline string-array? [a]
`(if (instance? (Class/forName "[Ljava.lang.String;") ~a) true false))

       
  
     
