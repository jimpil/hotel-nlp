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
        [clojure.edn           :as edn]
        [clojure.core.reducers :as r]
        [clojure.test :refer [with-test is run-tests]]
        [hotel_nlp.protocols   :refer :all]
        ;[plumbing.fnk.impl :refer [fnk-form]]
        [plumbing.core :refer [fnk]]
        [plumbing.graph :refer [eager-compile]]
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
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(defonce cpu-no (.. Runtime getRuntime availableProcessors))
(defonce brown-pos-tagset
  {:simplified #{"ADJ" "ADV" "CNJ" "EX" "DET" "FW" "MOD" "N" "NP" "NUM" "PRO" "P" "TO" "UH" "V" "VD" "VG" "VN" "WH"} ;;19 tags
   :original   #{"." "(" ")" "--" "*" "," ":" "ABL" "ABN" "ABX" "AP" "AT" "BE" "BED" "BEDZ" "BEG" "BEN" "BER" "BEM" "BEZ" "CC" "CD" "CS" "DO" "DOD" "DOZ" "DT" "DTI" "DTS" "DTX" 
                 "EX" "FW" "HV" "HVD" "HVG" "HVN" "IN" "JJ" "JJR" "JJS" "JJT" "MD" "NC" "NN" "NN$" "NNS" "NNS$" "NP" "NP$" "NPS" "NPS$" "NR" "OD" "PN" "PN$" "PP$" "PP$$" "PPL" "-NC" 
                 "PPLS" "PPO" "PPS" "PPSS" "PRP" "PRP$" "QL" "QLP" "RB" "RBR" "RBT" "RN" "RP" "TO" "UH" "VB" "VBD" "VBG" "VBN" "VBP" "VBZ" "WDT" "WP$" "WPO" "WPS" "WQL" "WRB" "-TL" "-HL"} ;;88 tags
   :full       #{"``" "VBN-NC" "IN-TL" "FW-NR" "CC" "PPSS" "NNS$-HL" "PPSS+MD-NC" "''" "BEM*" "HVN" "JJ" "DO+PPSS" "NPS-HL" "--" "NP-NC" "NN" "NP+MD" "PP$$" "NR$" "RBR-NC" "WDT+BER+PP" 
                 "FW-TO+VB" "PPSS+BER-TL" "CD" "BED" "RB-HL" "JJ$-TL" "NN$-TL" "BED-NC" "CD-NC" "PP$-TL" "NN+HVZ" "NR-TL" "DT-HL" "CS-HL" "FW-IN" "BE-TL" "RBR" "NN-NC" "FW-AT-HL" "PPO-HL" 
                 "NN+MD" "JJ-NC" "MD+HV" "NP" "PPSS-NC" "CC-NC" "DOZ-HL" "NP$" "PPSS+MD" "BER-HL" "VBN" "AP-HL" "BE" "PPS+BEZ-NC" "VBN-TL" "NP$-TL" "VB+JJ-NC" "DTS" "NP+HVZ" "HV-HL" "NP-TL"
                 ".-HL" "VBZ-HL" "CD$" "FW-IN-TL" "RBT" "PN+MD" "VB+IN" "BEG" "NN+HVZ-TL" "PPSS+BER" "FW-IN+AT-T" "VBN+TO" "NR" "PP$" "NN$" "ABN-HL" "CD-TL" "RP-NC" "PN-NC" "PPL-HL" "NR$-TL"
                 "WPS+HVD" "IN" "NN-TL" "PPSS+BER-N" "JJ-TL" ",-HL" "PPSS-TL" "CC-TL" "FW-NR-TL" "VB-HL" "DT+BEZ-NC" "WPS+BEZ" "FW-VBN" "FW-NN-NC" "FW-JJ-NC" "DO-HL" "PN$" "RB+BEZ-NC" 
                 "FW-VBG-TL" "'" "RP-TL" "FW-AT+NP-TL" "PN-TL" "WRB" "*-HL" "VBG-NC" "TO-NC" "FW-NP-TL" "QL-NC" "FW-DTS" "WDT" "BER*-NC" "PPLS" "WPS+MD" "(" "WDT+HVZ" "FW-VB" ")-HL" "DTX" 
                 "FW-PP$" "FW-CD-TL" "FW-NN$" "WPS-NC" "FW-IN+AT-TL" "PN+HVD" ")" "WRB+IN" "(-HL" "IN+PPO" "JJR+CS" "FW-VBD-TL" "FW-NN-TL" "FW-JJ-TL" "DOD-NC" "FW-CC-TL" "BED*" "FW-AT+NN-TL" 
                 "BEDZ-NC" "*" "VBD-NC" "BEM" "DOZ*-TL" "WDT-HL" "VBG-TL" "TO-TL" "QL-TL" "FW-HV" "PN+BEZ" "WRB-HL" "FW-IN+NN" "WDT+BER" "FW-WDT" "WP$" "ABL" "WPS-TL" "DO" "BEN" ":-TL" 
                 "PPSS+HVD" "MD-NC" "RB+CS" "WRB+BER" "WRB+DOZ" "BEZ-NC" "VBG+TO" "IN+IN" "FW-NNS-NC" "," "FW-RB" "CC-TL-HL" "PPSS+BEZ" "HVZ" "JJ-TL-HL" "WPO-NC" "NN-TL-HL" "NN+BEZ" "PPL" 
                 "VB" "PPSS+HV-TL" "IN-HL" "FW-CS" "DTI-TL" "FW-DT" "CD-TL-HL" "ABN" "OD-NC" "NN$-HL" "VBD-TL" "PP$-HL" "NR-HL" "." "FW-NNS-TL" "NP-TL-HL" "HV" "DTS-HL" "NP+BEZ" "FW-*" 
                 "VBN-TL-HL" "MD-TL" "BE-HL" "JJT-NC" "VBZ" "BEZ-TL" "NP$-HL" "AP" "BER" "NP-HL" "DTS+BEZ" "EX-NC" "FW-UH" "DOZ" "EX+MD" "WDT+BEZ-NC" "AT-NC" "PPO" "FW-AT" "MD+TO" "WPO-TL"
                 "NNS-NC" "NRS" "UH-NC" "PPS-NC" "VBN-HL" "NNS+MD" "PPS+MD" "WRB+DO" "CS" "DT" "NN+IN" "OD-TL" "NNS-TL-NC" "CD-HL" "RB" "JJR-NC" "VB+PPO" "NR-TL-HL" "FW-NPS-TL" "FW-PPL" 
                 "NN-HL" "---HL" "JJ-HL" "FW-OD-NC" "NNS$-NC" "PPSS-HL" "CC-HL" "IN-TL-HL" "DOD*" "NIL" "NPS-NC" "NPS" "BEDZ*" "JJT-TL" "NNS$" "PPS+BEZ-HL" "PPS-TL" "DT-NC" "CS-NC" "FW-VBZ" 
                 "DT+MD" "WRB+BEZ-TL" "DO*-HL" "WQL" "WDT+BEZ-TL" "AT-TL" "RB-NC" "JJR" "NPS$-TL" "JJS-TL" "VB+AT" "RP+IN" "NNS-TL" "UH-TL" "AP-NC" "AT" "WDT+BEZ" "EX" "WQL-TL" "MD*" "FW-BER" 
                 "PPO-NC" "WRB+BEZ" "BEZ*" "PPS" "UH" "NNS" "FW-PPL+VBZ" "RP-HL" "JJR-TL" "JJS" "FW-UH-NC" "PN-HL" "NPS$" "FW-PPO" "BER-NC" "EX+HVZ" "VBZ-NC" "FW-OD-TL" "DT$" "HVD*" "NNS$-TL"
                 "PPS+HVZ" "NP+BEZ-NC" "NPS-TL" "HV-NC" "JJT" "DOD*-TL" ".-NC" "MD+PPSS" "RB$" "WRB+DOD*" "DT-TL" "CS-TL" "WDT+DOD" "QLP" "RB+BEZ-HL" "OD" "AP$" "WRB+DOD" "ABN-NC" "RB-TL" 
                 "FW-NPS" "FW-JJR" "VB-NC" "BER-TL" "WPO" "PPL-NC" "AP-TL" "VBG-HL" "HVD" "TO-HL" "NRS-TL" "HVZ-NC" "QL-HL" "FW-AT-TL" "PPO-TL" ",-NC" "DOZ-TL" "FW-UH-TL" "WDT+DO+PPS" ".-TL" 
                 "ABX" "FW-QL" "BEZ" "VB+VB-NC" "VBZ-TL" "JJ+JJ-NC" "NN+NN-NC" "MD" "DO-NC" "FW-*-TL" "WPS-HL" ":-HL" "FW-PPS" "HV-TL" "PPSS+VB" "AP+AP-NC" "FW-NNS" "HV+TO" "FW-JJT" "DTI-HL" 
                 "FW-DT+BEZ" "BEM-NC" "*-NC" "HVG-HL" "VBD" "BEDZ-HL" "ABN-TL" "BEDZ" "FW-NN-TL-NC" "HVG" "DOD" "VB-TL" "PPL-TL" "NN+BEZ-TL" "DTI" "PPSS+HV" "RBR+CS" "HVZ-TL" "FW-RB-TL" 
                 "VBD-HL" ",-TL" "FW-IN+NP-TL" ":" "FW-VB-NC" "FW-PN" "WPS" "NN+HVD-TL" "MD-HL" "VB+TO" "FW-WPO" "BEZ-HL" "FW-PP$-NC" "DO-TL" "BEN-TL" "WRB-NC" "DOZ*" "WRB+MD" "WPS+HVZ" 
                 "HVD-HL" "BER*" "FW-BEZ" "WDT-NC" "QL" "TO" "*-TL" "VBG" "FW-IN+NN-TL" "FW-NN" "FW-VBD" "WPS+BEZ-NC" "HV*" "DT+BEZ" "RN" "RB+BEZ" "FW-CC" "FW-JJ" "TO+VB" "FW-PPSS" "OD-HL" 
                 "FW-PPSS+HV" "FW-VB-TL" "VB+RP" "FW-PPO+IN" "JJT-HL" "JJ-TL-NC" "FW-IN+AT" "FW-CD" "NN-TL-NC" "FW-NN$-TL" "FW-PP$-TL" "IN-NC" "NNS$-TL-HL" "EX+HVD" "FW-NP" "EX-HL" "HVZ*" 
                 "WDT+BEZ-HL" "PPSS+BEZ*" "AT-HL" "WRB-TL" "NPS$-HL" "FW-WPS" "JJS-HL" "PN" "PP$-NC" "PPS+HVD" "RP" "NR-NC" "NNS-HL" "UH-HL" "PPS-HL" "NR+MD" "PPSS+BER-NC" "MD*-HL" "PN+HVZ" 
                 "FW-VBG" "NNS-TL-HL" "PPSS+BEM" "DO*" "WPS+BEZ-TL" "PPS+BEZ" "JJR-HL" "FW-RB+CC" "NP+HVZ-NC" "AT-TL-HL" "FW-BE" "VBN-TL-NC" "EX+BEZ"} ;;472 tags
   :mapping  {} }) 

(declare addC removeC replaceC insert-at remove-at link* linkage)

(defrecord Workflow [components] ;;a seq of components
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
IComponent   ;; the workflow can itself be a component
 (link [this pos other]
   (linkage this pos other))
 ;(run [this] (deploy this))
 (run [this text] (deploy this text)) ;;(reduce #(run  %2 %) text components))
 ;(run [this text & more] (deploy this text (first more)))
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (deploy this arg))
  (applyTo [this args]
    (apply deploy this args))
;Object
  ;(toString [this]
    ;(str "#hotel_nlp.helper.Workflow" {:components (:components this)}))
    )
    
(defmacro component->fnk "Converts a component into a fnk."
 [c & arg-syms]
  `(fnk  [~@arg-syms] (run ~c ~@arg-syms)))       
    
(defrecord GraphWorkflow [components] ;;a map of components -> fnks (see prismatic.plumbing)
 IWorkflow
 (getComponents [this] (keys (dissoc components :input))) ;;just return all keys except :input 
 (appendComponent [this [c fnk :as entry]]  (assoc components entry))
 (addComponent [this pos c]  (throw (IllegalStateException. "GWorkflow does not allow adding a component at a specified position because it is not a serial workflow.")))
 (removeComponent [this c] (dissoc components c))
 (replaceComponent [this pos replacement] 
    (throw (IllegalStateException. "GWorkflow does not allow replacing a component at a specified position because it is not a serial workflow. 
                                    Consider using appendComponent with a [:key-to-replace new-component]")))
 (deploy [_ text intermediates?]
   (let [G (eager-compile components)]
     (if intermediates?  
             (G {:s text})
      (first (G {:s text})))))
 (deploy [this text] 
  (deploy this text false))
IComponent   ;; the workflow can itself be a component
 (link [this pos other]
   (throw (IllegalStateException. "GWorkflow does not support linking. ")))
 (run [this text] (deploy this text)) 
clojure.lang.IFn  ;;can act as an fn
  (invoke [this arg]
    (deploy this arg))
  (applyTo [this args]
    (apply deploy this args))
;Object
  ;(toString [this]
    ;(str "#hotel_nlp.helper.Workflow" {:components (:components this)}))
    )    

#_(defmethod print-method Workflow [wf ^java.io.Writer w]
  (.write w "#hotel_nlp.helper.Workflow")
  (print-method {:components (:components wf)} w) )

#_(defmethod print-dup Workflow [r, ^java.io.Writer w]
  (.write w "hotel_nlp.helper.Workflow<")
  (print (:components r))
  ;(.write w ",")
  ;(print (.snd r))
  (.write w ">"))

(defn- addC [cs pos c]
  (Workflow. (insert-at cs pos c)))

(defn- removeC [cs pos]
  (Workflow. (remove-at cs pos)))

(defn- replaceC [cs pos c]
{:pre [(pos? pos) (<= pos (count cs))]}
  (Workflow. (assoc (vec cs) (dec pos) c)))


(definline testables
"Returns all the vars from the given namespace with the :test key present in their meta-data."
 [nspace]
 `(for [[_# v#] (ns-map ~nspace) :when (-> v# meta :test)] v#))

(definline in?
  "Returns the value of x if coll contains it, otherwise nil."
  [coll x]
 `(some #{~x} ~coll))

(defn linkage [obj pos other]
 (Workflow. (link* ~obj ~pos ~other)))

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

(defn slurp-resource
"Given a path to some resource (resources/...) , read it in using slurp."
[path]
(io!
 (binding [*read-eval* false]
 (-> path
    io/resource
    slurp))))

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
"Depending on lang, returns the appropriate porter-stemmer instance. Using Snowball underneath.
 Supported languages include the following:
 [danish, french, italian, spanish dutch, german, english, romanian, turkish, finnish, hungarian, russian, swedish, norwegian, portugese]
 If the language you specified does not match anything, an instance of the english stemmer will be returned."
^org.tartarus.snowball.SnowballProgram
[^String lang]
(case lang
   "danish"    (DanishStemmer.)  "dutch"   (DutchStemmer.)   "english"   (EnglishStemmer.) "finnish"   (FinnishStemmer.)
   "french"    (FrenchStemmer.)  "german2" (German2Stemmer.) "german"    (GermanStemmer.)  "hungarian" (HungarianStemmer.)
   "italian"   (ItalianStemmer.) "kp"      (KpStemmer.)      "lovins"    (LovinsStemmer.)  "norwegian" (NorwegianStemmer.)
   "portugese" (PortugueseStemmer.) "romanian"  (RomanianStemmer.) "russian" (RussianStemmer.) "spanish" (SpanishStemmer.)
   "swedish"   (SwedishStemmer.)    "turskish"  (TurkishStemmer.)
 (EnglishStemmer.)) )


(defn porter-stem "A function that stems words using Porter's algorithm."
(^String [^String s lang]
 (let [^org.tartarus.snowball.SnowballProgram stemmer
            (cond-> lang
                 (string? lang) (porter-stemmer))] ;if a stemmer object was passed in, use it!
   (.getCurrent
      (doto stemmer
        (.setCurrent s)
        .stem))))
([ss] ;;a collection?
  (mapv #(porter-stem % (porter-stemmer "english")) ss)) )

(defmacro instantiate
"Returns an instance of the given class. Depending on the argument list will invoke the coresponding contructor."
[cl-name & args]
 `(eval (list 'new ~cl-name ~@args)))

(defn new-instance "Create a new instance of the specified class using reflection."
 [^Class c & args]
  (if (empty? args) (.newInstance c)
    (.newInstance
      (.getConstructor c (into-array (map class args)))
      (to-array args))))

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
"Returns the number of dimensions for coll which must be a java.util.Collection or an array.."
[coll]
(assert (instance? java.util.Collection (seq coll)) "Can only accept seqables")
(loop [i 1
      [f s & more] (try (seq coll) (catch Exception ex coll))]
  (if-not (instance? java.util.Collection (try (if (string? f) f (seq f))
                                          (catch Exception ex f))) i
 (recur (inc i) f))))

(is (= 1 (dim-no [3 4 5])))
(is (= 2 (dim-no [[3 4] [5 6]])))
(is (= 3 (dim-no [[[1 2 3] [4 5 6]] [[7 8 9] [10 11 12]]])))
(is (= 1 (dim-no (doto (java.util.ArrayList.) (.add 10) (.add 30)))))
(is (= 2 (dim-no (doto (java.util.ArrayList. 10) (.add (doto (java.util.ArrayList.) (.add 0) ))))))
(is (= 1 (dim-no ["jim" "george"])))
(is (= 2 (dim-no [["jim" "george"]])))
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

(defn str-builder 
"A simple function that wraps a fresh StringBuilder upon each invocation.
 Returns a StringBuilder with all args appended." 
[& args]
 (let [buffer (StringBuilder.)]
 (doseq [x args]
   (.append buffer x)) buffer))

(defn pdf->string ^String [^File src]
  (let [pd (PDDocument/load src)
        stripper (PDFTextStripper.)]
    (.getText stripper pd)))   


(defn pdf->txt [^String src & {:keys [s-page e-page dest]
                               :or {s-page 1 dest (str (first (stu/split src #"\.")) ".txt")}}]
 {:pre [(< 0 s-page) (.endsWith src ".pdf")]}
 (println "     \u001B[31mYOU ARE PERFORMING A POTENTIALLY ILLEGAL OPERATION...\n\t PROCEED AT YOUR OWN RISK!!!\u001B[m Proceed? (y/n):")
 (when  (-> *in*
              (java.util.Scanner.)
              .next
              (.charAt 0)
              (= \y))
 (with-open [pd (PDDocument/load (File. src))
             wr (io/writer dest)]
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

(defn- link* [c1 pos c2]
(let [others (if (satisfies? IWorkflow c2) (getComponents c2) c2)]
 (case pos
   :before  (if (map? others) (vector c1 others) (apply vector c1 others))
   :after   (if (map? others) (vector others c1) (apply vector others [c1]))
 "'pos' can be either :before or :after")))

(defn pool-map
"A saner, more disciplined version of pmap.
 Submits jobs eagerly but polls for results lazily.
 Don't use if original ordering of 'coll' matters."
([f coll threads]
 (let [exec (Executors/newFixedThreadPool threads)
       pool (ExecutorCompletionService. exec)
       futures (try (mapv (fn [x] (.submit pool #(f x))) coll)
               (finally (.shutdown exec)))]
(repeatedly (count futures) #(.. pool take get))))
([f coll]
  (pool-map f coll (+ 2 cpu-no))))


(defn fold-into-vec [chunk coll]
"Provided a reducer, concatenate into a vector.
 Same as (into [] coll), but parallel."
  (r/fold chunk (r/monoid into vector) conj coll))

(defn rmap
"A fork-join based mapping function that uses vectors underneath."
([f coll fj-chunk-size shuffle?]
  (fold-into-vec fj-chunk-size (r/map f (cond-> coll shuffle? shuffle (not shuffle?) vec))))
([f coll fj-chunk-size]
  (rmap f coll fj-chunk-size fj-chunk-size false))
([f coll]
  (rmap f coll 1)) )

(defn rhmap
"A high-performance, fork-join based mapping function that uses ArrayList underneath."
([f coll fj-chunk-size shuffle?]
  (r/fold fj-chunk-size r/cat r/append! (r/map f (cond-> coll shuffle? shuffle (not shuffle?) vec))))
([f coll fj-chunk-size]
  (rhmap f coll fj-chunk-size false))
([f coll]
  (rhmap f coll 1)) )

(defn mapr
"A pretty basic map-reduce style mapping function. Also supports shuffling which defaults to false. p-size defaults to (/ (count coll) (+ 2 cpu-no)).
 Will partition the data according to p-size and assign a future to each partition (per pmap). Assumes a large coll and a cheap f."
([f coll p-size shuffle?]
 (->> (cond-> coll shuffle? shuffle)
    (partition-all p-size)
    (pmap #(mapv f %) )
    (apply concat)   ;;concat the inner vectors that represent the partitions
    dorun) ) 
([f coll p-size]
  (mapr f coll p-size false))
([f coll]
  (mapr f coll (/ (count coll) 
                  (+ 2 cpu-no)))) )

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

(defn file-filter "Returns a reified FileFilter object that scans according the specified filter."
^FileFilter [^String filter]
(reify FileFilter
 (accept [this f]
  (boolean
  (re-find (re-pattern (str ".*" filter)) (.getName f))))))

(definline url? [x]
`(instance? java.net.URL ~x))

(defn file->data
"Read the file f back on memory safely.
 Contents of f should be a clojure data-structure. Not allowed to execute arbitrary code (per #=)."
[^String fname]
(io! (edn/read-string (slurp fname))))

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


(defmacro with-resources ;;credit to Meikel Brandmeyer (kotarak) for revisiting it from the book 'Joy of Clojure'
"Like 'with-open' but more generic in that it doesn't assume the resources are Closeable."
[bindings close-fn & body]
(assert  (vector? bindings) "bindings must be contained in a vector")
(assert  (even? (count bindings)) "'with-resources' requires an even number of forms in the binding vector")
(let [[x v & more] bindings]
 `(let [~x ~v]
    (try  ~(if-let [more (seq more)]
          `(with-resources ~more ~close-fn ~@body)
          `(do ~@body))
    (finally
      (~close-fn ~x))))))

(defn map->properties
"Converts a Map<String, String> to a java.util.Properties object."
[^java.util.Map property-value-map]
 {:pre [(every? #(every? string? %) property-value-map)]}
  (doto (java.util.Properties.)
    (.putAll property-value-map)))


(defn map-difference
  "Compares the 2 given maps and returns a map of their difference."
  [m1 m2]
  (let [ks1 (set (keys m1))
        ks2 (set (keys m2))
        ks1-ks2 (sets/difference ks1 ks2)
        ks2-ks1 (sets/difference ks2 ks1)
        ks1*ks2 (sets/intersection ks1 ks2)]
    (merge (select-keys m1 ks1-ks2)
           (select-keys m2 ks2-ks1)
           (select-keys m1
                        (remove (fn [k] (= (m1 k) (m2 k)))
                                ks1*ks2)))))
                                
(defn empty+ "Like clojure.core/empty but works with non-persistent collections too (via reflection)."
 [coll & ctor-args]
  (condp instance? coll 
    clojure.lang.IPersistentCollection   (.empty ^clojure.lang.IPersistentCollection coll)
    java.util.Collection  (clojure.lang.Reflector/invokeConstructor (class coll) (to-array ctor-args)) ;;slower but does the job nicely
   nil ))                                  

(defmacro let-timed "Just like 'let' but each binding expression will be timed."
  [bindings & body]
  (let [parts   (partition 2 bindings)
        names   (map first parts)
        results (map #(list 'clojure.core/time (second %)) parts)]
    `(let ~(vec (interleave names results))
       ~@body)))


(definline third [coll]
   `(-> ~coll next second))

 (definline fourth [coll]
   `(-> ~coll next next second))


(defn abbreviations-simple
"Extracts singleton (continuous string) abbreviations from a given string s, using 3 simple but empirically sound rules:
 Abbreviations appear right next to the proper name (right or left) at least once (typically the first time).
 In addition, on such occassions, they are almost always contained in parentheses (e.g. \"New-York (NY)\" or \"NY (New-York)\"),
 and the abbreviation itself only contains characters that are present in the proper name (ignoring case).
 Finally, abbreviations contain at least 2 characters (this has to be encoded in the regex, see 'hotel_nlp.concretions.regexes/re-abbreviation-paren)."
[s & abbr-regexes]
 (let [potentials  (apply concat (for [r abbr-regexes] (re-seq r s)))
       name-abbr-map (into {} (map (comp vec rest) potentials))   ;;first match is the entire expression (ignore it)
       char-present? (fn [ch s] (re-find (re-pattern (str "(?i)" ch)) (str s)))]
(reduce-kv
(fn [m n a]
(let [unparen (->> a (drop 1) drop-last)]
  (cond
     (every? #(char-present? % n) unparen)  (assoc m n a)  ;;ABBREVIATION CHARACTERS MUST ALL APPEAR IN THE NAME TO BE CONSIDERED VALID
     (every? #(char-present? % unparen) n)  (assoc m n a)
   :else m)))
{} name-abbr-map)))

(defn abbreviations-advanced [s chunk-fn NP-extractor]
 (let [;chunker  (hotel_nlp.core/fn->component chunk-fn)
      chunks (chunk-fn s)
      noun-phrases (NP-extractor chunks)
      with-parens (filter #(some #{"(" ")"} %) noun-phrases)
      remaining (map (fn [xs]
                      (remove #(some #{"the" "that" "a" "and" "this"} %) xs)) with-parens)]
      remaining))

;;EXAMPLE FOLLOWS FOR OPENNLP-java
#_(fn [sentence] ;;the chunk-fn
   (let [tokens   (.tokenize bin/opennlp-simple-tok sentence) ;;example tokenizer
         pos-tags (.tag (bin/opennlp-me-pos) tokens) ]
      (bin/chunk->spans (bin/opennlp-me-chunk) tokens pos-tags)))

#_(fn [chunk-spans] ;;the noun-phrase extractor
  (filter #(= "NP" (.getType %)) chunk-spans))

;;example for opennlp-clojure
#_(fn [s]
((opennlp.treebank/make-treebank-chunker "resources/pretrained_models/opennlp/en-chunker.bin")
 ((opennlp.nlp/make-pos-tagger "resources/pretrained_models/opennlp/en-pos-maxent.bin")
  (seq (run bin/opennlp-simple-tok s)))))

#_(fn [chunks]
  (opennlp.treebank/phrases
  (opennlp.tools.filters/noun-phrases chunks)))

(defn avg
(^double [xs n]
  (/ (reduce + xs) n))
(^double [xs]
 (avg xs (count xs))) )

(defn variance "Calculates the variance drawn from a sample of a population."
(^double [xs sample? n]
 (let [mean (avg xs n)
       intermediates (map #(Math/pow (- % mean) 2) xs)]
  (/ (reduce + intermediates) (if sample? (dec n) n))))
(^double [xs sample?]
  (variance xs sample? (count xs)))
(^double [xs]  ;;assume complete population (not sample)
  (variance xs false))    )

(definline std-dev "Computes the standard-deviation of a 1-dimensional sampled dataset."
 [xs sample?]
 `(Math/sqrt (variance ~xs ~sample?)))

(defn corr-coefficient "Correlation coefficient of 2 1-dimensional datasets."
([d1 d2 sample?]
  (assert (= (count d1) (count d2)) "The data-sets provided do not have the same size...")
  (let [size (count d1)]
    (* (/ 1 size)
       (reduce-kv #(+ %1 (* (/ (- %2 (avg d1 size)) (std-dev d1 sample?))
                            (/ (- %3 (avg d2 size)) (std-dev d2 sample?)))) 0 (zipmap d1 d2)))))
([d1 d2] (corr-coefficient d1 d2 nil)) )

(def day->int "Mapping of days to ints according to the java.util.Calendar class"
{:MONDAY 2 :TUESDAY 3 :WEDNESDAY 4 :THURSDAY 5 :FRIDAY 6 :SATURDAY 7 :SUNDAY 1})

(defn is-today? ;; (is-today? 'MONDAY)
"Returns true/false depending on whether the first argument (a symbol) is equal with the name of today's name. If called with no arguments today's name will be returned.
 (is-today? (is-today?)) will always return true ."
([s ^java.util.GregorianCalendar instant]
  (= (.get instant java.util.Calendar/DAY_OF_WEEK)
      ((keyword s) day->int)))
([s]
  (is-today? s (java.util.Calendar/getInstance)))
([]
 (let [inst (java.util.Calendar/getInstance)
       int-id (.get inst java.util.Calendar/DAY_OF_WEEK)]
  (-> (some (fn [[k v]] (when (= v int-id) k)) day->int)
    name
    symbol))) )

(defn serialize!
"Serialize the object b on to the disk using standard Java serialization."
[b ^String fname]
(with-open [oout (java.io.ObjectOutputStream.
                 (java.io.FileOutputStream. fname))]
  (.writeObject oout b)))

(defn deserialize!
"Deserializes the object in file f from the disk using standard Java serialization."
 [^String fname]
(with-open [oin (java.io.ObjectInputStream.
                (java.io.FileInputStream. fname))]
  (.readObject oin)))
 
(deftype ReducibleCharStream [filename]
clojure.core.protocols/CollReduce
 (coll-reduce [this f1]
   (clojure.core.protocols/coll-reduce this f1 (f1)))
 (coll-reduce [_ f1 init]
   (with-open [^java.io.Reader r  (io/reader filename)]
     (loop [ret init
            c (.read r)]
      (if (neg? c)  ret
        (let [ret (f1 ret (char c))]
          (if (reduced? ret) @ret
            (recur ret (.read r)))))))))
 
(defn slurp++ 
 "Reducible slurp which doesn't hold on to the head. 
  Returns a ReducibleCharStream object which you can use with any reducer." 
 [filename]
  (ReducibleCharStream. filename)) 
  
#_(defn spit++ "Like 'spit' but" 
[filename data]
 (with-open [^java.io.Writer wrt (io/writer filename)]
  (doseq [x data]
    (.write wrt (str x "\n")))))        


