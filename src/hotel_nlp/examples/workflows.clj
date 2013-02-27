(ns hotel_nlp.examples.workflows
   (:require  [hotel_nlp.protocols :refer :all]
              [hotel_nlp.externals.bindings :as bin]
              [hotel_nlp.concretions.models :refer :all]
              [hotel_nlp.concretions.artefacts :refer [reg-seg reg-tok stemmer brown-nltk-pos-probs]]
              [hotel_nlp.algorithms.viterbi :as vit]
              [hotel_nlp.helper :as help]
              [hotel_nlp.core :refer [defcomponent defworkflow fn->component]]
              [clojure.pprint :refer [pprint print-table]]
   )
   (:import [hotel_nlp.concretions.models Workflow HMM-POS-tagger])
)
(def ^java.util.Properties s-properties (System/getProperties)) ;just in case we need them
(.setProperty s-properties "WNSEARCHDIR", "/home/sorted/WordNet-3.0/dict/") ;better to do this here rather than in project.clj (I think!)
                                          ;"/home/sorted/WordNet-3.0/dict/"
;;these should come first
(bin/extend-opennlp)
;(bin/extend-opennlp :modules [[:ner bin/spans->strings] [:chunk bin/chunk->spans]])
(bin/extend-stanford-core)


(def bbc-sample ;;some bbc-sample sentences from the BBC news website
"Any edit that changes content in a way that deliberately compromises the integrity of Wikipedia is considered vandalism. The most common and obvious types of vandalism include insertion of obscenities and crude humor. Vandalism can also include advertising language, and other types of spam. Sometimes editors commit vandalism by removing information or entirely blanking a given page. Less common types of vandalism, such as the deliberate addition of plausible but false information to an article, can be more difficult to detect. Vandals can introduce irrelevant formatting, modify page semantics such as the page's title or categorization, manipulate the underlying code of an article, or utilize images disruptively. Mr. Brown is dead after someone shot him!") 
(def op-sample
"Pierre Vinken, 61 years old, will join the board as a nonexecutive director Nov. 29. Mr. Vinken is chairman of Elsevier N.V., the Dutch publishing group. Rudolph Agnew, 55 years old and former chairman of Consolidated Gold Fields PLC, was named a director of this British industrial conglomerate.")
(def easy-sample "Mary likes pizza but she also likes kebaps. Knowing her, I'd give it 2 weeks before she turns massive!")


(defcomponent opennlp-tok    "openNLP's simple tokenizer"    bin/opennlp-simple-tok)
(defcomponent opennlp-ssplit "openNLP's maxent sentence-splitter" (bin/opennlp-me-ssplit))
(defcomponent opennlp-pos  "openNLP's maxent pos-tagger"  (bin/opennlp-me-pos))                                                                    
(defcomponent opennlp-ner "openNLP's maxent ner [person]"   (bin/opennlp-me-ner))
(defcomponent opennlp-chunk "openNLP's maxent chunker"   (bin/opennlp-me-chunk))
(defcomponent opennlp-parse "openNLP's maxent parser"   (bin/opennlp-me-parse))
(defcomponent opennlp-coref "openNLP's coreference linker" (bin/opennlp-me-coref)) 
(defcomponent my-ssplit "my own sentence-splitter" reg-seg)
(defcomponent my-tokenizer "my own sentence-splitter" reg-tok)
(defcomponent porter-stemmer "my own sentence-splitter" stemmer)
(defcomponent my-pos-tagger "my own HMM pos-tagger based on bigrams." 
  (HMM-POS-tagger. (comp vit/proper-statistics vit/tables) vit/viterbi {:probs brown-nltk-pos-probs} nil)) ;;pass the pre-observed probabilities as meta-data
 


(defworkflow my-stem-pipe "my own basic stemming pipe" my-ssplit my-tokenizer porter-stemmer)
(defworkflow my-pos-pipe "my own basic pos-tagging pipe" my-ssplit my-tokenizer my-pos-tagger)
                                  
(defworkflow opennlp-basic-pipe "A common openNLP workflow."  
  opennlp-ssplit 
  opennlp-tok
  ;opennlp-parse 
  opennlp-pos 
  ;opennlp-ner
  ;opennlp-chunk  
 ) ;;a typical and common workflow
 
(defworkflow opennlp-parsing-pipe "A parsing openNLP workflow."  
  opennlp-ssplit 
  opennlp-tok
  opennlp-parse  
 )

(defworkflow opennlp-ner-pipe "A parsing openNLP workflow."  
  opennlp-ssplit 
  opennlp-tok
  opennlp-ner 
 ) 

; (run opennlp-coref (deploy opennlp-parsing-pipe bbc-sample) ["person" (deploy opennlp-ner-pipe bbc-sample) nil])
; (filter #(< 1 (.getNumMentions ^opennlp.tools.coref.DiscourseEntity %)) *1)
 
 
(defworkflow mixed-pipe1 "a pipe with mixed components" my-ssplit my-tokenizer opennlp-pos) 
;(defworkflow mixed-pipe2 "another pipe with mixed components" my-ssplit my-tokenizer opennlp-pos)  

;;openNLP's chunker expects both tokens and pos-tags. This makes it slightly odd to use inside the workflow.  
;;nothing stops us to use it outside though. For example one can do this:
#_(let [redux (deploy opennlp-basic-pipe easy-sample true)] ;;deploy the standard workflow first and ask for reductions
 (run opennlp-chunk   (nth redux 2)   ;the tokens
                      (nth redux 3))) ;the pos-tags 
                    
;;or this (nice demo of fn->component as well)
(defworkflow opennlp-chunking-pipe "A chunking openNLP workflow." 
                   (fn->component #(deploy mixed-pipe1 % true)) ;notice how now we're using mixed-pipe1 - no difference! 
                   (fn->component #(zipmap (nth % 2)  (nth % 3))) 
                   (fn->component #(reverse (run opennlp-chunk (keys %) (vals %))))) ;;reverse here due to zipping
;(deploy opennlp-chunking-pipe bbc-sample) 
                
  
#_(def stanford-pipe "A common stanford-nlp workflow."  ;;it is already a workflow - no need to use 'defworkflow'
  (bin/new-coreNLP (bin/new-properties "annotators" "tokenize" "ssplit" "pos" "lemma" ))) ;"ner" "parse" "dcoref"

;;deploy the 2 workflows in parallel 
#_(defn opennlp-vs-stanford [] 
(let [stanford-res (future (bin/squeeze-annotation (deploy stanford-pipe bbc-sample)))
      opennlp-res  (future (deploy opennlp-basic-pipe bbc-sample))]
    {:opennlp  @opennlp-res 
     :stanford @stanford-res}  )  )

