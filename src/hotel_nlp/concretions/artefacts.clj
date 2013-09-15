(ns hotel_nlp.concretions.artefacts
   (:require  [hotel_nlp.helper :as help]
   	      [hotel_nlp.concretions.regexes :as re]
              [hotel_nlp.concretions.models]
   )
   (:import [hotel_nlp.concretions.models RE-Tokenizer RE-Segmenter RE-Combiner RE-Abbrv PorterStemmer
             Ngrams LevenshteinDistance MapReduceMapper ForkJoinMapper PoolMapper SemiLazyMapper]
             [hotel_nlp.helper Workflow]
   
   )
)



(defonce reg-tok (RE-Tokenizer. re/token-regex :string :string-seq))
(defonce reg-seg (RE-Segmenter. re/sentence-segmentation-regex :string :string-seq))
(defonce stick   (RE-Combiner. :string :string-seq))
(defonce stemmer (PorterStemmer. "english" :string :string))
(defonce bigrams  (Ngrams. 2 :string :string))
(defonce MR-mapper  (MapReduceMapper. 5))
(defonce main (Workflow. [reg-seg reg-tok stemmer]))
(defonce alt  (Workflow. [reg-tok stemmer]))
(defonce levenshtein (LevenshteinDistance. :strings :number))
(defonce mr-mapper (MapReduceMapper. 100))
(defonce brown-nltk-pos-probs (help/read-resource "corpora-train/BROWN-NLTK/brown-probs.txt"))
(defonce pannotator-pluggable-sent-splitter  (Workflow. [reg-seg stick])) ;;replace reg-seg with your own sentence splitter
(defonce abbreviation-extractor (RE-Abbrv. [re/re-abbreviation-paren re/re-term-paren] :string :map))


