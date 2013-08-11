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



(def reg-tok (RE-Tokenizer. re/token-regex :string :string-seq))
(def reg-seg (RE-Segmenter. re/sentence-segmentation-regex :string :string-seq))
(def stick   (RE-Combiner. :string :string-seq))
(def stemmer (PorterStemmer. "english" :string :string))
(def bigrams  (Ngrams. 2 :string :string))
(def MR-mapper  (MapReduceMapper. 5))
(def main (Workflow. [reg-seg reg-tok stemmer]))
(def alt  (Workflow. [reg-tok stemmer]))
(def levenshtein (LevenshteinDistance. :strings :number))
(def mr-mapper (MapReduceMapper. 100))
(def brown-nltk-pos-probs (help/read-resource "corpora-train/BROWN-NLTK/brown-probs.txt"))
(def pannotator-pluggable-sent-splitter  (Workflow. [reg-seg stick])) ;;replace reg-seg with your own sentence splitter
(def abbreviation-extractor (RE-Abbrv. [re/re-abbreviation-paren re/re-term-paren] :string :map))


