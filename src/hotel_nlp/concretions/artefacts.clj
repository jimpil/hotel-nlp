(ns hotel_nlp.concretions.artefacts
   (:require  [hotel_nlp.helper :as help]
              [hotel_nlp.concretions.models]
   )
   (:import [hotel_nlp.concretions.models RE-Tokenizer RE-Segmenter RE-Combiner PorterStemmer Workflow Ngrams LevenshteinDistance 
                                      MapReduceMapper ForkJoinMapper PoolMapper SemiLazyMapper]
   
   )
)



(def token-regex #"[\w\d/]+|[\-\,\.\?\!\(\)]")
(def sentence-regex  #"(?<=[.!?]|[.!?][\\'\"])(?<!e\.g\.|i\.e\.|vs\.|p\.m\.|a\.m\.|Mr\.|Mrs\.|Ms\.|St\.|Fig\.|fig\.|Jr\.|Dr\.|Prof\.|Sr\.|\s[A-Z]\.)\s+")
(def reg-tok (RE-Tokenizer. token-regex :string :string-seq))
(def reg-seg (RE-Segmenter. sentence-regex :string :string-seq))
(def stick   (RE-Combiner. :string :string-seq))
(def stemmer (PorterStemmer. "english" :string :string))
(def n-grams  (Ngrams. 2 :string :string))
(def MR-mapper  (MapReduceMapper. 5))
(def main (Workflow. [reg-seg reg-tok stemmer]))
(def alt  (Workflow. [reg-tok stemmer]))
(def levenshtein (LevenshteinDistance. :strings :number))
(def mr-mapper (MapReduceMapper. 100))
(def brown-nltk-pos-probs (help/read-resource "corpora-train/BROWN-NLTK/brown-probs.txt"))
(def pannotator-pluggable-sent-splitter  (Workflow. [reg-seg stick]))


