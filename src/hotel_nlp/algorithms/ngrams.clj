(ns hotel_nlp.algorithms.ngrams)


(defn ngrams*
 "Create ngrams from a seq s. 
  Pass a single string for character n-grams or a seq of strings for word n-grams."
  [s n] 
   (partition n 1 s))
      
      
(defn n-grams-count
  "Used to create n-grams with the specified numbers
  A seq of numbers for the ngrams to create. 
  The documents to process with the ngrams 
*returns*
  A map of the ngrams"
  [numbers documents]
  (reduce (fn [counts number]
             (reduce (fn [counts document]
                       (reduce (fn [counts freqs]
                                 (let [ngram (first freqs)]
                                   (if-let [val  (counts ngram)]
                                     (assoc counts ngram (+ val (second freqs)))
                                     (assoc counts ngram (second freqs)))))
                               counts (frequencies (ngrams* document number))))
                     counts documents)) {} (if (sequential? numbers) numbers (list numbers))))

(defn add-ngrams-to-document
  "Used to add ngrams to the document.

*numbers*
  The number of ngrams to create. <br />
*document*
  The document to process. <br />
*processed-document*
  The processed-document to add the ngrams too. <br />
*ngrams-count*
  The ngrams map. <br />
*return*
  The processed document with the added ngrams"
  [numbers document processed-document ngrams-count]
  (reduce (fn [processed-document ngram]
            (if (contains? ngrams-count (first ngram))
              (assoc processed-document :counts
                (assoc (:counts processed-document) (first ngram) (second ngram))
                :number-of-words (+ (:number-of-words processed-document) (second ngram)))
              processed-document)) processed-document (n-grams-count numbers (list document))))       
