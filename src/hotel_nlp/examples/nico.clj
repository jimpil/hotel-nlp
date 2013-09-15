(ns hotel_nlp.examples.nico
   (:require  [opennlp.nlp :refer [make-document-categorizer]]
              [opennlp.tools.train :refer [train-document-categorization write-model]]))
;------------------------------------------------------------------------------------------------------------------              

(defn train-model 
"Trains a bag-of-words based document-categorizer and returns it." 
[^String train-file]
 (->  train-file
   train-document-categorization
   make-document-categorizer))   
  
(defonce model (comp :best-category 
                     (train-model "resources/doccat.train"))) ;;model ready after this line - use it as a regular function     

(println (model "Microsoft shows interest to buy Nokia.")) ;; "Technology", clue = 'Microsoft'
(println (model "Japan is shutting down its last functioning nuclear reactor, with no timetable for a restart.")) ;; "Health", clue = 'nuclear'
(println (model "Five people are being questioned by police about a suspected arson attack which killed four people in Leicester.")) ;; "News", clue = 'people'

(comment
 
 (write-model m "resources/doccat.bin")

)

                                  
