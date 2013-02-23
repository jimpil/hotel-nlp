(ns hotel_nlp.examples.workflows
   (:require  [hotel_nlp.protocols :refer :all]
              [hotel_nlp.externals.bindings :as bin]
              [hotel_nlp.concretions.models :refer :all]
              [hotel_nlp.helper :as help]
              [hotel_nlp.core :refer [defcomponent defworkflow fn->component]]
              [clojure.pprint :refer [pprint print-table]]
   )
   (:import [hotel_nlp.concretions.models Workflow])
)
;;these come first
(bin/extend-opennlp :all)
(bin/extend-stanford-core)
(def sample "Any edit that changes content in a way that deliberately compromises the integrity of Wikipedia is considered vandalism. The most common and obvious types of vandalism include insertion of obscenities and crude humor. Vandalism can also include advertising language, and other types of spam. Sometimes editors commit vandalism by removing information or entirely blanking a given page. Less common types of vandalism, such as the deliberate addition of plausible but false information to an article, can be more difficult to detect. Vandals can introduce irrelevant formatting, modify page semantics such as the page's title or categorization, manipulate the underlying code of an article, or utilize images disruptively.")  

(defcomponent opennlp-tok     bin/opennlp-simple-tok)
(defcomponent opennlp-ssplit (bin/opennlp-me-ssplit))
(defcomponent opennlp-pos    (bin/opennlp-me-pos))                                                                    
(defcomponent opennlp-ner    (bin/opennlp-me-ner))
                                  
(defworkflow opennlp-pipe   
  opennlp-ssplit opennlp-tok opennlp-pos) ;;a typical workflow
  
(def stanford-pipe ;;it is already a workflow - no need to use 'defworkflow'
  (bin/new-coreNLP (bin/new-properties "annotators" "tokenize" "ssplit" "pos")))

;;deploy the 2 workflows in parallel  
(let [stanford-res (future (bin/squeeze-annotation (deploy stanford-pipe sample))
      opennlp-res  (future (deploy opennlp-pipe sample))]
    {:opennlp  @opennlp-res 
     :stanford @stanford-res}  )  

