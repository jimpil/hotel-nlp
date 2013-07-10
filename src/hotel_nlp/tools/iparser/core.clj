(ns hotel_nlp.tools.iparser.core
   (:require [clojure.pprint :refer [pprint]]
             [instaparse.core :as insta]
             [hotel_nlp.helper    :as help]) )
;-----------------------------------------------------------------------------------------------------------------------------------             
;----------------------------<EXPERIMENTAL CODE>------------------------------------------------------------------------------------  

;;from the instaparse readme
(def words-and-numbers
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = word | number
     whitespace = #'\\s+'
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'")) 
     
(def arithmetic
  (insta/parser
    "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))
     
(def arithmetic-translation 
 {:add + 
  :sub - 
  :mul * 
  :div /
  :number clojure.edn/read-string 
  :expr identity} )
  
(defmacro infix-arithmetic [& body]
`(let [no-spaces# (apply str (remove #(re-matches #"\s+" (str %)) (str '~body)))]
   (insta/transform arithmetic-translation (arithmetic no-spaces#))) )               
