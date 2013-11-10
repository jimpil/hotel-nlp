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
   
   
   
;;let's see...
(def toy "a toy parser just to get warm."
(insta/parser
  "S = NP SPACE VP
   SPACE = #'\\s+'
   VP = VB SPACE NP
   NP = DET? SPACE? ADJ? SPACE? N 
   N = 'boy' | 'girl'
   VB = 'sees' | 'likes'
   ADJ = 'big' | 'small'
   DET = 'a' | 'the'" ))
   
   
(def parsePK
  (insta/parser
   "S  = TOKEN (SPACE TOKEN)* END
   TOKEN = (NUM | DRUG | PK | MECH | SIGN | EFF) / WORD
   <WORD> = #'\\w+'
    NUM =  #'[0-9]+'
    ADV =   #'[a-z]+ly'
   <SPACE> = #'\\s+'
    DRUG =  #'(?i)didanosine|quinidine'
    PK =    #'(?i)exposure|bioavailability|lower?[\\s|\\-]?clearance'
    MECH =  #'[a-z]+ed'
    EFF =  SIGN? MECH | MECH SIGN? 
    SIGN =  ADV | NEG
    NEG = 'not'
    <TO> = 'to' | 'of'
    <BE> = 'is' | 'was'
  (*  DO = 'does' | 'do' | 'did' *) 
  (* <COMMA> = ',' *)
  (* <OTHER> = 'as' | 'its' | 'by' *)
    END =  '.' " ))
   
(parsePK "exposure to quinidine was not importantly changed as indicated by its lower clearance.")  
       
   
                   
