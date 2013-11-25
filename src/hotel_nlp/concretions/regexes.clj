(ns hotel_nlp.concretions.regexes
    #_(:require [hotel_nlp.protocols :as pro]
                [hotel_nlp.helper :as help])
)




(def sentence-segmentation-regex  "Regular expression capable of identifying sentence boundaries. To be used with clojure.string/split."  
  ;#"(?<=[.!?]|[.!?][\\'\"]|\n{2,})(?<!\([e|i]|\.[g|e]|vs|[0-9]{1,2}\s{1,2}(p|a)|\.m|Mr|Mrs|Ms|St|Fig|fig|Jr|Dr|Prof|Sr|[0-9])\s*" 
  #"(?<!\([e|i]|\.[g|e]|vs|[0-9]{1,2}\s{1,2}(p|a)|\.m|Mr|Mrs|Ms|St|Fig|fig|Jr|Dr|Prof|Sr|[0-9])([.!?]|[.!?][\\'\"]|\n{2,})(?=\s*[A-Z0-9])" )
(def token-regex "Regular expression capable of identifying word boundaries." 
   #"[\p{L}\d/]+|[\-\,\.\?\!\(\)]")
(def re-abbreviation-paren "Regular expression capable of identifying abbreviations (NOT acronyms!) contained in parentheses (at least 2 characters), following a proper term." 
   #"(?i)([a-zA-Z0-9-]*)\s?(\(\w{2,}\))")   ;;match any word followed (possibly) by a /space, followed by non-empty parentheses
(def re-term-paren "Regular expression capable of identifying proper terms contained in parentheses, following an abbreviation."
   #"(?i)(\w{2,})\s?(\([a-zA-Z0-9-]*\))")  ;;the previous regex inverted
(def _LEAD  "Regular expression that matches 0 or more vowels + 1 or more consonants at the start of the word."
   #"^[aeiouy]*(?:qu|[bcdfghjklmnpqrstvwxz])+")    
(def _INNER  "Regular expression that matches 1 or more vowels + 1 or more consonants inside a word."
   #"\B[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+\B") 
(def _TRAIL  "Regular expression that matches 1 or more vowels + 0 or more consonants at the end of a word."
   #"[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+$") 
(def re-date "Matches dd mm yyyy with optional seprators [-  /  .]. Will return all matches along with their inner constituents (day month year)." 
   #"\b(0?[1-9]|[12][0-9]|3[01])[- /\.](0?[1-9]|1[012])[- /\.]((?:19|20|')?\d\d)\b")
(def re-email "Matches 99% of valid e-mail addresses as found in text. If need to validate user input replace word boundaries [\b] with start/end of input anchors [^,$]" 
   #"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,4}\b")
(def re-ip "Matches IP addresses (adopted from http://www.regular-expressions.info/regexbuddy/ipaccurate.html)" 
   #"\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b")
(def re-double-quotes "Matches double quoted strings." #"\"[^\"\r\n]*\"")
(def re-url-simple #"(\bhttps?:\/\/)?www\d?\..+\b") ;simple
(def re-url-advanced #"/^(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$/");a bit more advanced
(def re-prices "Matches prices preceded by the dollar sign [$] and followed by optional decimal points letter stating million, billion etc, e.g: [$200,47m]" 
	#"\$\d+(,\d\d)?[a-zA-Z]?")
(def re-CYP "Regular expression capable of matching metabolites from the CYP family."
   #"(?:cyp|CYP|P450|CYP450)?[0-9]+[a-zA-Z][0-9]+" ) 
(def re-mechanisms 
  [ #"inhibit(?:e(?:s|d)?|ing|ions?|or)"
    #"cataly(?:z|s)(?:e(?:s|d)?|ing)" 
    #"metaboli(?:(?:z|s)(?:e(?:s|d)?|ing)|sm)"
    #"correlat(?:e(?:s|d)?|ing|ions?)"
    #"induc(?:e(?:s|d)?|ing|tions?|or)"
    #"stimulat(?:e(?:s|d)?|ing|ions?)"
    #"activ(?:e(?:s)?|(?:at)(?:e(?:s|d)?|ing|ions?))"
    #"form(?:(?:s|ed|ing|ations?)?)" 
    #"suppress(?:e(?:s|d)?|ing|ions?)"
    #"increas(?:e(?:s|d)?|ing)"
    #"decreas(?:e(?:s|d)?|ing)"
  ])
  
(def re-numbers "Regular expression capable of identifying numbers followed by their unit of measure." 
 #"([0-9]+)\s*([a-zA-Z//]+)\s*"  )

  

