(ns hotel_nlp.concretions.regexes
    #_(:require [hotel_nlp.protocols :as pro]
                [hotel_nlp.helper :as help])






)


(def sentence-segmentation-regex  "Regular expression capable of identifying sentence boundaries."  
	#"(?<=[.!?]|[.!?][\\'\"])(?<!e\.g\.|i\.e\.|vs\.|p\.m\.|a\.m\.|Mr\.|Mrs\.|Ms\.|St\.|Fig\.|fig\.|Jr\.|Dr\.|Prof\.|Sr\.|\s[A-Z]\.)\s+")
(def token-regex "Regular expression capable of identifying word boundaries." 
	#"[\w\d/]+|[\-\,\.\?\!\(\)]")
(def abbreviation-regex "Regular expression capable of identifying abbreviations (NOT acronyms!) contained in parentheses (at least 2 characters)." 
      #"(?i)[a-zA-Z0-9-]*\s?\(\w{2,}\)")       				    ;;match any word followed (possibly) by a /space, followed by non-empty parentheses
(def _LEAD  "Regular expression that matches 0 or more vowels + 1 or more consonants at the start of the word."
   #"^[aeiouy]*(?:qu|[bcdfghjklmnpqrstvwxz])+")    
(def _INNER  "Regular expression that matches 1 or more vowels + 1 or more consonants inside a word."
   #"\B[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+\B") 
(def _TRAIL  "Regular expression that matches 1 or more vowels + 0 or more consonants at the end of a word."
  #"[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+$") 
