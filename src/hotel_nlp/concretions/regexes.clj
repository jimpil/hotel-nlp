(ns hotel_nlp.concretions.regexes
    #_(:require [hotel_nlp.protocols :as pro]
                [hotel_nlp.helper :as help])






)


(def sentence-segmentation-regex  #"(?<=[.!?]|[.!?][\\'\"])(?<!e\.g\.|i\.e\.|vs\.|p\.m\.|a\.m\.|Mr\.|Mrs\.|Ms\.|St\.|Fig\.|fig\.|Jr\.|Dr\.|Prof\.|Sr\.|\s[A-Z]\.)\s+")
(def token-regex #"[\w\d/]+|[\-\,\.\?\!\(\)]")
(def _LEAD  #"^[aeiouy]*(?:qu|[bcdfghjklmnpqrstvwxz])+")    ;;match 0 or more vowels + 1 or more consonants at the start of the word 
(def _INNER #"\B[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+\B") ;;match 1 or more vowels + 1 or more consonants inside a word 
(def _TRAIL #"[aeiouy]+(?:qu|[bcdfghjklmnpqrstvwxz])+$")    ;;match 1 or more vowels + 0 or more consonants at the end of a word 
