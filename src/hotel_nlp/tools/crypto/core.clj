(ns hotel_nlp.tools.crypto.core
  (:require [hotel_nlp.helper :as help]))

(defn ^bytes rand-bytes
"Returns an array of random bytes, using the java.security.SecureRandom class, 
 which provides a cryptographically strong pseudo-random number generator (PRNG)." 
[size]
  (let [random (java.security.SecureRandom/getInstance "SHA1PRNG")
        buffer (make-array Byte/TYPE size)]
    (.nextBytes random buffer) 
    buffer))
    
(defn encrypt
"Returns a map containing two seqs -> :pad & :msg. 
 The message is plain binary data, so if you need it as a string 
 you need to turn it in to hex string or encode it using Base64." 
[^String m]
  (let [message (.getBytes m)
        size (alength message)
        pad  (rand-bytes size)
        code (map bit-xor message pad)]
    {:pad (seq pad) 
     :msg (seq code)}))
    
(defn ^String decrypt
"Given a pad and some encoded message, return the original String message." 
[pad message]
  (->> message
     (map bit-xor pad)
     (map char) 
     (apply str)))
