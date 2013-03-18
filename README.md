# hotel-nlp

A Clojure roof for several 'foreign' NLP libraries/frameworks.  

## Motivation
Natural Language and text-mining researchers who know (or are willing to learn) Python have a united and comprehensive set of tools to work with in their favourite language called, NLTK. 
Pretty much in the same way that NumPy is the de-facto standard for scientific computing, NLTK is the de-facto standard for NLP. On the JVM things are a bit different though...
There exist perhaps dozens of great but incompatible NLP libraries. Many of them are open-source, which is good, but the fact that they are completely incompatible doesn't help at all. 
It is practically impossible to combine components from several libraries in a mix-and-match fashion, thus creating workflows. In text-mining, the concept of a work-flow is quite distinct but 
in functional-progrmaming terms it's nothing more than plain function composition. The one software that does this sort of thing is, perhaps, U-Compare [1] which sits on top of UIMA [2]. However, U-Compare is NOT an API. It is an end-product - a platform if you like. Therefore, you cannot expect to get results from U-Compare in a computable format (e.g. if you ask for tokenization, you'll get your tokenized text on-screen). For many, if not most researchers, an experiment doesn't stop there...

Drawing inspiration from core.matrix [3], hotel-nlp aims to bring capabilities similar to U-Compare but down to the API level. We should be able to provide a common API that can be extended to 
any other implementation, essentially making it ours without any wrapping (via protocols only). As a text-mining researcher, I strongly believe there is great value in putting all these great capabilities from several 'foreign' libraries/tools under a common & extensible roof. What core.matrix aims to do for matrix-libs, hotel-nlp aims to do with NLP-libs (openNLP, stanford-NLP, GATE etc.)          

## Usage

FIXME


##References
[1] http://www.nactem.ac.uk/u-compare.php     
[2] http://uima.apache.org/    
[3] https://github.com/mikera/matrix-api     

## License

Copyright © 2013

Distributed under the Eclipse Public License, the same as Clojure.
Note: the used implementation of the Snowball stemming algorithm is licensed under the BSD license.
