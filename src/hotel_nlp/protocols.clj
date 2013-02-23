(ns hotel_nlp.protocols)

(defprotocol ITokenizer
(tokenize [this sentence]))

(defprotocol IStemmable
(stem [this] [this token] [this token lang])
(getRoot [this token dictionary]))

(defprotocol ISegmenter
(segment [this text]))

(defprotocol ISticker
(stick [this strings] [this strings in-between]))

(defprotocol ITaggable
(tag [this text tagging-scheme]))

(defprotocol IAnnotator
(annotate [this arg-map] [this entity target] [this entity corpus target]))

(defprotocol IDistance
(getDistance [this other] [this s1 s2] [this s1 s2 m-weight]))

(defprotocol IDictionary
(combine [this other more]))

(defprotocol IModel
(predict [this knowledge sample]))

(defprotocol IAllignable
(allign [this other]))

(defprotocol INormaliser
(normalize [this token]))

(defprotocol ITrainable
(train [this data]))

(defprotocol IComponent
(run [this args] [this args more])
(getIOTypes  [this])
(link [this position other])) ;;:before or :after some component

#_(defprotocol IExecutable
(execute [this args]))

(defprotocol IWorkflow
(getComponents [this])
(addComponent [this pos component])
(appendComponent [this component])
(removeComponent [this pos])
(replaceComponent [this pos replacement])
(deploy [this text] [this text intermediates?]))

(defprotocol IFFilter
(select [this ffilter]))

(defprotocol IReporter
(report [this where]))

(defprotocol IGenerator
(generate [this] [this number] [this number limitations]))

(defprotocol IRipper
(rip [this file]))

(defprotocol IExtractor
(extract [this] [this args]))

(defprotocol ITimer
(timing [this]))

(defprotocol IGram 
(ngrams [this n]))

(defprotocol IProbabilistic
(observe [this data] [this data previous-probs]))

(defprotocol IEvaluator
(evaluate [this predictions ideals]))

(defprotocol IParallelMapper
 (par-map [this f coll]))


;(map-sl [this f coll])  ;pmap (semi-lazy)
;(map-fj [this f coll] [this f coll fj-size]) ;r/map (reducers map)
;(map-exec [this f coll] [this f coll pool-size]) ;pool-map (executors)
;(map-reduce [this f coll] [this f coll partition-no])) ;;basic map-reduce








