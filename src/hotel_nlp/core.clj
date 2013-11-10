(ns hotel_nlp.core
   (:require  [clojure.tools.macro :refer [name-with-attributes]];;useful for writing def-something macros
              [rhizome.viz :as viz]
              [hotel_nlp.protocols :refer :all]
              [hotel_nlp.helper :as help])
   (:import [hotel_nlp.helper Workflow GraphWorkflow])
)


(definline component? 
"Tests if c satisfies IComponent." [c]
 `(satisfies? IComponent ~c))
 
(definline workflow? 
"Tests if c satisfies IWorkflow." [w]
 `(satisfies? IWorkflow ~w))   
 
(defn fn->component [f]
(reify IComponent
 (link [this pos other]
   (help/linkage this pos other)) 
(run [_ args] 
  (try (apply f args)
  (catch clojure.lang.ArityException are 
       (f args))))   
(run [_ arg1 args] 
  (try (apply f arg1 args)
  (catch clojure.lang.ArityException are 
       (f arg1 args))))  ))       
   
#_(defn fn->component [f]
(reify IComponent
 (link [this pos other]
   (help/linkage this pos other)) 
(run [_ args] (f args))   
(run [_ arg1 args] (f arg1 args)) ))    
 
#_(defmacro defcomponent 
"Defines a top-level Component with the specified name and optionally doc-string and attr-map on the var. 
 The only thing this does over a plain 'def' is checking whether the last argument is a valid IComponent." 
[name & args] 
(let [[name attrs] (name-with-attributes name args)
       c (first attrs)] ;;the actual component  
`(do
   (assert (component? ~c) (str "Not a valid IComponent : " ~c))
   (def ~name ~c)) ))
   
(defmacro defcomponent 
"Defines a top-level Component with the specified name and optionally doc-string and attr-map on the var. 
 You can pass anything satisfying IFn and you'll get back a reified IComponent. Careful not to pass a fn of arity > 2.
 If you need to do that consider putting all the arguments in a list and using the 1-arg overload or 'run'.
 If the thing you pass is already a Component, it won't be touched so it will boil down to a regular def." 
[name & args] 
(let [[name attrs] (name-with-attributes name args)
       c (first attrs)] ;;the actual component  
`(let [component# ~c 
       fcomponent# (cond-> component#  
                     (fn? component#) fn->component
                     (component? component#) identity) ]
  (assert (component? fcomponent#) "UNEXPECTED COMPONENT TYPE! ABORTING... ")                   
   (def ~name fcomponent#)) ))   
   
;  (defcomponent tok "a simple tokenizer that expects 3 arguments - an arity not acceptable by run"   
;     (fn [^String text delims with-delims?]
;        (enumeration-seq 
;         (java.util.StringTokenizer. text (or delims " \t\n\r\f") with-delims?))))     
;(run tok ["the fox jumped pver the lazy dog" nil false])     
   
(defmacro defworkflow 
"Defines a top-level Workflow with the specified name optionally doc-string and attr-map on the var.
You can pass in an arbitrary number of valid Components and you'll get back a Workflow object. 
The var's doc-string will also be attached to the Workflow's metadata under :description."  
[name & args]
(let [[name attrs] (name-with-attributes name args)]    
`(let [cs-coll#  ~(vec attrs)] 
   (assert (every? component? cs-coll#) "Can only accept IComponents")
   (def ~name (Workflow. cs-coll# {:description ~(-> name meta :doc)} nil))) )) 

  
(defmacro defgraph 
"Defines a top-level Workflow with the specified name optionally doc-string and attr-map on the var.
You can pass in an arbitrary number of [:key fnk] and you'll get back a GraphWorkflow object. 
The var's doc-string will also be attached to the GraphWorkflow's metadata under :description. 
An additional arg is supported last which specifies the view of this graph (for graphical representation)."  
[name & args]
(let [[name attrs] (name-with-attributes name args)]    
`(let [cs-coll#  ~(->> attrs (partition 2) (map vec) (into {}))
       attrs#  (drop (* 2 (count cs-coll#)) '~attrs)
       view#  (if (empty? attrs#) nil (eval (first attrs#)))] 
 (def ~name (GraphWorkflow. cs-coll# {:description ~(-> name  meta :doc) :view view#} nil))) ))   

(defn draw 
"Draws your workflow using circles and arrows. Relies on Graphiz being installed." 
 [wf & opts]
(assert (workflow? wf) (str "Not a valid IWorfklow: " wf))
(let [g (if (instance? GraphWorkflow wf) (-> wf meta :view)  
          (-> (reduce 
             (fn [init [c1 c2]]
                 (assoc init c1 [c2])) 
             {} (->> wf 
                  :components
                  (partition 2 1)))
          (assoc (-> wf :components last) []))) ]  
 (apply viz/view-graph (keys g) g 
   (conj opts (fn [n] {:label (-> n class .getSimpleName)}) :node->descriptor)) ))  

   
 
 
