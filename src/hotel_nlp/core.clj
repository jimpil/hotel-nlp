(ns hotel_nlp.core
   (:require  [clojure.tools.macro :refer [name-with-attributes]];;useful for writing def-something macros
              [hotel_nlp.protocols :refer :all]
              [hotel_nlp.helper :as help])
   (:import [hotel_nlp.helper Workflow])
)


(definline component? 
"Tests if c satisfies IComponent." [c]
 `(satisfies? IComponent ~c)) 
 
(defn fn->component [f]
(reify IComponent
 (link [this pos other]
   (Workflow. (help/link* this pos other))) 
 (run [_ args] 
   (f args)))) 
 
(defmacro defcomponent 
"Defines a top-level Component with the specified name and optionally doc-string and attr-map on the var. 
 The only thing this does over a plain 'def' is checking whether the last argument is a valid IComponent." 
[name & args] 
(let [[name attrs] (name-with-attributes name args)
       c (first attrs)] ;;the actual component  
`(do
   (assert (component? ~c) (str "Not a valid IComponent : " ~c))
   (def ~name ~c)) ))
   
   
(defmacro defworkflow 
"Defines a top-level Workflow with the specified name optionally doc-string and attr-map on the var.
The only thing this does over a plain 'def' is checking whether the the list of components (the last arg) are all valid IComponents."  
[name & args]
(let [[name attrs] (name-with-attributes name args)]    
`(let [cs-coll#  ~(vec attrs)] 
   (assert (every? component? cs-coll#) "Can only accept IComponents")
   (def ~name (Workflow. cs-coll# {:description (-> ~name meta :doc)} nil))) )) 

  
  

  

   
 
 
