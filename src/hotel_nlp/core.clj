(ns hotel_nlp.core
   (:require  [hotel_nlp.protocols :refer :all]
              [hotel_nlp.externals.bindings :refer :all]
              [hotel_nlp.concretions.models]
              [hotel_nlp.helper :as help]
   )
   (:import [hotel_nlp.concretions.models Workflow])
)


(definline component? 
"Tests if c satisfies IComponent." [c]
 `(if (satisfies? IComponent ~c) true false))
 
(defn fn->component [f]
(reify IComponent
 (link [this pos other]
   (Workflow. (help/link this pos other))) 
 (run [_ args] 
   (apply f args)))) 
 
(defmacro defcomponent
"Defines a top-level Component with the specified name. 
 co must satisfy IComponent." 
[name co]
`(let [c# ~co]  
  (assert (component? c#) "Not a valid IComponent")
  (def ~name c#))) 

(defmacro defworkflow 
"Defines a top-level Workflow with the specified name containing the given Components."  
[name & components]
`(let [cs# (vector ~@components)]
  (assert (every? component? cs#) "Can only accept IComponents")
  (def ~name (Workflow. cs#))))
  
  
#_(defmacro  defworkflow 
"Defines a top-level Workflow with the specified name containing the given Components."  
 ([name & components]
 `(let [cs# (vector ~@components)]
   (assert (every? component? cs#) "Can only accept IComponents")
   (def  ~name  (Workflow. cs#))))
 ([name doc-string & comps] 
 `(let [cs# (vector ~@comps)]
   (assert (every? component? cs#) "Can only accept IComponents")
   (def ^{:doc doc-string} ~name  (Workflow. cs#)))) )  
  

   
 
 
