(ns hotel_nlp.app.remote.srepl
 (:require  ;[clojure.tools.nrepl :as repl]
             [clojure.tools.nrepl.transport :as t]
             [clojure.tools.nrepl.server :refer [start-server stop-server]]))	
 


;(defonce server (atom (repl/start-server :port 7888)))

(defmacro defserver [name port tty?]
`(defonce ~name (atom 
  (if ~tty? (start-server :transport-fn t/tty :port ~port) (start-server :port ~port)))))

(defn stop [s]
 (stop-server @s))
 
#_(defn connect-to [server] 
(with-open [conn (repl/connect :port (:port server))]
     (-> (repl/client conn 1000)))) 
