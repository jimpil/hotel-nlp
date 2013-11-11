(ns hotel_nlp.app.gui.core
  (:require [clojure.string :as s]
            [seesaw.core :as ssw] 
            [seesaw.chooser :as choo]
            [seesaw.swingx :as ssx])
   (:import [javax.swing UIManager])
)
            
            
(def brand-new {:curr-text " "
                :result " "
                :block? false })

(def state "Various knobs for the gui. Better keep them together."
 (atom brand-new))
      
(definline knob! [k nv]
`(swap! state assoc ~k ~nv)) 

(defmacro with-block [& blocking-jobs]
`(try (knob! :block? true)  ~@blocking-jobs
  (catch Exception e# (.getMessage e#)) 
    (finally (knob! :block? false))))    

(def curr-game (promise))

(defn refresh [& {:as knobs}]
  (doseq [[k v] knobs]
    (knob! k v)))
 
(defmacro reset-knobs! []
`(reset! knobs brand-new)) 

(defn str-buffer [^StringBuilder buffer]
  (fn [& args]
    (when-let [arg (first args)]
      (.append buffer arg)
      (recur (rest args)))))            


(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action :handler (fn [e] 
                                    #_(when-let [f (choo/choose-file :filters [["text" ["txt"]]])]  
                                    (reset! core/board-history (ut/string->data f @curr-game) #_(ut/deserialize! f)) ;;use java serialisation for now
                                    (ssw/repaint! canvas)))
                        :name "Open file" 
                        :tip  "Load text from a regular .txt file." 
                        :key  "menu N") 
                        
      a-newpdf (ssw/action :handler (fn [e] 
                                    #_(when-let [f (choo/choose-file :filters [["PDF" ["pdf"]]])]  
                                    (reset! core/board-history (ut/string->data f @curr-game) #_(ut/deserialize! f)) ;;use java serialisation for now
                                    (ssw/repaint! canvas)))
                        :name "Open PDF" 
                        :tip  "Load text from a .pdf file." 
                        :key  "menu P")
                        
       a-newurl (ssw/action :handler (fn [e] 
                                    #_(when-let [f (choo/choose-file :filters [["PDF" ["pdf"]]])]  
                                    (reset! core/board-history (ut/string->data f @curr-game) #_(ut/deserialize! f)) ;;use java serialisation for now
                                    (ssw/repaint! canvas)))
                        :name "Open URL" 
                        :tip  "Load text from a remote file." 
                        :key  "menu U")                                        
                                                  
      a-save (ssw/action :handler (fn [e] (choo/choose-file :type :save
                                                            :filters [["text" ["txt"]]] ;;use java serialisation for now
                                                            :success-fn (fn [_ f] #_(ut/data->string @core/board-history f) #_(ut/serialize! @core/board-history f))))
      
                        :name "Save as ..." 
                        :tip "Write the contents of the result pane to a txt file." 
                        :key "menu S")
      a-quit (ssw/action :handler (fn [e] #_(let [repl-server-var (find-var 'Clondie24.lib.gui/Clondie24-nREPL)]
           			           (if (bound? repl-server-var)
            				    (do (sre/stop (var-get repl-server-var)) (System/exit 0))
            				    (System/exit 0))))
                         :name "Quit" 
                         :tip  "Close arena" 
                         :key  "menu Q")                  
      a-pref (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                         :name "Preferences" 
                         :tip  "Show options" 
                         :key  "menu O")
      a-details (ssw/action :handler (fn [e] (ssw/alert "Not implemented!")) 
                            :name "Details" 
                            :tip  "Show info abou your PC."
                            :key  "menu I")
      lo-repl (ssw/action   :handler (fn [e] #_(show-repl!)) 
                            :name "REPL" 
                            :tip  "Show a swing-based local REPL") 
      re-repl (ssw/action   :handler (fn [e] #_(sre/defserver Clondie24-nREPL  8989 false) 
                                      #_(ssw/alert (str "nREPL server is up and running! Make a note of the following:\nIP address = " (ut/external-ip) "\nPort = 8989\nName = Clondie24-nREPL")) )    
                            :name "nREPL server" 
                            :tip  "Start nREPL server that accepts remote clients.")
      a-bout    (ssw/action :handler (fn [e] (ssw/alert "hotel-NLP v0.2.3 (jimpil)")) 
                           :name "About" 
                           :tip  "Version & author" 
                           :key  "menu A")]   
(ssw/menubar :items 
   [(ssw/menu :text "File"    :items [a-new a-newpdf a-newurl a-save  a-quit])
    (ssw/menu :text "Options" :items [a-pref])
    (ssw/menu :text "Tools"   :items [lo-repl re-repl])
    (ssw/menu :text "Help"    :items [a-details a-bout])]) ))

(def canvas "The paintable canvas - our board"
 (ssw/canvas
    :paint #()
    :id :canvas
    :listen [:mouse-clicked #() #_(fn [^MouseEvent e] #_(when (and  (not (:block? @knobs)) 
                                                            (realized? curr-game)) 
                                                      (canva-react @curr-game e)))]
    ;:background "#222222"; no need for background anymore
    ))
    
(def input-pane 
(ssw/text :text "Paste your input text here ..." 
          :multi-line? true))
  
(def result-pane 
  (ssw/styled-text :wrap-lines? true))       
    
(defn GUI "Constructs and returns the entire frame." []
 (ssw/frame
    :title "hotel-NLP"
    :size  [1000 :by 650]
    :resizable? true
    :on-close :dispose
    :menubar  (make-menubar)                   
    :content  (ssw/border-panel
               :border 10
               :hgap 10
               :vgap 10
               :north (let [bg (ssw/button-group)]
                       (ssw/flow-panel :items [(ssw/radio :id :a :text "openNLP"     :group bg)
                                               (ssw/radio :id :b :text "stanfordNLP" :group bg)
                                               (ssw/radio :id :b :text "GATE" :group bg)
                                               (ssw/radio :id :b :text "regex" :selected? true :group bg)
                                               (ssw/radio :id :b :text "custom" :group bg)]
                                       :hgap 20 ))  
               :east  (let [scr  (ssw/scrollable result-pane) ]
                   (.setBackground (.getViewport scr) java.awt.Color/white)
                   (.setPreferredSize scr (java.awt.Dimension. 430 530)) scr) 
               :center (ssw/vertical-panel :items 
                       [(ssw/button :text "Segment"  :listen [:action (fn [e] (ssw/text! result-pane (s/join "\n" (seq (.split (ssw/text input-pane) "\\."))))
                                                                        #_(when-not (:block? @knobs)      
                                                                           (do (refresh :highlighting? false 
                                                                                        :hint nil 
                                                                                        :whose-turn (opposite-color-of (:whose-turn @knobs)))        
                                                                               (undo!)
                                                                               (ssw/config! status-label :text (str (:whose-turn @knobs) " moves next..."))
                                                                               (ssw/repaint! canvas))))])   [:fill-v 15]
                        (ssw/button :text "Tokenise" 
                                    :listen [:action 
                                             (fn [e] #_(when-not (:block? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-v 15]
                                                               
                        (ssw/button :text "POS-tag" 
                                    :listen [:action 
                                             (fn [e] #_(when-not (:block? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-v 15]                                       
                        (ssw/button :text "NER" 
                                    :listen [:action (fn [e] 
                                                       #_(when-not (:block? @knobs) 
                                                           (do (refresh :highlighting? true 
                                                                        :hint nil) 
                                                                (ssw/repaint! canvas))))])  [:fill-v 15]
                        (ssw/button :text "Chunk" 
                                    :listen [:action (fn [e] #_(when-not (:block? @knobs)
                                                      (knob! :highlighting? false) 
                                                        (with-busy-cursor canvas 
                                                           :hint (hint (:pruning? @knobs)))))])  [:fill-v 15]
                       (ssw/button   :text "Parse" 
                                     :listen [:action (fn [e] 
                                                       #_(when-not (:block? @knobs)
                                                        (knob! :pruning? (not (:pruning? @knobs)))))])  [:fill-v 15]
                       (ssw/button   :text "Categorise" 
                                     :listen [:action (fn [e] 
                                                       #_(when-not (:block? @knobs)
                                                        (knob! :pruning? (not (:pruning? @knobs)))))])   [:fill-v 15]  ]) 
               :west 
                 (let [scr  (ssw/scrollable input-pane) ]
                   (.setBackground (.getViewport scr) java.awt.Color/white)
                   (.setPreferredSize scr (java.awt.Dimension. 430 530)) scr)  
               :south   (ssx/busy-label :busy? (:block? @state))   )))    
                         
                         
(defn set-laf! "Set look and feel of the ui, provided its name as a string."  
[laf-name]
(when-let [lf1 (some #(when (= laf-name (.getName %)) %) (UIManager/getInstalledLookAndFeels))]
  (UIManager/setLookAndFeel (.getClassName lf1))))                         
                         
(defn show-GUI! "Everything starts from here." [m]
; (reset-knobs!)      ;start from scratch 
 ;(set-laf! "Nimbus") ;try to look nice
 (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  ;(deliver state game-map) ;firstly make the gui aware of what game we want it to display
   (ssw/invoke-later 
     (ssw/show! (GUI) )))                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
