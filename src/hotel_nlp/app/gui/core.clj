(ns hotel_nlp.app.gui.core
  (:require [clojure.string :as s] 
            [clojure.inspector :refer [inspect]]
            [seesaw.core :as ssw] 
            [seesaw.chooser :as choo]
            [seesaw.swingx :as ssx] 
            [seesaw.icon :refer [icon]]
            [hotel_nlp.helper :as ut])
   (:import [javax.swing UIManager])
)

;;try to look like a native app            
(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName)) 
           
(def brand-new {:curr-text " "
                :result " "
                :busy? false })

(def state "Various knobs for the gui. Better keep them together."
 (atom brand-new))
      
(definline knob! [k nv]
`(swap! state assoc ~k ~nv)) 

(defmacro with-block [& blocking-jobs]
`(try (knob! :busy? true)  ~@blocking-jobs
  (catch Exception e# (.getMessage e#)) 
    (finally (knob! :busy? false))))

(defn refresh [& {:as knobs}]
  (doseq [[k v] knobs]
    (knob! k v)))
 
(defmacro reset-knobs! []
`(reset! knobs brand-new))

(def input-pane "The input text pane"
(ssw/text :text "Paste your input text here ..." 
          :multi-line? true))
  
(def result-pane "The output text pane"
  (ssw/styled-text :wrap-lines? false :styles '([:font "serif"]))) 

(def blabel
  (ssx/busy-label :text "Status" :busy? false))
  
(defmacro with-busy [& code]
 `(try (ssw/config! blabel :busy? true) ~@code
   (finally (ssw/config! blabel :busy? false))))            

(declare GUI)

(defn make-menubar 
"Constructs and returns the entire menu-bar." []
(let [a-new (ssw/action :handler (fn [_] 
                                    (when-let [f (choo/choose-file GUI :filters [["text" ["txt"]]])]  
                                       (ssw/text! input-pane (slurp f))))
                        :name "Open file" 
                        :icon (icon (clojure.java.io/resource "img/File.png"))
                        :tip  "Load text from a regular .txt file" 
                        :key  "menu N") 
                        
      a-newpdf (ssw/action :handler (fn [_] 
                                     (when-let [f (choo/choose-file GUI :filters [["PDF" ["pdf"]]])]
                                      (with-busy (ssw/text! input-pane (ut/pdf->string f)))))
                        :name "Open PDF" 
                        :icon (icon (clojure.java.io/resource "img/File_Pdf.png"))
                        :tip  "Load text from a .pdf file" 
                        :key  "menu P")
                        
       a-newurl (ssw/action :handler (fn [_] 
                                       (when-let [^String user-input (ssw/input GUI "Provide a valid remote location (URL):" :title "Load text from URL" :type :question)]
                                        (when (seq user-input)
                                          (->> user-input 
                                           clojure.java.io/as-url 
                                           slurp 
                                           (ssw/text! input-pane)))))  
                        :name "Open URL" 
                        :icon  (icon (clojure.java.io/resource "img/Globe.png"))
                        :tip  "Load text from a remote file" 
                        :key  "menu U")                                        
                                                  
      a-save (ssw/action :handler (fn [e] (choo/choose-file GUI
                                                            :type :save
                                                            ;:filters [["text" ["txt"]]]
                                                            :success-fn (fn [_ f] (spit f (ssw/text result-pane)))))
      
                        :name "Save as ..."
                        :icon   (icon (clojure.java.io/resource "img/flash_disk.png"))
                        :tip "Write the contents of the result pane to a .txt file" 
                        :key "menu S")
      a-quit (ssw/action :handler (fn [e] (System/exit 0))
                         :name "Quit" 
                         :icon  (icon (clojure.java.io/resource "img/deletered.png"))
                         :tip  "Quit hotel-nlp GUI" 
                         :key  "menu Q")                  
      a-pref (ssw/action :handler (fn [e] (ssw/alert GUI "Not implemented!")) 
                         :name "Preferences" 
                         :tip  "Show options" 
                         :key  "menu O")
      a-details (ssw/action :handler (fn [e] (clojure.inspector/inspect  (into {} (System/getProperties)))) 
                            :name "Details"
                            :icon (icon (clojure.java.io/resource "img/information2.png"))   
                            :tip  "Info about your PC and the JVM."
                            :key  "menu I")
      lo-repl (ssw/action   :handler (fn [e] #_(show-repl!)) 
                            :name "REPL" 
                            :tip  "Show a swing-based local REPL") 
      re-repl (ssw/action   :handler (fn [e] #_(sre/defserver Clondie24-nREPL  8989 false) 
                                      #_(ssw/alert (str "nREPL server is up and running! Make a note of the following:\nIP address = " (ut/external-ip) "\nPort = 8989\nName = Clondie24-nREPL")) )    
                            :name "nREPL server" 
                            :tip  "Start nREPL server that accepts remote clients.")
      a-bout    (ssw/action :handler (fn [_] (ssw/alert GUI "hotel-NLP v0.2.3 (by jimpil)")) 
                           :name "About" 
                           :icon (icon (clojure.java.io/resource "img/sleep_hotel.png"))
                           :tip  "Version & author" 
                           :key  "menu A")]   
(ssw/menubar :items 
   [(ssw/menu :text "File"    :items [a-new a-newpdf a-newurl a-save  a-quit])
    (ssw/menu :text "Options" :items [a-pref])
    (ssw/menu :text "Tools"   :items [lo-repl re-repl])
    (ssw/menu :text "Help"    :items [a-details a-bout])]) ))

#_(def canvas "The paintable canvas - our board"
 (ssw/canvas
    :paint #()
    :id :canvas
    :listen [:mouse-clicked #() #_(fn [^MouseEvent e] #_(when (and  (not (:busy? @knobs)) 
                                                            (realized? curr-game)) 
                                                      (canva-react @curr-game e)))]
    ;:background "#222222"; no need for background anymore
    ))
    
 
  
(defn move-text! "Moves the text contents of tp1 to tp2." 
([tp1 tp2 clear?]
  (ssw/text! tp2 (ssw/text tp1)) 
  (when clear?
    (ssw/text! tp1 "")))
([tp1 tp2]
  (move-text! tp1 tp2 false)))            
    
(def GUI "the entire frame." 
 (ssw/frame
    :title "hotel-NLP"
    :icon "img/hotel-icon.png"
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
                       [(ssw/button :text "SEGMENT"  :listen [:action (fn [e] 
                                                                        (when-not (:busy? @state)      
                                                                           (ssw/text! result-pane (s/join "\n" (seq (.split (ssw/text input-pane) "\\."))))))])   [:fill-v 15]
                        (ssw/button :text "TOKENISE" 
                                    :listen [:action 
                                             (fn [e] #_(when-not (:busy? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-v 15]
                        (ssw/button :text "STEM" 
                                    :listen [:action 
                                             (fn [e] #_(when-not (:busy? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-v 15]                                       
                                                                                                                             
                        (ssw/button :text "POS" 
                                    :listen [:action 
                                             (fn [e] #_(when-not (:busy? @knobs)
                                                      (do (refresh :highlighting? false 
                                                                   :hint nil) 
                                                               (clear!) (ssw/repaint! canvas))))]) [:fill-v 15]                                       
                        (ssw/button :text "NER" 
                                    :listen [:action (fn [e] 
                                                       #_(when-not (:busy? @knobs) 
                                                           (do (refresh :highlighting? true 
                                                                        :hint nil) 
                                                                (ssw/repaint! canvas))))])  [:fill-v 15]
                         (ssw/button :text "COREF" 
                                    :listen [:action (fn [e] 
                                                       #_(when-not (:busy? @knobs) 
                                                           (do (refresh :highlighting? true 
                                                                        :hint nil) 
                                                                (ssw/repaint! canvas))))])  [:fill-v 15]                                        
                                                                
                        (ssw/button :text "CHUNK" 
                                    :listen [:action (fn [e] #_(when-not (:busy? @knobs)
                                                      (knob! :highlighting? false) 
                                                        (with-busy-cursor canvas 
                                                           :hint (hint (:pruning? @knobs)))))])  [:fill-v 15]
                       (ssw/button   :text "PARSE" 
                                     :listen [:action (fn [e] 
                                                       #_(when-not (:busy? @knobs)
                                                        (knob! :pruning? (not (:pruning? @knobs)))))])  [:fill-v 15]
                       (ssw/button   :text "DOCCAT" 
                                     :listen [:action (fn [e] 
                                                       #_(when-not (:busy? @knobs)
                                                        (knob! :pruning? (not (:pruning? @knobs)))))])   [:fill-v 15] 
                                                        
                      (ssw/button   :text "<<MOVE<" 
                                    :listen [:action (fn [e] 
                                                       (when-not (:busy? @state)
                                                        (move-text! result-pane input-pane true)))])   [:fill-v 15]  ]) 
               :west 
                 (let [scr  (ssw/scrollable input-pane) ]
                   (.setBackground (.getViewport scr) java.awt.Color/white)
                   (.setPreferredSize scr (java.awt.Dimension. 430 530)) scr)  
               :south   (ssx/busy-label :text "Status" :busy? (:busy? @state))   )))    
                         
                         
(defn set-laf! "Set look and feel of the ui, provided its name as a string."  
[laf-name]
(when-let [lf1 (some #(when (= laf-name (.getName %)) %) (UIManager/getInstalledLookAndFeels))]
  (UIManager/setLookAndFeel (.getClassName lf1))))                         
                         
(defn show-GUI! "Everything starts from here." []
; (reset-knobs!)      ;start from scratch 
 ;(set-laf! "Nimbus") ;try to look nice
 ;(UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  ;(deliver state game-map) ;firstly make the gui aware of what game we want it to display
   (ssw/invoke-later 
     (ssw/show! GUI ))) 
                            
                         
(defonce paragraph
"More than nine million people have been affected in the Philippines. Many are now struggling to survive without food, shelter or clean drinking water. A picture is slowly emerging of the full damage wrought by the storm.")                         
                         
                         
                         
                         
                         
                         
                         
