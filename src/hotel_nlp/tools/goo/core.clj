(ns hotel_nlp.tools.goo.core
(:import  (java.util Properties) 
          (java.net URL URLEncoder)
           javax.mail.internet.MimeMessage
           (javax.mail.internet MimeMessage InternetAddress)
           (javax.mail Session Transport Authenticator PasswordAuthentication Message$RecipientType)))

(def google-search-url "http://www.google.com/search?q=")
(def ^:dynamic *user-agent* "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.22 (KHTML, like Gecko) Chrome/25.0.1364.172")

(defn get-response [^URL url]
(let [conn (doto (.openConnection url)
                 (.setRequestProperty "User-Agent" *user-agent*))]
 (slurp (.getInputStream conn))))

(defn goo-search [^String query]
 (let [url (URL. (str google-search-url (URLEncoder/encode query)))]
  (get-response url)))

;usage:
;(spit "response.html" (goo-search "clojure reducers"))
 
(defn mail "Send email programmatically." 
[{:keys [from to subject text password host ssl? port]}]
  (let [auth (proxy [Authenticator] []
               (getPasswordAuthentication []
                 (PasswordAuthentication. from password)))
        props (doto (Properties.)
                (.putAll {"mail.smtp.user" from
                          "mail.smtp.host" host
                          "mail.smtp.starttls.enable" (if ssl? "true" "false")
                          "mail.smtp.auth" "true"
                          "mail.smtp.port" (str port)}))
        session (Session/getInstance props auth)
        msg (doto (MimeMessage. session)
              (.setText text)
              (.setSubject subject)
              (.setFrom (InternetAddress. from)))]
    (doseq [addr to]
      (.addRecipient msg Message$RecipientType/TO (InternetAddress. addr)))
    (Transport/send msg)))
    
(defn gmail "Send email with GMail." 
 [opt-map] 
 (mail 
   (merge opt-map {:host "smtp.gmail.com" 
                   :ssl? true 
                   :port 584})))
                          
;example  
#_(gmail {:from "jimpil1985@gmail.com"
          :to ["piliourd@cs.man.ac.uk"]
          :subject "TEST-JAVAX.MAIL"
          :text "clojure says HI!"
          :password "***"})     
  
