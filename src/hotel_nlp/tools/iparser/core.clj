(ns hotel_nlp.tools.iparser.core
   (:require [clojure.pprint :refer [pprint]]
             [clojure.walk :refer [walk]]
             [instaparse.core :as insta]
             [hotel_nlp.helper    :as help]
             [net.cgrand.enlive-html  :as enl]) )
;-----------------------------------------------------------------------------------------------------------------------------------             
;----------------------------<EXPERIMENTAL CODE>------------------------------------------------------------------------------------  

;;from the instaparse readme
(def words-and-numbers
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = word | number
     whitespace = #'\\s+'
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'")) 
     
(def arithmetic
  (insta/parser
    "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))
     
(def arithmetic-translation 
 {:add + 
  :sub - 
  :mul * 
  :div /
  :number clojure.edn/read-string 
  :expr identity} )
  
(defmacro infix-arithmetic [& body]
`(let [no-spaces# (apply str (remove #(re-matches #"\s+" (str %)) (str '~body)))]
   (insta/transform arithmetic-translation (arithmetic no-spaces#))) ) 
   
   
   
;;let's see...
(def toy "a toy parser just to get warm."
(insta/parser
  "S = NP SPACE VP
   SPACE = #'\\s+'
   VP = VB SPACE NP
   NP = DET? SPACE? ADJ? SPACE? N 
   N = 'boy' | 'girl'
   VB = 'sees' | 'likes'
   ADJ = 'big' | 'small'
   DET = 'a' | 'the'" ))
   
(def bib "a bibtex parser"
(insta/parser
  " <bibfile> ::= whitespace {bibentry whitespace}
    <whitespace> ::= <#'[\\s$]*'>
    <word> ::= #'[a-zA-Z0-9-_]+'
    type ::= <'@'> word
    tag ::= #'[a-zA-Z0-9-_/:]+'
    bibentry ::= type whitespace <'{'> whitespace tag whitespace <','> whitespace bibcontent whitespace <'}'>
    <bibcontent> ::=  {key-value whitespace <','> whitespace} (key-value whitespace)?
    key-value ::= word whitespace <'='> whitespace value
    <value> ::= word | quoted-value | bracketed-value
    quoted-value ::= <'\"'> quoted-content* <'\"'>
    <quoted-content> ::= #'(\\\\\"|[^\"])*'
    <bracketed-value> ::= <'{'> whitespace  bracketed-content?  whitespace <'}'>
    bracketed-content ::= no-brackets [{'{' bracketed-content? '}' no-brackets}]
    no-brackets ::= #'[^{}]*'     ") )  
   
   
(def parsePK
 (insta/parser
  "S  =  PHRASE+  SPACE END
   PHRASE = (DDIPK / DDI / COADMIN / ENCLOSED ) | TOKEN (SPACE TOKEN PUNCT?)*
   DDIPK = OBJECT? SPACE PK SPACE (BE | TO)? SPACE OBJECT? SPACE EFF?  
   DDI =  PRECIPITANT SPACE MECH SPACE OBJECT SPACE PK | PRECIPITANT SPACE MECH SPACE 'in' SPACE
   EFF =  MAYBE? SPACE BE? SPACE (SIGN | FOLD)? SPACE MECH SPACE (ADV | FOLD)? SPACE (PRECIPITANT | PK)? SPACE  
   TOKEN = ((OBJECT / PRECIPITANT / DRUG) | DOSE | ROUTE | NUM  |  PK | PERCENTAGE | XFOLD | XFACTOR | CYP | ABBR |  MECH | SIGN | TO | ENCLOSED | COMMA) / WORD 
   <WORD> = #'\\w+' 
   <PUNCT> = #'\\p{Punct}'
   INC-DEC = ('increase' | 'decrease') / #'\\b+[a-z]+e\\b+'
   FOLD =  (NUM SPACE '-' SPACE ('and' | ',')? SPACE)+ SPACE 'fold'
   COADMIN = #'(?i)when' SPACE #'(?i)co\\-?administered' SPACE PRECIPITANT SPACE | 
             #'(?i)co\\-?administration' SPACE TO SPACE DRUG SPACE 'with' SPACE DRUG  |
             #'(?i)co\\-?administration' SPACE TO SPACE 'either'? SPACE (DRUG 'or'?)+ 
   XFOLD = FOLD SPACE EFF? 
   XFACTOR = BY SPACE 'factors' SPACE (TO | #'[a-z]+ing'? SPACE 'between') SPACE (NUM ('and' SPACE NUM)*) 
   ROUTE = #'(?i)oral|intravenous'
   BY = 'by'
   UNIT = 'mg' | 'g' 
   DOSE = (NUM SPACE '-'? SPACE NUM?) SPACE UNIT SPACE INTERVAL?  
   ABBR = #'([a-z]\\.)+'
   INTERVAL = #'[a-z]+ce'? SPACE ADV | NUM SPACE 'times per' TIME | '/' TIME | ABBR
   TIME =  'hour' | 'day' | 'week'
   PERCENTAGE = NUM SPACE ('%' | #'per(\\s|\\-)?cent') 
   ENCLOSED = PAREN | SQBR
   <PAREN> = '(' PHRASE ')'
   <SQBR> =   #'\\[.*\\]'
   <INT> =  #'\\p{Digit}+'
    NUM =   #'\\p{Digit}*(\\.|\\,)?\\p{Digit}+'    (*  INT* ('.' | ',')? INT  *)
    CYP =  #'\\b+CYP[A-Z0-9]*\\b+' 
    ADV =   #'\\b+[a-z]+ly\\b+'
   <SPACE> = <#'\\s*'>
   PRECIPITANT = (BY | 'with' | 'as') SPACE DRUG / DRUG SPACE EFF 
   PARENDOSE = '(' DOSE  ('for' SPACE NUM #'days?')? ')'
   DRUGDOSE = PRECIPITANT SPACE PARENDOSE 
   2DRUG = DRUGDOSE SPACE 'administered'? SPACE 'with' 'a single'? SPACE DRUGDOSE
   OBJECT = TO SPACE DRUG / DRUG SPACE PK SPACE BE / DRUG SPACE 'metabolism'
    DRUG = (ROUTE SPACE)?  
      (#'(?i)\\b+\\w+a[z|st|p]ine?\\b+' | 
       #'(?i)\\b+\\w+[i|u]dine?\\b+'    | 
       #'(?i)\\b+\\w+azo[l|n]e?\\b+'    |
       #'(?i)\\b+\\w+tamine?\\b+'       |
       #'(?i)\\b+\\w+zepam\\b+'         |
       #'(?i)\\b+\\w+zolam\\b+'         |
       #'(?i)\\b+\\w+[y|u]lline?\\b+'   |
       #'(?i)\\b+\\w+artane?\\b+'       |
       #'(?i)\\b+\\w+retine?\\b+'       |
       #'(?i)\\b+\\w+navir\\b+'         |
       #'(?i)\\b+\\w+ocaine\\b+'        |
       #'(?i)\\b+\\w+oxetine\\b+'        |
       #'(?i)\\b+\\w+fenone\\b+'        |   
       #'(?i)didanosine|tenofovir|vaprisol|conivaptan|amlodipine|carbamazepine|S?\\-?warfarin|metoclopramide|acetaminophen|tetracycline|digoxin|levodopa|cyclosporine|ethanol|captopril|furosemide|S?\\-?metoprolol') SPACE 'hydrochloride'?
       (SPACE 'up to'? SPACE  DOSE)?
 
    PK =  MECH? SPACE 'the'? SPACE 'mean'? SPACE (OBJECT | DRUG)?
          #'(?i)(pharmacokinetics|exposure|bioavailability|lower?(\\s|\\-)?clearance|concentration|absorption|elimination|AUC|half\\-life|Cmax|plasma(\\-|\\s)levels?)' SPACE OBJECT?       
    MECH =  MAYBE? SPACE BE?
           ( #'inhibit(?:e(?:s|d)?|ing|ions?|or)?' |
             #'cataly(?:z|s)(?:e(?:s|d)?|ing)'    |
             #'metaboli(?:(z|s)(?:e(?:s|d)?|ing)|sm)' |
             #'correlat(?:e(?:s|d)?|ing|ions?)'  |
             #'induc(?:e(?:s|d)?|ing|tions?|or)' |
             #'stimulat(?:e(?:s|d)?|ing|ions?)' |
             #'activ(?:e(?:s)?|(?:at)(?:e(?:s|d)?|ing|ions?))' |
             #'form(?:(?:s|ed|ing|ations?)?)'   |
             #'suppress(?:e(?:s|d)?|ing|ions?)' |
             #'increas(?:e(?:s|d)?|ing)'  |
             #'decreas(?:e(?:s|d)?|ing)' |
             #'diminish(?:e(?:s|d)?|ing)' |
             #'\\b+[a-z]+e(s|d)\\b+'      )  (SPACE (FOLD | (BY? SPACE ((NUM 'x') | PERCENTAGE) | OBJECT)))?
    SIGN =  ADV | NEG
    MAYBE = 'can' | 'may'
    NEG = 'not' | #'un[a-z]+ed'
    <TO> = 'to' | 'of'
    BE = 'be' | 'is' | 'are' | 'was' | 'were'
  (*  DO = 'does' | 'do' | 'did' *) 
   <COMMA> = ',' 
  (* <OTHER> = 'as' | 'its' | 'by' *)
    END =  '.' "  ;:output-format :enlive
))

(defonce GOLD-PK ;;the gold corpus as a data-structure
 (help/csv->maps "resources/all-consensus-interaction-entities-dumped-05162011.csv" 
   {:file 0 ;;string
    :precipitant 2 ;;drug
    :object  7 ;;drug
    :type 13 ;;pos/neg 
    :modality 12})) ;;qualitative/quantitative

;(comment
;(defn explore 
;([tree res-map])
; (let [ddipk  (enl/select tree [:DDIPK])
;       ddi    (enl/select tree [:DDI])
;       ;major? (or (seq ddipk) (seq ddi))
;       xfold  (enl/select tree [:XFOLD])
;       xfactor (enl/select tree [:XFACTOR])
;       obj   (enl/select tree [:OBJECT :DRUG])
;       preci (enl/select tree [:PRECIPITANT :PRECIPITANT])           
;       pk    (enl/select tree [:PK])
;       mecha  (enl/select tree [:MECH])
;       all-drugs (enl/select tree [:DRUG])] 
;  (-> res-map
;    (assoc :pk ddi
;           :mod ddipk))     
;       
;       )
;       
;(cond-> tree
;  (empty? ddipk) )

;)     





;([tree] 
;(explore tree 
;  {:drugs {:precipitant nil 
;           :objects []} 
;   :ddi nil
;   :ddi-pk nil
;   :pk nil
;   :mod nil
;   })))

#_(pprint   
(parsePK "Exposure to oral didanosine is significantly increased when coadministered with tenofovir disoproxil fumarate [Table 5 and see Clinical Pharmacokinetics (12.3, Tables 9 and 10)]."))      
#_(pprint   
(parsePK "Oral conivaptan hydrochloride 40 mg twice daily resulted in a 2-fold increase in the AUC and half-life of amlodipine [see Warnings and Precautions (5.3)]."))
#_(pprint 
(parsePK "VAPRISOL 30 mg/day resulted in a 3-fold increase in the AUC of simvastatin."))
#_(pprint 
(parsePK "Coadministration of oral conivaptan 10 mg with ketoconazole 200 mg resulted in 4- and 11-fold increases in Cmax and AUC of conivaptan, respectively [see Contraindications (4.2)].")) 
#_(pprint 
(parsePK "Conivaptan bioavailability was unchanged."))
#_(pprint 
(parsePK "Conivaptan bioavailability was decreased by 12%."))

#_(pprint 
(parsePK "Coadministration of oral conivaptan hydrochloride 10 mg with ketoconazole 200 mg resulted in."))

(def sucess "23 representative sentences"
["Exposure to didanosine is increased when coadministered with tenofovir disoproxil fumarate [Table 5 and see Clinical Pharmacokinetics (12.3, Tables 9 and 10)]."
"Carbamazepine can increase alprazolam metabolism and therefore can decrease plasma levels of alprazolam." 
"Absorption of drugs from the stomach may be diminished by metoclopramide (e.g., digoxin), whereas the rate and/or extent of absorption of drugs from the small bowel may be increased (e.g., acetaminophen, tetracycline, levodopa, ethanol, cyclosporine)."
"VAPRISOL 40 mg/day increased the mean AUC values by approximately 2- and 3-fold for 1 mg intravenous or 2 mg oral doses of midazolam, respectively."
"VAPRISOL 30 mg/day resulted in a 3-fold increase in the AUC of simvastatin."
"Oral conivaptan hydrochloride 40 mg twice daily resulted in a 2-fold increase in the AUC and half-life of amlodipine [see Warnings and Precautions (5.3)]."
"Coadministration of a 0.5 mg dose of digoxin, a P-glycoprotein substrate, with oral conivaptan hydrochloride 40 mg twice daily resulted in a 30% reduction in clearance and 79% and 43% increases in digoxin Cmax and AUC values, respectively [see Warnings and Precautions (5.4)]."
"The pharmacokinetics of oral conivaptan (20 - 40 mg/day) were unchanged with coadministration of either captopril 25 mg or furosemide up to 80 mg/day."
"Drugs that inhibit CYP2D6 such as quinidine, fluoxetine, paroxetine, and propafenone are likely to increase metoprolol concentration."
"In healthy subjects with CYP2D6 extensive metabolizer phenotype, coadministration of quinidine 100 mg and immediate-release metoprolol 200 mg tripled the concentration of S-metoprolol and doubled the metoprolol elimination half-life."
"In four patients with cardiovascular disease, coadministration of propafenone 150 mg t.i.d. with immediate-release metoprolol 50 mg t.i.d. resulted in two- to five-fold increases in the steady-state concentration of metoprolol."
"Coadministration of multiple doses of quinidine sulfate, 200 mg t.i.d., and nifedipine, 20 mg t.i.d., increased Cmax and AUC of nifedipine in healthy volunteers by factors of 2.30 and 1.37, respectively."
"The exposure to quinidine was not importantly changed in the presence of nifedipine."
"Pretreatment of healthy volunteers with 30 mg or 90 mg t.i.d. diltiazem p.o. increased the AUC of nifedipine after a single dose of 20 mg nifedipine by factors of 2.2 and 3.1, respectively."
"The corresponding Cmax values of nifedipine increased by factors of 2.0 and 1.7, respectively."
"In a study assessing disposition of sertraline (100 mg) on the second of 8 days of cimetidine administration (800 mg daily), there were significant increases in sertraline mean AUC (50%), C max (24%) and half-life (26%) compared to the placebo group."
"However, in clinical studies, concomitant nifedipine had no effect on irbesartan pharmacokinetics."
"Coadministration of nifedipine resulted in a decrease in AUC and Cmax of doxazosin to 83% and 86% of the values in the absence of nifedipine, respectively."
"In the presence of doxazosin, AUC and Cmax of nifedipine were increased by factors of 1.13 and 1.23, respectively."
"In normotensive subjects receiving single doses of 10 mg or multiple doses of up to 20 mg nifedipine t.i.d. alone or together with cimetidine up to 1000 mg/day, the AUC values of nifedipine in the presence of cimetidine were between 1.52 and 2.01 times those in the absence of cimetidine."
"The Cmax values of nifedipine in the presence of cimetidine were increased by factors ranging between 1.60 and 2.02."
"Concomitant administration of quinupristin/dalfopristin and nifedipine (repeated oral dose) in healthy volunteers increased AUC and Cmax for nifedipine by factors of 1.44 and 1.18, respectively, compared to nifedipine monotherapy."
])


(def fail 
["VAPRISOL (40 mg/day for 4 days) administered with a single 25 mg dose of warfarin, which undergoes major metabolism by CYP2C9 and minor metabolism by CYP3A, increased the mean S-warfarin AUC and S-warfarin Cmax by 14% and 17%, respectively."
])
                   
