(ns hotel-nlp.tools_test
  (:use clojure.test
        hotel_nlp.tools.normalisation.core))
        
(deftest norm-test
(testing "String normalisation..."
    (is (= "eat" (normalise "eating" transform-by-porter)))
    (is (= "kiss" (normalise "kissing" transform-by-porter)))
    (is (= "danc" (normalise "dancing" transform-by-porter)))
    (is (= ["eat" "kiss" "danc"] (normalise ["eating" "kissing" "dancing"] transform-by-porter "english")))
    (is (= ["eat" "kiss" "danc"] (normalise ["eating" "kissing" "dancing"] transform-by-porter)))
    (is (= #{"eat" "kiss" "danc"} (normalise #{"eating" "kissing" "dancing"} transform-by-porter "english")))
    (is (= '("eat" "kiss" "danc") (normalise '("eating" "kissing" "dancing") transform-by-porter "english"))))
(testing "Number normalisation..."    
  (is (= {[-1 -3/4 -1/2 -1/4 0N 1/4 1/2 3/4 1] [-3/13 -11/13 -7/13 -1 1]} 
         (normalise {(java.util.ArrayList. (range -4 5)) [-3 -7 -5 -8 5]} transform-in-range))) 
  (is (= {[-5 -15/4 -5/2 -5/4 0N 5/4 5/2 15/4 5] [-15/13 -55/13 -35/13 -5 5]} 
         (normalise {(java.util.ArrayList. (range -4 5)) [-3 -7 -5 -8 5]} transform-in-range [5 -5]))) 
  (is (= '(1/4 1/5 1/6 1/7 1/8 1/9 1/10 1/11 1/12 1/13 1/14) 
         (normalise (range 4 15) transform-reciprocal [1 -1])))                  
  (is (= [-2 -28/15 -26/15 -8/5 -22/15 -4/3 -6/5 -16/15 -14/15 -4/5 -2/3 -8/15 -2/5 -4/15 -2/15 0N 2/15 4/15 2/5 8/15 2/3 4/5 14/15 16/15 6/5 4/3 22/15 8/5 26/15 28/15 2] 
         (normalise (vec (range -15 16)) transform-in-range [2 -2])))
  (is (= [0 1/100 1/50 3/100 1/25 1/20 3/50 7/100 2/25 9/100 1/10 11/100 3/25 13/100 7/50 3/20 4/25 17/100 9/50 19/100 1/5 21/100 11/50 23/100 6/25] 
         (normalise (vec (range 25)) transform-by-value)))        
            
)  
  
  
  
    
)        
