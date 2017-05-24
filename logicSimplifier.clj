(ns csc344-proj2.core)


 (defn deep-substitute [l m]
    (map #( cond
            (seq? %) (deep-substitute % m)
            :default (m % %)) l)
 )
 
   (defn simpl [e1]
    (cond
      (= e1 '(or true)) true
      (= e1 '(or false)) false
      (= e1 '(or true false)) true
      (= e1 '(or false true)) true
      (= e1 '(or true true)) true
      (= e1 '(or false false)) false
      (= e1(and true)) true
      (= e1 '(and false)) false
      (= e1 '(and false false)) false
      (= e1 '(and false true)) false
      (= e1 '(and true false)) false
      (= e1 '(and true true)) true
      (= e1 '(and true)) true
      )
    )
   
   
  (defn simplify [a]
   (println "simplifying this: " + a)
   (cond
 ;not ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    (= (nth a 0) 'not) (cond 
                         (= (nth a 1) true) false
                         (= (nth a 1) false) true
                         (and (and (not (= (nth a 1) false)) (not (= (nth a 1) false))) (not (seq? (nth a 1)))) (list 'not (nth a 1))
                                                
  (seq? (nth a 1)) (cond
                                            
                                            (and (not(= (simplify (nth a 1)) true)) (not(= (simplify (nth a 1)) false))) (list 'not (simplify (nth a 1))) 
                                            (= (simplify (nth a 1)) true) false
                                            (= (simplify (nth a 1)) false) true
                                            )                                                   
                         )
    
               
    ;The second and third arguments are either true or false   
    (and (or (= (nth a 1) false) (= (nth a 1) true)) (or (= (nth a 2) false) (= (nth a 2) true)) ) (simpl a)
      
   
 ;and ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
    (= (nth a 0) 'and)  (cond 
                         (= (nth a 2) false) false
                         (= (nth a 1) false) false
                        
                         
; both arguments are variables

                         (and (and (and (not (seq? (nth a 1))) (not (= (nth a 1) true))) (not (= (nth a 1) false))) (and (and (not (seq? (nth a 2)))) (not (= (nth a 2) true)) (not (= (nth a 2) false)))) (list 'and (nth a 1) (nth a 2))
                                                                      
 ;the first arguement is not a list, false, or true                     
                         (and (and (not (seq? (nth a 1))) (not (= (nth a 1) true))) (not (= (nth a 1) false))) (cond 
                                                                                                                 (= (nth a 2) false) false
                                                                                                                 (= (nth a 2) true) (nth a 1) 
                                                                                                                 (seq? (nth a 2)) (cond
                                                                                                                                    (= (simplify (nth a 2)) false) false
                                                                                                                                    (= (simplify (nth a 2)) true) (nth a 1)
                                                                                                                                     :default (list 'and (nth a 1) (simplify (nth a 2)))
                                                                                                                                    )
                                                                                                                 )
    
  ;the second arguement is not a list, false, or true                     
                         (and (and (not (seq? (nth a 2))) (not (= (nth a 2) true))) (not (= (nth a 2) false))) (cond 
                                                                                                                 (= (nth a 1) false) false
                                                                                                                 (= (nth a 1) true) (nth a 2) 
                                                                                                                 (seq? (nth a 1)) (cond
                                                                                                                                    (= (simplify (nth a 1)) false) true
                                                                                                                                    (= (simplify (nth a 1)) true) (nth a 2)
                                                                                                                                    :default (list 'and (simplify (nth a 1)) (nth a 2))
                                                                                                                                    )
                                                                                                                 )    
                         (and (seq? (nth a 1)) (seq? (nth a 2))) (cond  
                                                                   (= (simplify (nth a 1)) false) false
                                                                   (= (simplify (nth a 2)) false) false
                                                                   (and (= (simplify (nth a 1)) true) (= (simplify (nth a 2)) true)) true
                                                                   (= (simplify (nth a 1)) true) (simplify (nth a 2))
                                                                   (= (simplify (nth a 2)) true) (simplify (nth a 1)) 
                                                                   :default a
                                                                      )
                                                                      
                         (seq? (nth a 1))  (cond 
                                            (= (simplify (nth a 1)) false) false
                                            (not(= (simplify (nth a 1)) true)) (simplify (nth a 1))
                                            (= (and (= (simplify (nth a 1)) true) (nth a 2)) true) true
                                            (= (and (= (simplify (nth a 1)) true) (nth a 2)) false) false
                                            )
                         (seq? (nth a 2))  (cond 
                                           (= (simplify (nth a 2)) false) false
                                           (not(= (simplify (nth a 2)) true)) (simplify (nth a 2))
                                           (= (and (= (simplify (nth a 2)) true) (nth a 1)) true) true
                                           (= (and (= (simplify (nth a 2)) true) (nth a 1)) false) true
                                           )
    )
    
;or;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                         
    (= (nth a 0) 'or) (cond  
                        
                        (= (nth a 2) true) true
                        (= (nth a 1) true) true
                        
; both arguments are variables

                         (and (and (and (not (seq? (nth a 1))) (not (= (nth a 1) true))) (not (= (nth a 1) false)))  (and (and (not (seq? (nth a 2))) (not (= (nth a 2) true))) (not (= (nth a 2) false)))) (list 'or (nth a 1) (nth a 2))
                                      
                          
 ;the first arguement is not a list, false, or true                     
                          (and (and (not (seq? (nth a 1))) (not (= (nth a 1) true))) (not (= (nth a 1) false))) (cond 
                                                                                                                  (= (nth a 2) true) true
                                                                                                                  (= (nth a 2) false) (nth a 1) 
                                                                                                                  (seq? (nth a 2)) (cond
                                                                                                                                     (= (simplify (nth a 2)) true) true
                                                                                                                                     (= (simplify (nth a 2)) false) (nth a 1)
                                                                                                                                      :default (list 'or (nth a 1) (simplify (nth a 2)))
                                                                                                                                     )
                                                                                                                  )                         
;the second arguement is not a list, false, or true                     
                          (and (and (not (seq? (nth a 2))) (not (= (nth a 2) true))) (not (= (nth a 2) false))) (cond 
                                                                                                                  (= (nth a 1) true) true
                                                                                                                  (= (nth a 1) false) (nth a 2) 
                                                                                                                  (seq? (nth a 1)) (cond
                                                                                                                                     (= (simplify (nth a 1)) true) true
                                                                                                                                     (= (simplify (nth a 1)) false) (nth a 2)
                                                                                                                                     :default (list 'or (simplify (nth a 1)) (nth a 2))
                                                                                                                                     )
                                                                                                                  )                              
                          
                          (and (seq? (nth a 1)) (seq? (nth a 2))) (cond                                                                   
                                                                  (= (simplify (nth a 1)) true) true
                                                                  (= (simplify (nth a 2)) true) true
                                                                  (and (= (simplify (nth a 1)) false) (= (simplify (nth a 2)) false)) false
                                                                  (= (simplify (nth a 1)) false) (simplify (nth a 2))
                                                                  (= (simplify (nth a 2)) false) (simplify (nth a 1))  
                                                                  :default a
                                                                  )
                          
                         (seq? (nth a 1)) (cond 
                                            (= (simplify (nth a 1)) true) true
                                            (not(= (simplify (nth a 1)) false)) (simplify (nth a 1))
                                            (= (and (= (simplify (nth a 1)) false) (nth a 2)) true) true
                                            (= (and (= (simplify (nth a 1)) false) (nth a 2)) false) false
                                            )
                         (seq? (nth a 2))  (cond 
                                           (= (simplify (nth a 2)) true) true
                                           (not(= (simplify (nth a 2)) false)) (simplify (nth a 2))
                                           (= (and (= (simplify (nth a 2)) false) (nth a 1)) true) true
                                           (= (and (= (simplify (nth a 2)) false) (nth a 1)) false) false
                                           )
                         
                          :default false                         
                          )
    )
   )
  
  (defn andBuild [x y]
    (cond 
      (and (seq? x) (seq? y)) (list 'and (build x) (build y))
      (seq? x) (list 'and (build x) y)
      (seq? y) (list 'and (build y) x)
      true (list 'and x y)
      )
    )
  
   (defn orBuild [x y]
    (cond 
      (and (seq? x) (seq? y)) (list 'or (build x) (build y))
      (seq? x) (list 'or (build x) y)
      (seq? y) (list 'or (build y) x)
      true (list 'or x y)
      )
    )
  
  (defn andExp [a]
  (reduce andBuild a)
     
  )
  
  (defn orExp [a]
      (reduce orBuild a)
    )

  (defn notExp [a]
    (cond
      (seq? (nth a 0)) (list 'not (build (nth a 0)))
      true (list 'not (nth a 0))
      )
   )
  
  (defn build [a]    
    (cond 
     
      (= (nth a 0) 'and) (andExp (rest a))                      
      (= (nth a 0) 'or) (orExp (rest a))       
      (= (nth a 0) 'not) (notExp (rest a))       
                             
    )
  )
   
  (defn bind-values [e bindings]
      (deep-substitute e bindings)
  )
       
  
  (defn evalexp [exp bindings] (simplify (build (bind-values exp bindings))))
  
  
 
;Expression examples
 
  ;  (def p9 '(and xsdf (and true true)))
   (def p5 '(or false (and false (not x)) false false false))
    (def p4 '(and x y (or y z) (not (or z false))))
    (def p7 '(and x x (or x y (not y)) true (not (and true dog true true cat))))
(def p6 '(and x x x x x x ))


;(evalexp p5 '{x cat})
 ;(evalexp p4 '{x true, y true})
(evalexp p7 '{x true, y true})
;(simplify '(not (and (and true cat) dog)))

