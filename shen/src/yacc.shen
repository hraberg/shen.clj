(define yacc 
  [defcc S | CC_Stuff] -> (yacc->shen S CC_Stuff (extract-segvars CC_Stuff)))

(define extract-segvars
  X -> [X]   where (segvar? X)
  [X | Y] -> (union (extract-segvars X) (extract-segvars Y))
  _ -> [])
   
(define yacc->shen
  S CC_Stuff SegVars -> (let Main [define S | (yacc_cases (map (function cc_body) (split_cc_rules CC_Stuff [])))]
                             (if (empty? SegVars)
                                 Main
                                 [package null [] Main | (map (function segdef) SegVars)])))

(define segdef
  S -> [define S
        [@p In Out] Continuation -> [let Continue [Continuation [reverse Out] [@p In []]]
                                         [if [and [= Continue [fail]] [cons? In]]
                                             [S [@p [tl In] [cons [hd In] Out]] Continuation]
                                     Continue]]]) 

(define yacc_cases
  Cases -> (append (mapcan (/. Case [Stream <- Case]) Cases) [_ -> [fail]]))
 
(define first_n
  0 _ -> []
  _ [] -> []
  N [X | Y] -> [X | (first_n (- N 1) Y)])

(define split_cc_rules
  [] [] -> []
  [] RevRule -> [(split_cc_rule (reverse RevRule) [])]
  [; | CC_Stuff] RevRule 
   -> [(split_cc_rule (reverse RevRule) []) | (split_cc_rules CC_Stuff [])]
  [X | CC_Stuff] RevRule -> (split_cc_rules CC_Stuff [X | RevRule]))

(define split_cc_rule 
   [:= Semantics] RevSyntax -> [(reverse RevSyntax) Semantics]
   [:= | Semantics] RevSyntax -> [(reverse RevSyntax) (cons_form Semantics)]
   [] RevSyntax 
   -> (do (output "warning: ")
          (map (/. X (output "~A " X)) (reverse RevSyntax))
          (output "has no semantics.~%")
          (split_cc_rule [:= (default_semantics (reverse RevSyntax))] RevSyntax))
   [Syntax | Rule] RevSyntax -> (split_cc_rule Rule [Syntax | RevSyntax]))

(define default_semantics 
  [] -> []
  [S | Syntax] -> (let PS [snd (concat Parse_ S)]
                   (if (empty? Syntax) 
                       PS 
                       [append PS 
                              (default_semantics Syntax)]))	
                                   where (grammar_symbol? S)
  [S | Syntax] -> [cons S (default_semantics Syntax)])

(define cc_body 
  [Syntax Semantics] -> (syntax Syntax Stream Semantics))

(define syntax 
  [] Stream Semantics -> [reassemble [fst Stream] (semantics Semantics)]
  [S | Syntax] Stream Semantics 
    -> (if (grammar_symbol? S) 
           (recursive_descent [S | Syntax] Stream Semantics)
           (if (segvar? S)
               (segment-match [S | Syntax] Stream Semantics)
               (if (terminal? S)       
               (check_stream [S | Syntax] Stream Semantics)
               (if (jump_stream? S)    
                   (jump_stream [S | Syntax] Stream Semantics)
                   (if (list_stream? S)    
                       (list_stream (decons S) Syntax Stream Semantics)
	               (error "~A is not legal syntax~%" S)))))))
	       

(define list_stream?
  [_ | _] -> true
  _ -> false)

(define decons
  [cons X Y] -> [X | (decons Y)]
  X -> X)

(define list_stream
  S Syntax Stream Semantics 
   -> (let Test [and [cons? [fst Stream]] [cons? [hd [fst Stream]]]]
           Action [snd-or-fail (syntax S 
                          [reassemble [hd [fst Stream]] [snd Stream]]
                          [leave! (syntax Syntax 
                              [reassemble [tl [fst Stream]]
                                          [snd Stream]]
                              Semantics)])] 
          Else [fail]
          [if Test Action Else])) 
          
(define snd-or-fail
  (@p _ Y) -> Y
  _ -> (fail))          
   
(define grammar_symbol?
  S -> (and (symbol? S) 
            (let Cs (explode S) 
                (and (= (hd Cs) "<") (= (hd (reverse Cs)) ">")))))				
  
(define recursive_descent 
  [S | Syntax] Stream Semantics -> (let Test [S Stream]
                                        Action (syntax Syntax 
                                                       (concat Parse_ S) Semantics)
                                        Else [fail]
                                        [let (concat Parse_ S) Test
                                             [if [not [= [fail] (concat Parse_ S)]]
                                                 Action
                                                 Else]])) 
(define segvar?
  S -> (and (symbol? S) (= (hd (explode S)) "?")))

(define segment-match
  [S | Syntax] Stream Semantics 
   -> (let Continuation [lambda S [lambda Restart (syntax Syntax Restart Semantics)]]
           [S Stream Continuation]))

         
(define terminal? 
  [_ | _] -> false
  -*- -> false
  _ -> true)

(define jump_stream?
   -*- -> true
   _ -> false)
  
(define check_stream 
  [S | Syntax] Stream Semantics 
  -> (let Test [and [cons? [fst Stream]] [= S [hd [fst Stream]]]]
          Action (syntax Syntax [reassemble [tl [fst Stream]] 
                                            [snd Stream]] Semantics)
          Else [fail]
          [if Test Action Else])) 

(define reassemble
  _ X -> X  where (= X (fail))
  I O -> (@p I O))

(define jump_stream 
  [S | Syntax] Stream Semantics 
  -> (let Test [cons? [fst Stream]]
          Action (syntax Syntax [reassemble [tl [fst Stream]] 
                                            [snd Stream]] Semantics)
          Else [fail]
          [if Test Action Else]))
  
(define semantics 
  [leave! S] -> S
  [] -> []
  S -> [snd (concat Parse_ S)] 	where (grammar_symbol? S) 
   -o- -> [snd Stream] 
  -*- -> [hd [fst Stream]]     
  -s- -> [fst Stream]     
  [X | Y] -> (map (function semantics) [X | Y])
  X -> X)       

(define fail
  -> fail!) 

(define <!>
  (@p X _) -> (@p [] X)) 