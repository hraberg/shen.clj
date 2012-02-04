(define kl-to-lisp
   Params Param -> Param    where (element? Param Params)
   Params [type X _] -> (kl-to-lisp Params X)
   Params [lambda X Y] -> [FUNCTION [LAMBDA [X] (kl-to-lisp [X | Params] Y)]] 
   Params [let X Y Z] -> [LET [[X (kl-to-lisp Params Y)]] 
                                 (kl-to-lisp [X | Params] Z)]
   _ [defun F Params Code] -> [DEFUN F Params (kl-to-lisp Params Code)]
   Params [cond | Cond] -> [COND | (map (/. C (cond_code Params C)) (insert-default Cond))]  
   Params [Param | X] -> (higher-order-code Param
                                (map (/. Y (kl-to-lisp Params Y)) X))
                                        where (element? Param Params)
   Params [[X | Y] | Z] -> (higher-order-code (kl-to-lisp Params [X | Y])
                                               (map (/. W (kl-to-lisp Params W)) Z))   
   Params [F | X] -> (assemble-application F 
                                     (map (/. Y (kl-to-lisp Params Y)) X))
                                 where (symbol? F)
   _ [] -> []                              
   _ S -> [QUOTE S]  where (or (symbol? S) (boolean? S))
   _ X -> X)

(define insert-default
  [] -> [[true [ERROR "error: cond failure~%"]]]
  [[true X] | Y] -> [[true X] | Y] 
  [Case | Cases] -> [Case | (insert-default Cases)])    
   
(define higher-order-code 
   F X -> [let Args [LIST | X]
            [let NewF [maplispsym F]
               [trap-error [APPLY NewF Args] 
                           [lambda E [COND [[arity-error? F Args] 
                                            [funcall [EVAL [nest-lambda F NewF]] Args]]
                                           [[EQ NewF [QUOTE or]]
                                            [funcall [lambda X1 [lambda X2 [or X1 X2]]] Args]]
                                           [[EQ NewF [QUOTE and]]
                                            [funcall [lambda X1 [lambda X2 [and X1 X2]]] Args]]
                                           [[EQ NewF [QUOTE trap-error]]
                                            [funcall [lambda X1 [lambda X2 [trap-error X1 X2]]] Args]]
                                           [[bad-lambda-call? NewF Args] 
                                            [funcall NewF Args]]
                                           [T [relay-error E]]]]]]])
                                           
(define bad-lambda-call?
  F Args -> (AND (FUNCTIONP F) (NOT (= (LIST-LENGTH Args) 1))))
                                         
(define relay-error
  E -> (ERROR (error-to-string E)))
                                         
(define funcall
  Lambda [] -> Lambda
  Lambda [X | Y] -> (funcall (FUNCALL Lambda X) Y))   
  
(define arity-error?
   F Args -> (AND (SYMBOLP F)
                  (> (trap-error (arity F) (/. E -1)) (LIST-LENGTH Args)))       
   
(define nest-lambda
   F NewF -> (nest-lambda-help NewF (trap-error (arity F) (/. E -1))))
   
(define nest-lambda-help
  F -1 -> F
  F 0 -> F
  F N -> (let X (GENSYM "Y")
                 [lambda X (nest-lambda-help (add-p F X) (- N 1))]))
  
(define add-p
  [F | X] Y -> (append [F | X] [Y])
  F X -> [F X])           
 
(define cond_code
   Params [Test Result] -> [(lisp_test Params Test) 
                             (kl-to-lisp Params Result)])
                             
(define lisp_test
   _ true -> T
   Params [and | Tests] -> [AND | (map (/. X (wrap (kl-to-lisp Params X))) Tests)]
   Params Test -> (wrap (kl-to-lisp Params Test))) 
   
 (define wrap 
    [cons? X] -> [CONSP X]
    [string? X] -> [STRINGP X]
    [number? X] -> [NUMBERP X]
    [empty? X] -> [NULL X]
    [and P Q] -> [AND (wrap P) (wrap Q)]
    [or P Q] -> [OR (wrap P) (wrap Q)] 
    [not P] -> [NOT (wrap P)] 
    [equal? X []] -> [NULL X]
    [equal? [] X] -> [NULL X]
    [equal? X [Quote Y]] -> [EQ X [Quote Y]]    
                              where (and (= (SYMBOLP Y) T) (= Quote QUOTE))
    [equal? [Quote Y] X] -> [EQ [Quote Y] X]    
                                where (and (= (SYMBOLP Y) T) (= Quote QUOTE)) 
    [equal? [fail] X] -> [EQ [fail] X]
    [equal? X [fail]] -> [EQ X [fail]]
    [equal? S X] -> [EQUAL S X]  where (string? S)
    [equal? X S] -> [EQUAL X S]  where (string? S)
    [equal? X Y] -> [shen-ABSEQUAL X Y]
    [shen-+string? [tlstr X]] -> [NOT [STRING-EQUAL [tlstr X] ""]]
    [shen-pvar? X] -> [AND [ARRAYP X] [NOT [STRINGP X]] [EQ [AREF X 0] [QUOTE shen-pvar]]] 
    [tuple? X] -> [AND [ARRAYP X] [NOT [STRINGP X]] [EQ [AREF X 0] [QUOTE shen-tuple]]]
    [greater? X Y] -> [> X Y]
    [greater-than-or-equal-to? X Y] -> [>= X Y]
    [less? X Y] -> [< X Y]
    [less-than-or-equal-to? X Y] -> [<= X Y]
    X -> [wrapper X])

 (define wrapper
   true -> T
   false -> []
   X -> (error "boolean expected: not ~S~%" X)) 
   
 (define assemble-application
   hd [X] -> [CAR X]
   tl [X] -> [CDR X]
   cons [X Y] -> [CONS X Y]
   append [X Y] -> [APPEND X Y]
   reverse [X] -> [REVERSE X]
   if [P Q R] -> [IF (wrap P) Q R]
  \ do [X Y] -> [PROG2 X Y]\
   + [1 X] -> [1+ X]
   + [X 1] -> [1+ X]
   - [X 1] -> [1- X]
   value [[Quote X]] -> X  where (= Quote QUOTE)
   set [[Quote X] [1+ X]] -> [INCF X]  where (= Quote QUOTE)
   set [[Quote X] [1- X]] -> [DECF X]  where (= Quote QUOTE)
   F X -> (let NewF (maplispsym F)
               Arity (trap-error (arity F) (/. E -1))
               (if (or (= Arity (length X)) (= Arity -1))
                   [NewF | X]
                   [funcall (nest-lambda F NewF) [LIST | X]])))
                   
(define maplispsym  
    = -> equal?
    > -> greater?
    < -> less?
    >= -> greater-than-or-equal-to?
    <= -> less-than-or-equal-to?
    + -> add
    - -> subtract
    / -> divide
    * -> multiply
    F -> F)
    
 (define factorh
  [Defun F Params [Cond | Code]] -> [Defun F Params [BLOCK [] (process-tree (tree (map returns Code)))]]
                                       where (and (= Cond COND) (= Defun DEFUN))
  Code -> Code)
  
(define returns
  [Test Result] -> [Test [RETURN Result]]) 
    
(define process-tree
   (@p P Q R no-tag) -> [IF P (optimise-selectors P (process-tree Q)) (process-tree R)]
   (@p P Q R Tag) -> [TAGBODY [IF P (optimise-selectors P (process-tree Q))] Tag (process-tree R)]
   Q -> Q    where (not (tuple? Q))) 
   
(define optimise-selectors 
  Test Code -> (optimise-selectors-help (selectors-from Test) Code))                       

(define selectors-from
  [Consp X] -> [[CAR X] [CDR X]]    where (= Consp CONSP)
  [tuple? X] -> [[fst X] [snd X]]  
  _ -> [])    
  
(define optimise-selectors-help 
   [] Code -> Code
   [S1 S2] Code -> (let O1 (occurrences S1 Code)  
                        O2 (occurrences S2 Code) 
                        V1 (gensym V)
                        V2 (gensym V)
                        (if (and (> O1 1) (> O2 1))
                            [LET [[V1 S1] [V2 S2]]  
                                 (subst V1 S1 (subst V2 S2 Code))]
                            (if (> O1 1)
                                 [LET [[V1 S1]] (subst V1 S1 Code)] 
                                 (if (> O2 1)
                                      [LET [[V2 S2]] (subst V2 S2 Code)]
                                      Code)))))        

(define tree
  [[[And P Q] R] | S] ->  (let Tag (gensym tag)
                                Left (tree (append (branch-by P [[[And P Q] R] | S]) [[T [GO Tag]]]))
                                Right (tree (branch-by-not P [[[And P Q] R] | S]))                                
                                (@p P Left Right Tag))     where (= And AND)                 
  [[True Q] | _] -> Q     where (= True T)                         
  [[P Q] | R] -> (@p P Q (tree R) no-tag))
  
(define branch-by
  P [[[And P Q] R] | S] -> [[Q R] | (branch-by P S)]   where (= And AND)
  P [[P R] | S] -> [[T R]]
  _ Code -> [])
  
(define branch-by-not
  P [[[And P Q] R] | S] -> (branch-by-not P S)  where (= And AND)
  P [[P R] | S] -> S
  _ Code -> Code)   