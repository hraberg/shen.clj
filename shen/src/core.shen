(define shen->kl 
  F Def -> (compile (function <define>) [F | Def] (/. X (shen-syntax-error F X))))

(define shen-syntax-error
  F X -> (error "syntax error in ~A here:~%~% ~A~%" F (next-50 50 X)))

(defcc <define>
 <name> <signature> <rules> := (compile_to_machine_code <name> <rules>);
 <name> <rules> := (compile_to_machine_code <name> <rules>);)

(defcc <name>
  -*- := (if (and (symbol? -*-) (not (sysfunc? -*-))) 
             -*-
             (error "~A is not a legitimate function name.~%" -*-)))

(define sysfunc?
  F -> (element? F (value *system*)))

(defcc <signature>
  { <signature-help> } := (normalise-type (curry-type <signature-help>));)

(define curry-type
  [A --> B --> | C] -> (curry-type [A --> [B --> | C]])
  [cons A _] -> [list (curry-type A)]
  [A * B * | C] -> (curry-type [A * [B * | C]])
  [X | Y] -> (map (function curry-type) [X | Y])
  X -> X) 

(defcc <signature-help> 
  -*- <signature-help> := (if (element? -*- [{ }]) 
                              (fail)
                              [-*- | <signature-help>]);
 <e> := [];)

(defcc <rules>
  <rule> <rules> := [<rule> | <rules>];
  <rule> := [<rule>];)
  
(defcc <rule>
  <patterns> -> <action> where <guard> := [<patterns> [where <guard> <action>]];
  <patterns> -> <action> := [<patterns> <action>];
  <patterns> <- <action> where <guard> := [<patterns> [where <guard> [choicepoint! <action>]]];
  <patterns> <- <action> := [<patterns> [choicepoint! <action>]];)   

(define fail_if
  F X -> (if (F X) (fail) X))

(define succeeds?
  X -> false  where (= X (fail))
  _ -> true)

(defcc <patterns>
  <pattern> <patterns> := [<pattern> | <patterns>];
  <e> := [];)

(defcc <pattern>
  [@p <pattern1> <pattern2>] := [@p <pattern1> <pattern2>];
  [cons <pattern1> <pattern2>] := [cons <pattern1> <pattern2>];
  [@v <pattern1> <pattern2>] := [@v <pattern1> <pattern2>];
  [@s <pattern1> <pattern2>] := [@s <pattern1> <pattern2>];
  [vector 0] := [vector 0];
  -*- := (if (cons? -*-) 
             (error "~A is not a legitimate constructor~%" -*-) 
             (fail));
  <simple_pattern> := <simple_pattern>;)

(defcc <simple_pattern>
  -*- := (if (= -*- _) (gensym X) (fail));
  -*- := (if (element? -*- [-> <-]) (fail) -*-);)

(defcc <pattern1>
  <pattern> := <pattern>;)

(defcc <pattern2>
  <pattern> := <pattern>;)

(defcc <action>
  -*- := -*-;)

(defcc <guard>
  -*- := -*-;)

(define compile_to_machine_code 
  Name Rules -> (let Lambda+ (compile_to_lambda+ Name Rules)
                     KL (compile_to_kl Name Lambda+)
                     Record (record-source Name KL)
                     KL))

(define record-source
   _ _ -> skip    where (value *installing-kl*)
   Name ObjectCode -> (put Name source ObjectCode))

(define compile_to_lambda+
  Name Rules -> (let Arity (aritycheck Name Rules)
                     Free (map (/. Rule (free_variable_check Name Rule)) Rules)
                     Variables (parameters Arity)
                     Linear (map (function linearise) (strip-protect Rules))
                     Abstractions (map (function abstract_rule) Linear)
                     Applications 
                       (map (/. X (application_build Variables X))
                            Abstractions)
                     [Variables Applications]))

(define free_variable_check
  Name [Patts Action] -> (let Bound (extract_vars Patts)
                              Free (extract_free_vars Bound Action)
                              (free_variable_warnings Name Free)))

(define extract_vars
  X -> [X]	where (variable? X)
  [X | Y] -> (union (extract_vars X) (extract_vars Y))
  X -> [])

(define extract_free_vars
  Bound [protect _] -> []
  Bound X -> [X]	where (and (variable? X) (not (element? X Bound)))
  Bound [lambda X Y] -> (extract_free_vars [X | Bound] Y)
  Bound [let X Y Z] -> (union (extract_free_vars Bound Y) 
                              (extract_free_vars [X | Bound] Z))
  Bound [X | Y] -> (union (extract_free_vars Bound X) 
                          (extract_free_vars Bound Y))
  _ _ -> [])

(define free_variable_warnings
  _ [] -> _
  Name Vs -> (error "error: the following variables are free in ~A: ~A" Name (list_variables Vs)))

(define list_variables
  [V] -> (cn (str V) ".")
  [V | Vs] -> (cn (str V) (cn ", " (list_variables Vs))))

(define strip-protect
  [protect X] -> X
  [X | Y] -> [(strip-protect X) | (strip-protect Y)]
  X -> X)
                        
(define linearise
  [Patts Action] -> (linearise_help (flatten Patts) Patts Action))

(define flatten
  [] -> []
  [X | Y] -> (append (flatten X) (flatten Y))
  X -> [X])

(define linearise_help
  [] Patts Action -> [Patts Action]
  [X | Y] Patts Action -> (if (and (variable? X) (element? X Y))
                                    (let Var (gensym X)
                                         NewAction [where [= X Var] Action]
                                         NewPatts (linearise_X X Var Patts)
                                         (linearise_help Y NewPatts NewAction))
                                    (linearise_help Y Patts Action)))

(define linearise_X
  X Var X -> Var
  X Var [Y | Z] -> (let L (linearise_X X Var Y)
                       (if (= L Y)
                           [Y | (linearise_X X Var Z)]
                           [L | Z]))
  _ _ Y -> Y)  

(define aritycheck
  Name [[Patts Action]] -> (do (aritycheck-action Action) (aritycheck-name Name (arity Name) (length Patts)))
  Name [[Patts1 Action1] [Patts2 Action2] | Rules] 
  -> (if (= (length Patts1) (length Patts2))
         (do (aritycheck-action Action) (aritycheck Name [[Patts2 Action2] | Rules]))
         (error "arity error in ~A~%" Name)))

(define aritycheck-name
  _ -1 Arity -> Arity
  _ Arity Arity -> Arity
  Name _ Arity -> (do (output "~%warning: changing the arity of ~A can cause errors.~%" Name) Arity))

(define aritycheck-action
  [F | X] -> (do (aah F X) (map (function aritycheck-action) [F | X]))
  _ -> skip)

(define aah
  F X -> (let Arity (arity F)
              Len (length X)
                  (if (and (> Arity -1) (> Len Arity))
                      (output "warning: ~A might not like ~A argument~A.~%" F Len (if (> Len 1) "s" ""))
                      skip)))
                      
(define abstract_rule
  [Patterns Action] -> (abstraction_build Patterns Action))  

(define abstraction_build
  [] Action -> Action
  [Patt | Patts] Action -> [/. Patt (abstraction_build Patts Action)])
   
(define parameters
  0 -> []
  N -> [(gensym V) | (parameters (- N 1))])

(define application_build
  [] Application -> Application
  [V | Vs] Abstraction -> (application_build Vs [Abstraction V]))

(define compile_to_kl
  Name [Variables Applications] 
   -> (let Arity (store-arity Name (length Variables))
           Reduce (map (function reduce) Applications)
           CondExpression (cond-expression Name Variables Reduce)
           KL [defun Name Variables CondExpression]
           KL))

(define store-arity
  _ _ -> skip    where (value *installing-kl*)
  F Arity -> (put F arity Arity))

(define reduce
  Application -> (do (set *teststack* [])
                     (let Result (reduce_help Application)
                          [[:tests | (reverse (value *teststack*))] Result])))

(define reduce_help
   [[/. [cons X Y] Z] A] 
   -> (do (add_test [cons? A]) 
          (let Abstraction [/. X [/. Y (ebr A [cons X Y] Z)]]
               Application [[Abstraction [hd A]] [tl A]]
               (reduce_help Application)))
   [[/. [@p X Y] Z] A] 
   -> (do (add_test [tuple? A]) 
          (let Abstraction [/. X [/. Y (ebr A [@p X Y] Z)]]
               Application [[Abstraction [fst A]] [snd A]]
               (reduce_help Application)))
   [[/. [@v X Y] Z] A] 
   -> (do (add_test [+vector? A]) 
          (let Abstraction [/. X [/. Y (ebr A [@v X Y] Z)]]
               Application [[Abstraction [hdv A]] [tlv A]]
               (reduce_help Application)))
   [[/. [@s X Y] Z] A] 
   -> (do (add_test [+string? A]) 
          (let Abstraction [/. X [/. Y (ebr A [@s X Y] Z)]]
               Application [[Abstraction [pos A 0]] [tlstr A]]
               (reduce_help Application)))
   [[/. X Z] A] -> (do (add_test [= X A])
                       (reduce_help Z))  where (not (variable? X))
   [[/. X Z] A] -> (reduce_help (ebr A X Z))
   [where P Q] -> (do (add_test P) (reduce_help Q))
   [X Y] -> (let Z (reduce_help X) 
                   (if (= X Z) [X Y] (reduce_help [Z Y])))
   X -> X)

(define +string?
  "" -> false
  X -> (string? X))

(define +vector
  X -> false where (= X (vector 0))
  X -> (vector? X))

(define ebr
  A B B -> A
  A B [/. C D] -> [/. C D]	where (> (occurrences B C) 0)
  A B [let B C D] -> [let B (ebr A B C) D]	
  A B [C | D] -> [(ebr A B C) | (ebr A B D)]
  _ _ C -> C)  

(define add_test
   Test -> (set *teststack* [Test | (value *teststack*)]))   

(define cond-expression
  Name Variables Code -> (let Err (err-condition Name)
                              Cases (case-form Code Err)
                              EncodeChoices (encode-choices Cases Name)
                              (cond-form EncodeChoices)))

(define cond-form
  [[true Result] | _] -> Result	
  \[let X Y Z] -> [let X Y Z]\
  Cases -> [cond | Cases])
  
(define encode-choices
  [] _ -> []  
  [[true [choicepoint! Action]]] Name -> [[true [let Result Action
                                                  [if [= Result [fail]]
                                                      (if (value *installing-kl*) [sys-error Name] [f_error Name])
                                                      Result]]]]   
  [[true [choicepoint! Action]] | Code] Name -> [[true [let Result Action
                                                               [if [= Result [fail]]
                                                                   (cond-form (encode-choices Code Name))
                                                                   Result]]]]
  [[Test [choicepoint! Action]] | Code] Name -> [[true [let Freeze [freeze (cond-form (encode-choices Code Name))]
                                                             [if Test
                                                                 [let Result Action
                                                                      [if [= Result [fail]]
                                                                          [thaw Freeze]
                                                                          Result]]
                                                                 [thaw Freeze]]]]]
  [[Test Result] | Code] Name -> [[Test Result] | (encode-choices Code Name)])   

(define case-form
  [] Err -> [Err]
  [[[:tests] [choicepoint! Result]] | Code] Err -> [[true [choicepoint! Result]] | (case-form Code Err)]
  [[[:tests] Result] | _] _ -> [[true Result]]
  [[[:tests | Tests] Result] | Code] Err 
   -> [[(embed-and Tests) Result] | (case-form Code Err)])

(define embed-and
  [Test] -> Test
  [Test | Tests] -> [and Test (embed-and Tests)])

(define err-condition
  Name -> [true [sys-error Name]]    where (value *installing-kl*)
  Name -> [true [f_error Name]])

(define sys-error
  Name -> (error "system function ~A: unexpected argument~%" Name))