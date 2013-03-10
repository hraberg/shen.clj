\*                                                   

**********************************************************************************
*                           The License						*
* 										*
* The user is free to produce commercial applications with the software, to 	*
* distribute these applications in source or binary  form, and to charge monies *
* for them as he sees fit and in concordance with the laws of the land subject 	*
* to the following license.							*
*										* 
* 1. The license applies to all the software and all derived software and 	*
*    must appear on such.							*
*										*
* 2. It is illegal to distribute the software without this license attached	*
*    to it and use of the software implies agreement with the license as such.  *
*    It is illegal for anyone who is not the copyright holder to tamper with 	*
*    or change the license.							*
*										*
* 3. Neither the names of Lambda Associates or the copyright holder may be used *
*    to endorse or promote products built using the software without specific 	*
*    prior written permission from the copyright holder.			*
*										*
* 4. That possession of this license does not confer on the copyright holder 	*
*    any special contractual obligation towards the user. That in no event 	* 
*    shall the copyright holder be liable for any direct, indirect, incidental, *   
*    special, exemplary or consequential damages (including but not limited     *
*    to procurement of substitute goods or services, loss of use, data, 	* 
*    interruption), however caused and on any theory of liability, whether in	* 
*    contract, strict liability or tort (including negligence) arising in any 	*
*    way out of the use of the software, even if advised of the possibility of 	*
*    such damage.								* 
*										*
* 5. It is permitted for the user to change the software, for the purpose of 	*
*    improving performance, correcting an error, or porting to a new platform, 	*
*    and distribute the derived version of Shen provided the resulting program 	*
*    conforms in all respects to the Shen standard and is issued under that     * 
*    title. The user must make it clear with his distribution that he/she is 	*
*    the author of the changes and what these changes are and why. 		*
*										*
* 6. Derived versions of this software in whatever form are subject to the same *
*    restrictions. In particular it is not permitted to make derived copies of  *
*    this software which do not conform to the Shen standard or appear under a  *
*    different title.								*
*										*
*    It is permitted to distribute versions of Shen which incorporate libraries,*
*    graphics or other facilities which are not part of the Shen standard.	*
*										*
* For an explication of this license see www.shenlanguage.org/license.htm which *
* explains this license in full. 
*				 						*
*********************************************************************************

*\

(package shen. [] 

(define shen->kl 
  F Def -> (compile (function <define>) [F | Def] (/. X (shen-syntax-error F X))))

(define shen-syntax-error
  F X -> (error "syntax error in ~A here:~%~% ~A~%" F (next-50 50 X)))

(defcc <define>
 <name> <signature> <rules> := (compile_to_machine_code <name> <rules>);
 <name> <rules> := (compile_to_machine_code <name> <rules>);)

(defcc <name>
  X := (if (and (symbol? X) (not (sysfunc? X))) 
           X
           (error "~A is not a legitimate function name.~%" X)))

(define sysfunc?
  F -> (element? F (get (intern "shen") external-symbols)))

(defcc <signature>
  { <signature-help> } := (normalise-type (curry-type <signature-help>));)

(define curry-type
  [A --> B --> | C] -> (curry-type [A --> [B --> | C]])
  [cons A _] -> [list (curry-type A)]
  [A * B * | C] -> (curry-type [A * [B * | C]])
  [X | Y] -> (map (function curry-type) [X | Y])
  X -> X) 

(defcc <signature-help> 
  X <signature-help> := [X | <signature-help>]  where (not (element? X [{ }]));
 <e> := [];)

(defcc <rules>
  <rule> <rules> := [<rule> | <rules>];
  <rule> := [<rule>];)
  
(defcc <rule>
  <patterns> -> <action> where <guard> := [<patterns> [where <guard> <action>]];
  <patterns> -> <action> := [<patterns> <action>];
  <patterns> <- <action> where <guard> 
    := [<patterns> [where <guard> [choicepoint! <action>]]];
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
  X := (constructor-error X) 	where (cons? X);
  <simple_pattern> := <simple_pattern>;)

(define constructor-error
  X -> (error "~A is not a legitimate constructor~%" X))

(defcc <simple_pattern>
  X := (gensym (protect Y)) 	where (= X _);
  X := X 		        where (not (element? X [-> <-]));)

(defcc <pattern1>
  <pattern> := <pattern>;)

(defcc <pattern2>
  <pattern> := <pattern>;)

(defcc <action>
  X := X;)

(defcc <guard>
  X := X;)

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
  Bound [P _] -> []  where (= P protect)
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
  [P X] -> X   where (= P protect)
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
         (do (aritycheck-action Action1) (aritycheck Name [[Patts2 Action2] | Rules]))
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
  N -> [(gensym (protect V)) | (parameters (- N 1))])

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
  Cases -> [cond | Cases])
  
(define encode-choices
  [] _ -> []  
  [[true [choicepoint! Action]]] Name 
  -> [[true [let (protect Result) Action
                 [if [= (protect Result) [fail]]
                  (if (value *installing-kl*) [sys-error Name] [f_error Name])
                    (protect Result)]]]]   
  [[true [choicepoint! Action]] | Code] Name 
   -> [[true [let (protect Result) Action
             [if [= (protect Result) [fail]]
              (cond-form (encode-choices Code Name))
              (protect Result)]]]]
  [[Test [choicepoint! Action]] | Code] Name 
   -> [[true [let (protect Freeze) [freeze (cond-form (encode-choices Code Name))]
                 [if Test
                     [let (protect Result) Action
                          [if [= (protect Result) [fail]]
                              [thaw (protect Freeze)]
                              (protect Result)]]
                              [thaw (protect Freeze)]]]]]
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
  Name -> [true [f_error Name]])

(define sys-error
  Name -> (error "system function ~A: unexpected argument~%" Name)) )