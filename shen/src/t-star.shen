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

(define typecheck
  X A -> (let Curry (curry X) 
              ProcessN (start-new-prolog-process)
              Type (insert-prolog-variables (normalise-type (curry-type A)) ProcessN)
              Continuation (freeze (return Type ProcessN void))
              (t* [Curry : Type] [] ProcessN Continuation)))
            
(define curry
  [F | X] -> [F | (map (function curry) X)]   where (special? F)
  [Def F | X] -> [Def F | X] where (extraspecial? Def)
  [F X Y | Z] -> (curry [[F X] Y | Z])
  [F X] -> [(curry F) (curry X)]
  X -> X)  

(define special?
  F -> (element? F (value *special*)))

(define extraspecial?
  F -> (element? F (value *extraspecial*)))
               
(defprolog t* 
          _ _ <-- (fwhen (maxinfexceeded?)) (bind Error (errormaxinfs));
          (mode fail -) _ <-- ! (prolog-failure);
          (mode [X : A] -) Hyp <-- (fwhen (type-theory-enabled?)) ! (th* X A Hyp);
          P Hyp <-- (show P Hyp) (bind Datatypes (value *datatypes*)) (udefs* P Hyp Datatypes);) 

(define type-theory-enabled?
  -> (value *shen-type-theory-enabled?*)) 

(define enable-type-theory
  + -> (set *shen-type-theory-enabled?* true)
  - -> (set *shen-type-theory-enabled?* false) 
  _ -> (error "enable-type-theory expects a + or a -~%"))

(define prolog-failure
  _ _ -> false)                      

(define maxinfexceeded?
  -> (> (inferences) (value *maxinferences*)))

(define errormaxinfs
  -> (simple-error "maximum inferences exceeded~%"))
  
(defprolog udefs*
   P Hyp (mode [D | _] -) <-- (call [D P Hyp]);
   P Hyp (mode [_ | Ds] -) <-- (udefs* P Hyp Ds);)   
                                                     
(defprolog th*
  X A Hyps <-- (show [X : A] Hyps) (when false);
  X A _ <-- (fwhen (typedf? X)) (bind F (sigf X)) (call [F A]);
  X A _ <-- (base X A);
  X A Hyp <-- (by_hypothesis X A Hyp);
  (mode [F] -) A Hyp <-- (th* F [--> A] Hyp);
  (mode [F X] -) A Hyp <-- (th* F [B --> A] Hyp) (th* X B Hyp);
  (mode [cons X Y] -) [list A] Hyp <-- (th* X A Hyp) (th* Y [list A] Hyp);
  (mode [@p X Y] -) [A * B] Hyp <-- (th* X A Hyp) (th* Y B Hyp);
  (mode [@v X Y] -) [vector A] Hyp <-- (th* X A Hyp) (th* Y [vector A] Hyp);
  (mode [@s X Y] -) string Hyp <-- (th* X string Hyp) (th* Y string Hyp);
  (mode [lambda X Y] -) [A --> B] Hyp <-- ! 
                                           (bind X&& (placeholder)) 
                                           (bind Z (ebr X&& X Y))
                                           (th* Z B [[X&& : A] | Hyp]); 
  (mode [let X Y Z] -) A Hyp <-- ! (th* Y B Hyp) 
                                    (bind X&& (placeholder))
                                    (bind W (ebr X&& X Z))
                                    (th* W A [[X&& : B] | Hyp]);
  (mode [open file FileName Direction] -) [stream Direction] Hyp 
   <-- ! (th* FileName string Hyp);
  (mode [type X A] -) B Hyp <-- ! (unify A B) (th* X A Hyp);
  (mode [input+ : A] -) B Hyp <-- (bind C (normalise-type A)) (unify B C);
  (mode [where P X] -) A Hyp <-- ! (th* P boolean Hyp) 
                                 ! 
                                 (th* X A [[P : verified] | Hyp]);
  (mode [set Var Val] -) A Hyp <-- ! (th* [value Var] A Hyp) (th* Val A Hyp);
  (mode [<-sem F] -) C Hyp <-- ! 
                               (th* F [A ==> B] Hyp)
                               !
                               (bind F&& (concat && F))
                               !
                               (th* F&& C [[F&& : B] | Hyp]);
  (mode [fail] -) symbol _ <--;
   X A Hyp <-- (t*-hyps Hyp NewHyp) (th* X A NewHyp);
  (mode [define F | X] -) A Hyp <-- ! (t*-def [define F | X] A Hyp);
  (mode [defcc F | X] -) A Hyp <-- ! (t*-defcc [defcc F | X] A Hyp);
  (mode [process-datatype | _] -) symbol _ <--;
  (mode [synonyms-help | _] -) symbol _ <--;
  X A Hyp <-- (bind Datatypes (value *datatypes*))  
              (udefs* [X : A] Hyp Datatypes);)

(defprolog t*-hyps
    (mode [[[cons X Y] : (mode [list A] +)] | Hyp] -) Out <-- (bind Out [[X : A] [Y : [list A]] | Hyp]);
    (mode [[[@p X Y] : (mode [A * B] +)] | Hyp] -) Out <-- (bind Out [[X : A] [Y : B] | Hyp]);
    (mode [[[@v X Y] : (mode [vector A] +)] | Hyp] -) Out <-- (bind Out [[X : A] [Y : [vector A]] | Hyp]); 
    (mode [[[@s X Y] : (mode string +)] | Hyp] -) Out <-- (bind Out [[X : string] [Y : string] | Hyp]);
    (mode [X | Hyp] -) Out <-- (bind Out [X | NewHyps]) (t*-hyps Hyp NewHyps);) 
             
(define show
  P Hyps ProcessN Continuation 
   -> (do (line)
          (show-p (deref P ProcessN))
          (nl)
          (nl)
          (show-assumptions (deref Hyps ProcessN) 1)
          (output "~%> ") 
          (pause-for-user (value *language*))
          (thaw Continuation))   where (value *spy*)
   _ _ _ Continuation -> (thaw Continuation))

(define line
  -> (let Infs (inferences)
       (output "____________________________________________________________ ~A inference~A ~%?- " 
                Infs (if (= 1 Infs) "" "s"))))
                             
(define show-p 
  [X : A] -> (output "~R : ~R" X A)
  P -> (output "~R" P))
 
\* Enumerate assumptions. *\
(define show-assumptions
  [] _ -> skip
  [X | Y] N -> (do (output "~A. " N) (show-p X) (nl) (show-assumptions Y (+ N 1))))
  
\* Have to parameterise to language because CL does not behave well with read-byte. :< *\
(define pause-for-user
  "Common Lisp" 
   -> (let I ((protect FORMAT) [] "~C" ((protect READ-CHAR))) (if (= I "a") (error "input aborted~%") (nl)))
   _ -> (let I (read-char) (if (= I "a") (error "input aborted~%") (nl)))) 

(define read-char
  -> (read-char-h (read-byte (stinput)) 0))
  
(define read-char-h
  \* State 0; read until the stinput is empty - emptying any buffered bytes. *\
  -1 0 -> (read-char-h (read-byte (stinput)) 1)
  _ 0 -> (read-char-h (read-byte (stinput)) 0)
  -1 1 -> (read-char-h (read-byte (stinput)) 1)
  \* State 1; read until the stinput is not empty - returning the byte as a string. *\
  N 1 -> (n->string N))   

\* Does the function have a type? *\
(define typedf?
   F -> (element? F (value *signedfuncs*)))

\* The name of the Horn clause containing the signature of F. *\
(define sigf 
  F -> (concat type-signature-of- F))  
                                                     
\* Generate a placeholder - a symbol which stands for an arbitrary object.  *\    
(define placeholder
  -> (gensym &&))                                                          

\* base types *\              

(defprolog base
  X number <-- (fwhen (number? X));
  X boolean <-- (fwhen (boolean? X));
  X string <-- (fwhen (string? X));
  X symbol <-- (fwhen (symbol? X)) (fwhen (not (placeholder? X)));
  (mode [] -) [list A] <--;)   
            
\* Recognisor for placeholders - symbols which stand for arbitrary objects. *\
(define placeholder?
   S -> (and (symbol? S) (placeholder-help? (str S))))
   
(define placeholder-help?
   (@s "&&" _) -> true   
   _ -> false)

\* Prove a conclusion from the hypothesis list. *\    
           
(defprolog by_hypothesis
 X A (mode [[Y : B] | _] -) <-- (identical X Y) (unify! A B);
 X A (mode [_ | Hyp] -) <-- (by_hypothesis X A Hyp);)                 

\* Establish the type of a function. *\
      
(defprolog t*-def
  (mode [define F | X] -) A Hyp <-- (bind Sig+Rules (compile (function <sig+rules>) X []))
                                     (bind Error (if (= Sig+Rules (fail))
                                                     (errordef F)
                                                     skip))
                                     (bind Sig (hd Sig+Rules))
                                     (bind Rules (tl Sig+Rules))
                                     (bind Vars (extract_vars Sig))
                                     (bind Sig&& (placeholders Sig Vars))
                                     !
                                     (t*-rules Rules Sig&& 1 F [[F : Sig&&] | Hyp])
                                     (bind Declare (declare F Sig))
                                     (unify! A Sig);)  

\* Parse the def into its parts - a signature and the body of the definition. *\          
(defcc <sig+rules>
  <signature> <trules> := [<signature> | <trules>];) 
  
\* Replace the variables by place holders. *\
(define placeholders
  [X | Y] Vs -> (map (/. Z (placeholders Z Vs)) [X | Y])
  X Vs -> (concat && X)        where (element? X Vs)
  X _ -> X)    

(defcc <trules>
  <trule> <trules> := [<trule> | <trules>];
  <trule> := [<trule>];)
  
(defcc <trule>
  <patterns> <arrow> <action> <guard?> 
     := (let Vars (extract_vars <patterns>)
             Patterns (placeholders <patterns> Vars) 
             Action (placeholders (curry <action>) Vars)
             Guard (placeholders (curry <guard?>) Vars)
             (form-rule Patterns <arrow> Action Guard));)

(define form-rule
  Patterns forward Action Guard -> [Patterns (if (= Guard skip)
                                                 Action
                                                 [where Guard Action])]
  Patterns backward [[fail-if F] X] Guard -> [Patterns (if (= Guard skip)
                                                         [where [not [F X]] X]
                                                         [where [[and Guard] [not [F X]]] X])]
  Patterns backward Action Guard -> [Patterns (if (= Guard skip)
                                                  [where [not [[== Action] [fail]]] Action]
                                                  [where [[and Guard] [not [[== Action] [fail]]]] Action])])
                                                  
(defcc <guard?>
  where <guard> := <guard>;
  <e> := skip;)
   
(defcc <arrow>
  -> := forward; 
  <- := backward;)                                                       
                         
\* Error message if def does not parse. *\                                
(define errordef
  F -> (error "syntax error in ~A~%" F))

\* Establish the type of the rules of a function *\

(defprolog t*-rules
  (mode [] -) _ _ _ _ <--;
  (mode [Rule | Rules] -) A N F Hyp <-- (t*-rule Rule A N F Hyp) ! (bind M (+ N 1))  (t*-rules Rules A M F Hyp);) 
                                                            
\* Establish the type of a rule of a function *\
            
(defprolog t*-rule
   Rule A N F Hyp <-- (t*-ruleh Rule A Hyp);
   _ _ N F _ <-- (bind Error (type-insecure-rule-error-message N F));)
                                
(defprolog t*-ruleh
  (mode [[] Result] -) [--> A] Hyp <-- ! (th* Result A Hyp);
  (mode [Patterns Result] -) A Hyp <-- (t*-patterns Patterns A NewHyp B)
                                         !
                                        (conc NewHyp Hyp AllHyp)
                                        !
                                        (th* Result B AllHyp);)                     

(define type-insecure-rule-error-message
  N F -> (error "type error in rule ~A of ~A~%" N F))  

\* Establish that each pattern meets type constraints *\               
                         
(defprolog t*-patterns
  (mode [] -) B [] B <--;
  (mode [Pattern | Patterns] -) (mode [A --> B] -) [[Pattern : A] | Hyp] C
                      <-- (t*-assume Pattern Assume) !
                           (th* Pattern A Assume) !
                           (t*-patterns Patterns B Hyp C);) 
                             
\* Generate the assumptions for each pattern. *\                         

(defprolog t*-assume
   (mode [X | Y] -) Assume <-- ! 
                              (t*-assume X A1) 
                              (t*-assume Y A2) 
                              (bind Assume (append A1 A2));
   X Out <-- (fwhen (placeholder? X)) (bind Out [[X : A]]);
   _ [] <--;)
   
(defprolog conc
 (mode [] -) X Out <-- (bind Out X);
 (mode [X | Y] -) W Out <-- (bind Out [X | Z]) (conc Y W Z);)   
  
(defprolog findallhelp
  Pattern Literal X A <-- (call Literal) (remember A Pattern) (when false);
  _ _ X A <-- (bind X (value A));)

(defprolog remember
  A Pattern <-- (is B (set A [Pattern | (value A)]));) 
  
(defprolog findall
  Pattern Literal X <-- (bind A (gensym a)) (bind B (set A [])) (shen-findallhelp Pattern Literal X A);)  

(defprolog t*-defcc
 (mode [defcc F { [list A] ==> B } | Rest] -) C Hyp 
    <-- (bind Sig (placeholders [[list A] ==> B] (extract_vars [[list A] ==> B])))
        (bind ListA&& (hd Sig))
        (bind B&& (hd (tl (tl Sig))))
        (bind Rest& (plug-wildcards Rest))
        (bind Rest&& (placeholders Rest& (extract_vars Rest&)))
        (get-rules Rules Rest&&)
        !
        (tc-rules F Rules ListA&& B&& [[F : Sig] | Hyp] 1)
        (unify C [[list A] ==> B])
        (bind Declare (declare F [[list A] ==> B]));)

(define plug-wildcards
  [X | Y] -> (map (function plug-wildcards) [X | Y])
  X -> (gensym (intern "X"))   where (= X _)
  X -> X)
                                       
(defprolog get-rules 
  [] (mode [] -) <-- !;
  [Rule | Rules] Rest <-- (first-rule Rest Rule Other) 
                          !
                          (get-rules Rules Other);)
                          
(defprolog first-rule
  (mode [; | Other] -) [] Other <-- !;
  (mode [X | Rest] -) [X | Rule] Other <-- (first-rule Rest Rule Other);)
                                           
(defprolog tc-rules
  _ (mode [] -) _ _ _ _ <--;
  F (mode [Rule | Rules] -) (mode [list A] -) B Hyps N
  <-- (tc-rule F Rule A B Hyps N)
      (is M (+ N 1))
      !
      (tc-rules F Rules [list A] B Hyps M);)

(defprolog tc-rule
  _ Rule A B Hyps _ <-- (check-defcc-rule Rule A B Hyps);
  F _ _ _ _ N <-- (bind Err (error "type error in rule ~A of ~A" N F));)

(defprolog check-defcc-rule 
  Rule A B Hyps <--
      (get-syntax+semantics Syntax Semantics Rule)
      !
      (syntax-hyps Syntax Hyps SynHyps A) 
      !
      (syntax-check Syntax A SynHyps)
      !
      (semantics-check Semantics B SynHyps);)
      
(defprolog syntax-hyps
  (mode [] -) SynHyps SynHyps A <--;
  (mode [[X | Y] | Z] -) Hyps SynHyps A <-- ! 
                                            (conc [X | Y] Z W) 
                                            ! 
                                            (syntax-hyps W Hyps SynHyps A);
  (mode [X | Y] -) Hyps [[X : A] | SynHyps] A <-- (when (placeholder? X))
                                                  !
                                                  (syntax-hyps Y Hyps SynHyps A);
  (mode [_ | Y] -) Hyps SynHyps A <-- (syntax-hyps Y Hyps SynHyps A);)  
                                     
(defprolog get-syntax+semantics
  [] S (mode [:= Semantics] -) <-- ! (bind S Semantics);
  [] S (mode [:= Semantics where G] -) <-- ! (bind S [where G Semantics]);
  [X | Syntax] Semantics (mode [X | Rule] -)
   <-- (get-syntax+semantics Syntax Semantics Rule);)
                                     
(defprolog syntax-check
  (mode [] -) _ _ <--;
  (mode [X | Syntax] -) A Hyps <-- (fwhen (grammar_symbol? X))
                                   !
                                   (t* [X : [[list B] ==> C]] Hyps)
                                   !
                                   (bind X&& (concat && X))
                                   !
                                   (t* [X&& : [list A]] [[X&& : [list B]] | Hyps]) 
                                   !
                                   (syntax-check Syntax A Hyps);
  (mode [X | Syntax] -) A Hyps <-- (t* [X : A] Hyps)
                                   !
                                   (syntax-check Syntax A Hyps);)
  
(defprolog semantics-check
  Semantics B Hyps <-- (is Semantics* (curry (rename-semantics Semantics)))
                       (t* [Semantics* : B] Hyps);)
                       
(define rename-semantics
  [X | Y] -> [(rename-semantics X) | (rename-semantics Y)]
  X -> [<-sem X]  where (grammar_symbol? X)
  X -> X)   
  )
         
         