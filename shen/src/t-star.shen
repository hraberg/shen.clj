(package shen- []

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
  -> (> (inferences skip) (value *maxinferences*)))

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
  (mode [where P X] -) A Hyp <-- ! (th* P boolean Hyp) ! (th* X A [[P : verified] | Hyp]);
  (mode [set Var Val] -) A Hyp <-- ! (th* [value Var] A Hyp) (th* Val A Hyp);
  (mode [fail] -) symbol _ <--;
   X A Hyp <-- (t*-hyps Hyp NewHyp) (th* X A NewHyp);
  (mode [define F | X] -) A Hyp <-- ! (t*-def [define F | X] A Hyp);
  (mode [process-datatype | _] -) symbol _ <--;
  (mode [synonyms-help | _] -) symbol _ <--;
  X A Hyp <-- (bind Datatypes (value *datatypes*))  (udefs* [X : A] Hyp Datatypes);) 
 
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
  -> (let Infs (inferences _)
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
   -> (let I (FORMAT [] "~C" (READ-CHAR)) (if (= I "a") (error "input aborted~%") (nl)))
   _ -> (let I (read-char) (if (= I "a") (error "input aborted~%") (nl)))) 

(define read-char
  -> (read-char-h (read-byte) 0))
  
(define read-char-h
  \* State 0; read until the stinput is empty - emptying any buffered bytes. *\
  -1 0 -> (read-char-h (read-byte) 1)
  _ 0 -> (read-char-h (read-byte) 0)
  -1 1 -> (read-char-h (read-byte) 1)
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
   (mode [X | Y] -) Assume <-- ! (t*-assume X A1) (t*-assume Y A2) (bind Assume (append A1 A2));
   X Out <-- (fwhen (placeholder? X)) (bind Out [[X : A]]);
   _ [] <--;)
   
(defprolog conc
 (mode [] -) X Out <-- (bind Out X);
 (mode [X | Y] -) W Out <-- (bind Out [X | Z]) (conc Y W Z);)   
  
(defprolog findallhelp
  Pattern Literal X A <-- (call Literal) (remember A Pattern) (when false);
  _ _ X A <-- (bind X (value A));)

(defprolog remember
  A Pattern <-- (is B (set A [Pattern | (value A)]));) )
  
(defprolog findall
  Pattern Literal X <-- (bind A (gensym a)) (bind B (set A [])) (shen-findallhelp Pattern Literal X A);)                               