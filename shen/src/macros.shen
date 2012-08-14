(define macroexpand
   X -> (compose (value *macros*) X))

(define macroexpand
  X -> (let Y (compose (value *macros*) X)
            (if (= X Y)
                X
                (walk macroexpand Y))))

(set *macros* [timer-macro cases-macro abs-macro put/get-macro compile-macro yacc-macro 
               datatype-macro let-macro assoc-macro i/o-macro prolog-macro synonyms-macro
               nl-macro vector-macro @s-macro defmacro-macro defprolog-macro function-macro])

(define compose
   [] X -> X
   [F | Fs] X -> (compose Fs (F X)))
   
(define compile-macro
  [compile F X] -> [compile F X []]
  X -> X)   

(define prolog-macro
  [prolog? | X] -> [intprolog (prolog-form X)]
  X -> X)

(define defprolog-macro
  [defprolog F | X] -> (compile (function <defprolog>) [F | X] (/. Y (prolog-error F Y)))
  X -> X)

(define prolog-form
  X -> (cons_form (map (function cons_form) X)))
  
(define datatype-macro
  [datatype F | Rules] 
   -> [process-datatype F [compile [function <datatype-rules>] (rcons_form Rules) [function datatype-error]]]
  X -> X)

(define defmacro-macro
  [defmacro F | Rules] -> (let Macro (compile <defmacro> [F | Rules])
                               Declare [do [set *macros* [adjoin F [value *macros*]]] macro]
                               Package [package null [] Declare Macro]
                               Package)
  X -> X)

(define defmacro-macro
  [defmacro F | Rules] -> (let Macro [define F | (append Rules [X -> X])]
                               Declare [do [set *macros* [adjoin F [value *macros*]]] macro]
                               Package [package null [] Declare Macro]
                               Package)
  X -> X)

(defcc <defmacro>
 <name> <macrorules> := [define <name> | <macrorules>];)

(defcc <macrorules>
  <macrorule> <macrorules>;
  <macrorule> := (append <macrorule> [X -> X]);)

(defcc <macrorule>
  <patterns> -> <macroaction> where <guard>;
  <patterns> -> <macroaction>;
  <patterns> <- <macroaction> where <guard>;
  <patterns> <- <macroaction>;)

(defcc <macroaction>
  <action> := [[walk [function macroexpand] <action>]];)

(define @s-macro
  [@s W X Y | Z] -> [@s W (@s-macro [@s X Y | Z])]
  [@s X Y] -> (let E (explode X)
                   (if (> (length E) 1)
                       (@s-macro [@s | (append E [Y])])
                       [@s X Y]))   where (string? X)
  X -> X)

(define synonyms-macro
  [synonyms | X] -> [synonyms-help (rcons_form X)]
  X -> X)

(define nl-macro
  [nl] -> [nl 1]
  X -> X)

(define vector-macro
   <> -> [vector 0]
   X -> X)

(define yacc-macro
  [defcc F | X] -> (yacc->shen F X (extract-segvars X))
  X -> X)
   
(define assoc-macro   
  [F W X Y | Z] -> [F W (assoc-macro [F X Y | Z])]
                        where (element? F [@p @v append and or + * do])
  X -> X) 
  
(define let-macro
   [let V W X Y | Z] -> [let V W (let-macro [let X Y | Z])]
   X -> X)  

(define abs-macro
   [/. V W X | Y] -> [lambda V (abs-macro [/. W X | Y])]   
   [/. X Y] -> [lambda X Y]
   X -> X)

(define cases-macro
  [cases true X | _] -> X
  [cases X Y] -> [if X Y (i/o-macro [error "error: cases exhausted~%"])]
  [cases X Y | Z] -> [if X Y (cases-macro [cases | Z])]
  [cases X] -> (error "error: odd number of case elements~%")
  X -> X)
  
(define timer-macro
   [time Process] -> (let-macro
                        [let Start [get-time run]
                             Result Process
                             Finish [get-time run]
                             Time [- Finish Start]
                             Message (i/o-macro [output "~%run time: ~A secs~%" Time]) 
                             Result])
    X -> X)                           
  
(define i/o-macro
   [output String | Y] -> [intoutput String (tuple-up Y)]
   [make-string String | Y] -> [intmake-string String (tuple-up Y)]
   [error String | Y] -> [interror String (tuple-up Y)]
   [pr String] -> [pr String [stoutput 0]]
   [read-byte] -> [read-byte [stinput 0]]
   X -> X)
   
(define tuple-up
  [X | Y] -> [@p X (tuple-up Y)]
  X -> X)   
      
(define put/get-macro
  [put X Pointer Y] -> [put X Pointer Y [value *property-vector*]] 
  [get X Pointer] -> [get X Pointer [value *property-vector*]]
  X -> X)

(define function-macro
  [function F] -> (function-abstraction F (arity F))
  X -> X)
  
(define function-abstraction 
  F 0 -> [freeze F]
  F -1 -> F
  F N -> (function-abstraction-help F N []))  
  
(define function-abstraction-help
  F 0 Vars -> [F | Vars]
  F N Vars -> (let X (gensym V) [/. X (function-abstraction-help F (- N 1) (append Vars [X]))]))
  
  