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

(define macroexpand
  X -> (let Y (compose (value *macros*) X)
            (if (= X Y)
                X
                (walk macroexpand Y))))

(set *macros* [timer-macro cases-macro abs-macro put/get-macro compile-macro 
               datatype-macro let-macro assoc-macro make-string-macro output-macro 
               error-macro prolog-macro synonyms-macro nl-macro @s-macro 
               defmacro-macro defprolog-macro function-macro])


(define error-macro
  [error String | Args] -> [simple-error (mkstr String Args)]
  X -> X)
  
(define output-macro
  [output String | Args] -> [pr (mkstr String Args) [stoutput]]
  X -> X)  

(define make-string-macro
  [make-string String | Args] -> (mkstr String Args)
  X -> X)
  
(define compose
   [] X -> X
   [F | Fs] X -> (compose Fs (F X)))
   
(define compile-macro
  [compile F X] -> [compile F X [lambda (protect E) [if [cons? (protect E)]
                                   [error "parse error here: ~S~%" (protect E)]
                                              [error "parse error~%"]]]]
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
   -> [process-datatype (intern (cn "type#" (str F)))
        [compile [function <datatype-rules>] 
                 (rcons_form Rules) [function datatype-error]]]
  X -> X)

(define defmacro-macro
  [defmacro F | Rules] -> (let Macro [define F | (append Rules [(protect X) -> (protect X)])]
                               Declare [do [set *macros* [adjoin F [value *macros*]]] macro]
                               Package [package null [] Declare Macro]
                               Package)
  X -> X)

(defcc <defmacro>
 <name> <macrorules> := [define <name> | <macrorules>];)

(defcc <macrorules>
  <macrorule> <macrorules>;
  <macrorule> := (append <macrorule> [(protect X) -> (protect X)]);)

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
                        [let (protect Start) [get-time run]
                             (protect Result) Process
                             (protect Finish) [get-time run]
                             (protect Time) [- (protect Finish) (protect Start)]
                             (protect Message) [pr [cn "c#10;run time: " 
                                                       [cn [str (protect Time)] 
                                                           " secsc#10;"]]
                                                   [stoutput]] 
                             (protect Result)])
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
  F N Vars -> (let X (gensym (protect V)) 
                [/. X (function-abstraction-help F (- N 1) (append Vars [X]))]))

(define undefmacro
  F -> (do (set *macros* (remove F (value *macros*))) F)))
  