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

(define declare
  F A -> (let Record (set *signedfuncs* (adjoin F (value *signedfuncs*)))
              Variancy (trap-error (variancy-test F A) (/. E skip))
              Type (rcons_form (normalise-type A))
              F* (concat type-signature-of- F)
              Parameters (parameters 1)       
              Clause [[F* (protect X)] :- [[unify! (protect X) Type]]]
              AUM_instruction (aum Clause Parameters)
              Code (aum_to_shen AUM_instruction) 
              ShenDef [define F* | (append Parameters 
                                           [(protect ProcessN) (protect Continuation)] 
                                           [-> Code])]
              Eval (eval-without-macros ShenDef)
              F)) 

(define normalise-type
  X -> (fix (function normalise-type-help) X))
  
(define normalise-type-help
  [X | Y] -> (normalise-X (map (function normalise-type-help) [X | Y]))
  X -> (normalise-X X))
                 
(define normalise-X
  X -> (let Val (assoc X (value *synonyms*))
           (if (empty? Val)
               X
               (tl Val))))
  
(define variancy-test 
  F A -> (let TypeF (typecheck F (protect B)) 
              Check (cases (= symbol TypeF) skip
                           (variant? TypeF A) skip
                           true (output "warning: changing the type of ~A may create errors~%" F))
              skip))
              
(define variant?
  X X -> true
  [X | Y] [X | Z] -> (variant? Y Z)
  [X | Y] [W | Z] -> (variant? (subst a X Y) (subst a W Z))  where (and (pvar? X) (variable? W))
  [[X | Y] | Z] [[X* | Y*] | Z*] -> (variant? (append [X | Y] Z) (append [X* | Y*] Z*))
  _ _ -> false)

(declare absvector? [A --> boolean])
(declare adjoin [A --> [[list A] --> [list A]]])
(declare and [boolean --> [boolean --> boolean]])
(declare app [A --> [string --> [symbol --> string]]])
(declare append [[list A] --> [[list A] --> [list A]]])
(declare arity [A --> number])
(declare assoc [A --> [[list [list A]] --> [list A]]])
(declare boolean? [A --> boolean])    
(declare bound? [symbol --> boolean])
(declare cd [string --> string]) 
(declare close [[stream A] --> [list B]])
(declare cn [string --> [string --> string]])
(declare compile [[[list A] ==> B] --> [[list A] --> [[[list A] --> B] --> B]]])
(declare cons? [A --> boolean])
(declare destroy [[A --> B] --> [A --> B]])
(declare difference [[list A] --> [[list A] --> [list A]]]) 
(declare do [A --> [B --> B]])
(declare <e> [[list A] ==> [list B]])
(declare <!> [[list A] ==> [list A]])
(declare element? [A --> [[list A] --> boolean]]) 
(declare empty? [A --> boolean]) 
(declare enable-type-theory [symbol --> boolean]) 
(declare external [symbol --> [list symbol]])
(declare error-to-string [exception --> string])
(declare explode [A --> [list string]])
(declare fail-if [[symbol --> boolean] --> [symbol --> symbol]])  
(declare fix [[A --> A] --> [A --> A]])  
(declare format [[stream out] --> [string --> [A --> string]]])
(declare freeze [A --> [lazy A]])
(declare fst [[A * B] --> A]) 
(declare gensym [symbol --> symbol])
(declare <-vector [[vector A] --> [number --> A]]) 
(declare vector-> [[vector A] --> [number --> [A --> [vector A]]]]) 
(declare vector [number --> [vector A]]) 
(declare get-time [symbol --> number])
(declare hash [A --> [number --> number]])
(declare head [[list A] --> A]) 
(declare hdv [[vector A] --> A])
(declare hdstr [string --> string])
(declare if [boolean --> [A --> [A --> A]]]) 
(declare include [[list symbol] --> [list symbol]])
(declare include-all-but [[list symbol] --> [list symbol]]) 
(declare inferences [--> number]) 
(declare insert [A --> [string --> string]])
(declare integer? [A --> boolean])
(declare intersection [[list A] --> [[list A] --> [list A]]])
(declare length [[list A] --> number])
(declare limit [[vector A] --> number])
(declare load [string --> symbol])
(declare map [[A --> B] --> [[list A] --> [list B]]]) 
(declare mapcan [[A --> [list B]] --> [[list A] --> [list B]]])
(declare maxinferences [number --> number]) 
(declare n->string [number --> string])
(declare nl [number --> number])
(declare not [boolean --> boolean]) 
(declare nth [number --> [[list A] --> A]])
(declare number? [A --> boolean])
(declare occurrences [A --> [B --> number]])
(declare occurs-check [symbol --> boolean])
(declare or [boolean --> [boolean --> boolean]]) 
(declare pos [string --> [number --> string]])
(declare pr [string --> [[stream out] --> string]])
(declare print [A --> A])
(declare profile [[A --> B] --> [A --> B]]) 
(declare preclude [[list symbol] --> [list symbol]])
(declare proc-nl [string --> string])
(declare profile-results [A --> symbol]) 
(declare protect [symbol --> symbol])
(declare preclude-all-but [[list symbol] --> [list symbol]])
(declare ps [symbol --> [list unit]])
(declare read-byte [[stream in] --> number])
(declare read-file-as-bytelist [string --> [list number]])
(declare read-file-as-string [string --> string])
(declare read-file [string --> [list unit]])
(declare read-from-string [string --> [list unit]])
(declare remove [A --> [[list A] --> [list A]]]) 
(declare reverse [[list A] --> [list A]]) 
(declare simple-error [string --> A])
(declare snd [[A * B] --> B])
(declare specialise [symbol --> symbol])
(declare spy [symbol --> boolean])
(declare step [symbol --> boolean])
(declare stinput [--> [stream in]])
(declare stoutput [--> [stream out]])
(declare string? [A --> boolean])
(declare str [A --> string])
(declare string->n [string --> number])
(declare string->symbol [string --> symbol])
(declare sum [[list number] --> number])
(declare symbol? [A --> boolean])
(declare systemf [symbol --> [list symbol]])   
(declare tail [[list A] --> [list A]]) 
(declare tlstr [string --> string])
(declare tlv [[vector A] --> [vector A]])
(declare tc [symbol --> boolean]) 
(declare tc? [A --> boolean])
(declare thaw [[lazy A] --> A])
(declare track [symbol --> symbol]) 
(declare trap-error [A --> [[exception --> A] --> A]])
(declare truncate [string --> string])
(declare tuple? [A --> boolean])
(declare undefmacro [symbol --> symbol]) 
(declare union [[list A] --> [[list A] --> [list A]]])
(declare unprofile [[A --> B] --> [A --> B]]) 
(declare untrack [symbol --> symbol])  
(declare unspecialise [symbol --> symbol])
(declare variable? [A --> boolean])
(declare vector? [A --> boolean])
(declare version [string --> string])
(declare write-to-file [string --> [A --> A]])
(declare y-or-n? [string --> boolean])
(declare > [number --> [number --> boolean]])
(declare < [number --> [number --> boolean]])
(declare >= [number --> [number --> boolean]])
(declare <= [number --> [number --> boolean]])
(declare = [A --> [A --> boolean]])
(declare + [number --> [number --> number]])
(declare / [number --> [number --> number]])
(declare - [number --> [number --> number]])
(declare * [number --> [number --> number]])
(declare == [A --> [B --> boolean]]) )



