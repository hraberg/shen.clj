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

(define datatype-error 
  [D _] -> (error "datatype syntax error here:~%~% ~A~%" (next-50 50 D)))

(defcc <datatype-rules>
  <datatype-rule> <datatype-rules> := [<datatype-rule> | <datatype-rules>];
  <e> := [];)

(defcc <datatype-rule>
  <side-conditions> <premises> <singleunderline> <conclusion>
  := (sequent single [<side-conditions> <premises> <conclusion>]);
  <side-conditions> <premises> <doubleunderline> <conclusion>
  := (sequent double [<side-conditions> <premises> <conclusion>]);)

(defcc <side-conditions>
  <side-condition> <side-conditions> := [<side-condition> | <side-conditions>];
  <e> := [];)

(defcc <side-condition>
  if <expr> := [if <expr>];
  let <variable?> <expr> := [let <variable?> <expr>];)

(defcc <variable?>
  X := X	where (variable? X);)

(defcc <expr>
  X := (remove-bar X) where (not (or (element? X [>> ;]) 
                                     (singleunderline? X) 
                                     (doubleunderline? X)));)

(define remove-bar
  [X B Y] -> [X | Y] where (= B bar!)
  [X | Y] -> [(remove-bar X) | (remove-bar Y)]
  X -> X)

(defcc <premises>
  <premise> <semicolon-symbol> <premises> := [<premise> | <premises>];
  <e> := [];)

(defcc <semicolon-symbol>
  X := skip	where (= X ;);)

(defcc <premise>
  ! := !; 
  <formulae> >> <formula> := (sequent <formulae> <formula>);
  <formula> := (sequent [] <formula>);)

(defcc <conclusion>
  <formulae> >> <formula> <semicolon-symbol> := (sequent <formulae> <formula>);
  <formula> <semicolon-symbol> := (sequent [] <formula>);)

(define sequent
  Formulae Formula -> (@p Formulae Formula))

(defcc <formulae>
   <formula> <comma-symbol> <formulae> := [<formula> | <formulae>];
   <formula> := [<formula>];
   <e> := [];)

(defcc <comma-symbol>
  X := skip 	where (= X (intern ","));)

(defcc <formula>
   <expr> : <type> := [(curry <expr>) : (normalise-type <type>)];
   <expr> := <expr>;)

(defcc <type>
   <expr> := (curry-type <expr>);)

(defcc <doubleunderline>
  X := X	where (doubleunderline? X);)

(defcc <singleunderline> 
  X := X	where (singleunderline? X);)

(define singleunderline?
  S -> (and (symbol? S) (sh? (str S))))

(define sh?
  "_" -> true
  S -> (and (= (pos S 0) "_") (sh? (tlstr S))))
            
(define doubleunderline?
  S -> (and (symbol? S) (dh? (str S))))

(define dh?
  "=" -> true
  S -> (and (= (pos S 0) "=") (dh? (tlstr S))))

(define process-datatype 
  D Rules -> (remember-datatype (s-prolog (rules->horn-clauses D Rules))))

(define remember-datatype 
  [D | _] -> (do (set *datatypes* (adjoin D (value *datatypes*)))
                  (set *alldatatypes* (adjoin D (value *alldatatypes*))) 
                      D))

(define rules->horn-clauses
   _ [] -> []
   D [(@p single Rule) | Rules] 
    -> [(rule->horn-clause D Rule) | (rules->horn-clauses D Rules)]
   D [(@p double Rule) | Rules] 
   -> (rules->horn-clauses D (append (double->singles Rule) Rules)))

(define double->singles
  Rule -> [(right-rule Rule) (left-rule Rule)])

(define right-rule
  Rule -> (@p single Rule))

(define left-rule
  [S P (@p [] C)] -> (let Q (gensym (protect Qv))
                          NewConclusion (@p [C] Q)
                          NewPremises [(@p (map (function right->left) P) Q)]
                          (@p single [S NewPremises NewConclusion])))

(define right->left
  (@p [] C) -> C
  _ -> (error "syntax error with ==========~%")) 

(define rule->horn-clause
  D [S P (@p A C)] -> [(rule->horn-clause-head D C) :- (rule->horn-clause-body S P A)])

(define rule->horn-clause-head
  D C -> [D (mode-ify C) (protect Context_1957)])

(define mode-ify
  [X : A] -> [mode [X : [mode A +]] -]  
  X -> X)

(define rule->horn-clause-body
  S P A -> (let Variables (map (function extract_vars) A)
                Predicates (map (/. X (gensym cl)) A)
                SearchLiterals (construct-search-literals 
                                       Predicates Variables (protect Context_1957) (protect Context1_1957))
                SearchClauses (construct-search-clauses Predicates A Variables)
                SideLiterals (construct-side-literals S)
                PremissLiterals (map (/. X (construct-premiss-literal X (empty? A))) P)
                (append SearchLiterals SideLiterals PremissLiterals)))

(define construct-search-literals
  [] [] _ _ -> []
  Predicates Variables Context Context1 
  -> (csl-help Predicates Variables Context Context1))
 
(define csl-help
  [] [] In _ -> [[bind (protect ContextOut_1957) In]]
  [P | Ps] [V | Vs] In Out -> [[P In Out | V] | (csl-help Ps Vs Out (gensym (protect Context)))])

(define construct-search-clauses
  [] [] [] -> skip
  [Pred | Preds] [A | As] [V | Vs] -> (do (construct-search-clause Pred A V)
                                          (construct-search-clauses Preds As Vs)))

(define construct-search-clause 
  Pred A V -> (s-prolog [(construct-base-search-clause Pred A V)
                         (construct-recursive-search-clause Pred A V)]))

(define construct-base-search-clause
  Pred A V -> [[Pred [(mode-ify A) | (protect In_1957)] (protect In_1957) | V] :- []])

(define construct-recursive-search-clause
  Pred A V -> [[Pred [(protect Assumption_1957) | (protect Assumptions_1957)] [(protect Assumption_1957) | (protect Out_1957)] | V] 
                 :- [[Pred (protect Assumptions_1957) (protect Out_1957) | V]]])

(define construct-side-literals
  [] -> []
  [[if P] | Sides] -> [[when P] | (construct-side-literals Sides)]
  [[let X Y] | Sides] -> [[is X Y] | (construct-side-literals Sides)]
  [_ | Sides] -> (construct-side-literals Sides))

(define construct-premiss-literal
  (@p A C) Flag -> [t* (recursive_cons_form C) (construct-context Flag A)]
  ! _ -> [cut (protect Throwcontrol)])

(define construct-context
  true [] -> (protect Context_1957)
  false [] -> (protect ContextOut_1957)
  Flag [X | Y] -> [cons (recursive_cons_form X) (construct-context Flag Y)])

(define recursive_cons_form
  [X | Y] -> [cons (recursive_cons_form X) (recursive_cons_form Y)]
  X -> X) 

(define preclude
   Types -> (let FilterDatatypes (set *datatypes* (difference (value *datatypes*) Types))
                 (value *datatypes*)))
             
(define include
   Types -> (let ValidTypes (intersection Types (value *alldatatypes*))
                 NewDatatypes (set *datatypes* (union ValidTypes (value *datatypes*)))
                 (value *datatypes*)))

(define preclude-all-but
  Types -> (preclude (difference (value *alldatatypes*) Types)))

(define include-all-but
  Types -> (include (difference (value *alldatatypes*) Types)))

(define synonyms-help
  [] -> synonyms
  [S1 S2 | S] -> (do (pushnew [S1 | S2] *synonyms*)
                     (synonyms-help S))
  _ -> (error "odd number of synonyms~%" []))
  
(define pushnew
   X Global -> (if (element? X (value Global))
                    (value Global)
                    (set Global [X | (value Global)])))           )           