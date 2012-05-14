(define datatype-error 
  D -> (error "datatype syntax error here:~%~% ~A~%" (next-50 50 D)))

(defcc <datatype-rules>
  <datatype-rule> <datatype-rules> := [<datatype-rule> | <datatype-rules>];
  <e> := [];)

(defcc <datatype-rule>
  <side-conditions> <premises> <singleunderline> <conclusion>
  := (@p single [<side-conditions> <premises> <conclusion>]);
  <side-conditions> <premises> <doubleunderline> <conclusion>
  := (@p double [<side-conditions> <premises> <conclusion>]);)

(defcc <side-conditions>
  <side-condition> <side-conditions> := [<side-condition> | <side-conditions>];
  <e> := [];)

(defcc <side-condition>
  if <expr> := [if <expr>];
  let <variable?> <expr> := [let <variable?> <expr>];)

(defcc <variable?>
  -*- := (if (not (variable? -*-))
             (fail)
             -*-);)

(defcc <expr>
  -*- := (if (or (element? -*- [>> ;]) 
                 (or (singleunderline? -*-) (doubleunderline? -*-)))
             (fail)
              (remove-bar -*-));)

(define remove-bar
  [X B Y] -> [X | Y] where (= B bar!)
  [X | Y] -> [(remove-bar X) | (remove-bar Y)]
  X -> X)

(defcc <premises>
  <premise> <semicolon-symbol> <premises> := [<premise> | <premises>];
  <e> := [];)

(defcc <semicolon-symbol>
  -*- := (if (= -*- ;) skip (fail));)

(defcc <premise>
  ! := !; 
  <formulae> >> <formula> := (@p <formulae> <formula>);
  <formula> := (@p [] <formula>);)

(defcc <conclusion>
  <formulae> >> <formula> <semicolon-symbol> := (@p <formulae> <formula>);
  <formula> <semicolon-symbol> := (@p [] <formula>);)

(defcc <formulae>
   <formula> , <formulae> := [<formula> | <formulae>];
   <formula> := [<formula>];
   <e> := [];)

(defcc <formula>
   <expr> : <type> := [(curry <expr>) : (normalise-type <type>)];
   <expr> := <expr>;)

(defcc <colonsymbol>
  -*- := (if (= -*- ;) -*- (fail));)

(defcc <type>
   <expr> := (curry-type <expr>);)

(defcc <doubleunderline>
  -*- := (if (doubleunderline? -*-)
             -*-
             (fail));)

(defcc <singleunderline> 
  -*- := (if (singleunderline? -*-)
             -*-
             (fail));)

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
  [S P (@p [] C)] -> (let Q (gensym Qv)
                          NewConclusion (@p [C] Q)
                          NewPremises [(@p (map (function right->left) P) Q)]
                          (@p single [S NewPremises NewConclusion])))

(define right->left
  (@p [] C) -> C
  _ -> (error "syntax error with ==========~%")) 

(define rule->horn-clause
  D [S P (@p A C)] -> [(rule->horn-clause-head D C) :- (rule->horn-clause-body S P A)])

(define rule->horn-clause-head
  D C -> [D (mode-ify C) Context_1957])

(define mode-ify
  [X : A] -> [mode [X : [mode A +]] -]  
  X -> X)

(define rule->horn-clause-body
  S P A -> (let Variables (map (function extract_vars) A)
                Predicates (map (/. X (gensym cl)) A)
                SearchLiterals (construct-search-literals 
                                       Predicates Variables Context_1957 Context1_1957)
                SearchClauses (construct-search-clauses Predicates A Variables)
                SideLiterals (construct-side-literals S)
                PremissLiterals (map (/. X (construct-premiss-literal X (empty? A))) P)
                (append SearchLiterals SideLiterals PremissLiterals)))

(define construct-search-literals
  [] [] _ _ -> []
  Predicates Variables Context Context1 
  -> (csl-help Predicates Variables Context Context1))
 
(define csl-help
  [] [] In _ -> [[bind ContextOut_1957 In]]
  [P | Ps] [V | Vs] In Out -> [[P In Out | V] | (csl-help Ps Vs Out (gensym Context))])

(define construct-search-clauses
  [] [] [] -> skip
  [Pred | Preds] [A | As] [V | Vs] -> (do (construct-search-clause Pred A V)
                                          (construct-search-clauses Preds As Vs)))

(define construct-search-clause 
  Pred A V -> (s-prolog [(construct-base-search-clause Pred A V)
                         (construct-recursive-search-clause Pred A V)]))

(define construct-base-search-clause
  Pred A V -> [[Pred [(mode-ify A) | In_1957] In_1957 | V] :- []])

(define construct-recursive-search-clause
  Pred A V -> [[Pred [Assumption_1957 | Assumptions_1957] [Assumption_1957 | Out_1957] | V] 
                 :- [[Pred Assumptions_1957 Out_1957 | V]]])

(define construct-side-literals
  [] -> []
  [[if P] | Sides] -> [[when P] | (construct-side-literals Sides)]
  [[let X Y] | Sides] -> [[is X Y] | (construct-side-literals Sides)]
  [_ | Sides] -> (construct-side-literals Sides))

(define construct-premiss-literal
  (@p A C) Flag -> [t* (recursive_cons_form C) (construct-context Flag A)]
  ! _ -> [cut Throwcontrol])

(define construct-context
  true [] -> Context_1957
  false [] -> ContextOut_1957
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
                    (set Global [X | (value Global)])))                      