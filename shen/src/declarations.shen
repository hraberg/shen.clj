(set *installing-kl* false)
(set *history* [])
(set *tc* false)
(set *property-vector* (vector 20000))
(set *process-counter* 0)
(set *varcounter* (vector 1000))
(set *prologvectors* (vector 1000))
(set *reader-macros* [])
(set *printer* [])
(set *home-directory* [])
(set *gensym* 0)
(set *tracking* [])
(set *home-directory* "")
(set *alphabet* [A B C D E F G H I J K L M N O P Q R S T U V W X Y Z])
(set *special* [@p @s @v cons lambda let type where set open])  
(set *extraspecial* [define process-datatype input+])
(set *spy* false)
(set *datatypes* [])
(set *alldatatypes* [])
(set *synonyms* [])
(set *system* [])
(set *signedfuncs* []) 
(set *maxcomplexity* 128)
(set *occurs* true)
(set *maxinferences* 1000000)
(set *maximum-print-sequence-size* 20)
(set *catch* 0)
(set *call* 0)
(set *infs* 0)
(set *process-counter* 0)
(set *catch* 0)

(define initialise_arity_table
  [] -> []
  [F Arity | Table] -> (let DecArity (put F arity Arity)
                            (initialise_arity_table Table)))

(define arity 
   F -> (trap-error (get F arity) (/. E -1)))
                            
 (initialise_arity_table 
  [adjoin 2 and 2 append 2 arity 1 assoc 2 boolean? 1 cd 1 compile 3 concat 2 cons 2 
   cons? 1 cn 2 declare 2 destroy 1 difference 2 do 2 element? 2 empty? 1 interror 2 eval 1 eval-kl 1 explode 1 
   external 1 fail-if 2 fail 0 fix 2 findall 5 freeze 1 fst 1 gensym 1 get 3 address-> 3 <-address 2 <-vector 2 > 2 
   >= 2 = 2 hd 1 hdv 1 hdstr 1 head 1 if 3 integer? 1 identical 4 inferences 1 intoutput 2 make-string 2
   intersection 2 length 1 lineread 0 load 1 < 2 <= 2 vector 1 macroexpand 1 map 2 mapcan 2 intmake-string 2
   maxinferences 1 not 1 nth 2 n->string 1 number? 1 output 2 occurs-check 1 occurrences 2 occurs-check 1 or 2 
   package 3 pos 2 print 1 profile 1 profile-results 1 ps 1 preclude 1 preclude-all-but 1 protect 1 address-> 3 put 4 reassemble 2 read-file-as-string 1 read-file 1 read-byte 1 remove 2 reverse 1 set 2 simple-error 1 snd 1 specialise 1 
   spy 1 step 1 stinput 1 stoutput 1 string->n 1 string? 1 strong-warning 1 subst 3 symbol? 1 tail 1 tl 1 tc 1 tc? 1 thaw 1 
   track 1 trap-error 2 tuple? 1 type 1 return 3 unprofile 1 unify 4 unify! 4 union 2 untrack 1 unspecialise 1 vector 1 
   vector-> 3 value 1 variable? 1 version 1 warn 1 write-to-file 2 y-or-n? 1 + 2 * 2 / 2 - 2 == 2 <1> 1 <e> 1
   @p 2 @v 2 @s 2 preclude 1 include 1 preclude-all-but 1 include-all-but 1 where 2])

(define systemf
  F -> (set *system* (adjoin F (value *system*))))

(define adjoin
  X Y -> (if (element? X Y) Y [X | Y]))

(map (function systemf) 
     [! } { --> <-- && : ; :- := (intern "_") <!> -*- *language* *implementation* *stinput*
      *home-directory* *version* *maximum-print-sequence-size* *printer* *macros* *os* *release* @v @p @s 
      <- -> <e> == = >= > /. =! $ - / * + <= < >> <> y-or-n? write-to-file where when warn version verified 
      variable? value vector-> <-vector vector vector? unspecialise untrack union unify unify! unprofile 
      return type tuple? true trap-error track time thaw tc? tc tl tlstr tlv tail systemf synonyms symbol symbol? 
      sum subst string? string->n stream string stinput stoutput step spy specialise snd simple-error set save str run 
      reverse remove read read-file read-file-as-bytelist read-file-as-string read-byte quit put preclude 
      preclude-all-but ps prolog? protect profile-results profile print pr pos package output out or open occurrences 
      occurs-check n->string number? number null nth not nl mode macro macroexpand maxinferences mapcan map make-string 
      load loaded list lineread limit length let lazy lambda is intersection inferences intern integer? input input+ 
      include include-all-but in if identical head hd hdv hdstr hash get get-time gensym function fst freeze format 
      fix file fail fail-if fwhen findall false explode external exception eval-kl eval error-to-string error 
      empty? element? do difference destroy defun define defmacro defcc defprolog declare datatype cut 
      cn cons? cons cond concat compile cd cases call close bind bound? boolean? boolean bar! assoc arity append and 
      adjoin <-address address-> absvector? absvector abort intmake-string intoutput interror])

(define specialise
  F -> (do (set *special* [F | (value *special*)]) F))

(define unspecialise
  F -> (do (set *special* (remove F (value *special*))) F))