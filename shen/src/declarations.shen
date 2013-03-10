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

(set *installing-kl* false)
(set *history* [])
(set *tc* false)
(set *property-vector* (vector 20000))
(set *process-counter* 0)
(set *varcounter* (vector 1000))
(set *prologvectors* (vector 1000))
(set *reader-macros* [])
(set *home-directory* [])
(set *gensym* 0)
(set *tracking* [])
(set *home-directory* "")
(set *alphabet* [A B C D E F G H I J K L M N O P Q R S T U V W X Y Z])
(set *special* [@p @s @v cons lambda let type where set open])  
(set *extraspecial* [define process-datatype input+ defcc])
(set *spy* false)
(set *datatypes* [])
(set *alldatatypes* [])
(set *shen-type-theory-enabled?* true)
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

(define initialise_arity_table
  [] -> []
  [F Arity | Table] -> (let DecArity (put F arity Arity)
                            (initialise_arity_table Table)))

(define arity 
   F -> (trap-error (get F arity) (/. E -1)))
                            
 (initialise_arity_table 
  [adjoin 2 and 2 append 2 arity 1 assoc 2 boolean? 1 cd 1 compile 3 concat 2 cons 2 cons? 1 cn 2 
   declare 2 destroy 1 difference 2 do 2 element? 2 empty? 1 enable-type-theory 1 interror 2 eval 1 
   eval-kl 1 explode 1 external 1 fail-if 2 fail 0 fix 2 findall 5 freeze 1 fst 1 gensym 1 get 3 
   get-time 1 address-> 3 <-address 2 <-vector 2 > 2 >= 2 = 2 hd 1 hdv 1 hdstr 1 head 1 if 3 integer? 1
   identical 4 inferences 0 intersection 2 length 1 lineread 0 load 1 < 2 <= 2 vector 1 macroexpand 1 
   map 2 mapcan 2 maxinferences 1 not 1 nth 2 n->string 1 number? 1 occurs-check 1 occurrences 2 
   occurs-check 1 or 2 package 3 pos 2 print 1 profile 1 profile-results 0 pr 2 ps 1 preclude 1 
   preclude-all-but 1 protect 1 address-> 3 put 4 reassemble 2 read-file-as-string 1 read-file 1 
   read-byte 1 read-from-string 1 remove 2 reverse 1 set 2 simple-error 1 snd 1 specialise 1 spy 1 
   step 1 stinput 0 stoutput 0 string->n 1 string->symbol 1 string? 1 strong-warning 1 subst 3 sum 1
   symbol? 1 tail 1 tl 1 tc 1 tc? 1 thaw 1 track 1 trap-error 2 tuple? 1 type 1 return 3 undefmacro 1
   unprofile 1 unify 4 unify! 4 union 2 untrack 1 unspecialise 1 undefmacro 1 vector 1 vector-> 3 
   value 1 variable? 1 version 1 warn 1 write-to-file 2 y-or-n? 1 + 2 * 2 / 2 - 2 == 2 <1> 1 <e> 1
   @p 2 @v 2 @s 2 preclude 1 include 1 preclude-all-but 1 include-all-but 1 where 2])

(define systemf
  F -> (let Shen (intern "shen")
            External (get Shen external-symbols)
            (put Shen external-symbols (adjoin F External))))

(define adjoin
  X Y -> (if (element? X Y) Y [X | Y]))

(put (intern "shen") external-symbols
     [! } { --> <-- && : ; :- := _ *language* *implementation* *stinput* *home-directory* *version*       *maximum-print-sequence-size* *macros* *os* *release* *property-vector* @v @p @s *port* *porters*
      <- -> <e> == = >= > /. =! $ - / * + <= < >> <> ==> y-or-n? write-to-file where when warn version             verified variable? value vector-> <-vector vector vector? unspecialise untrack unix union unify 
      unify! unprofile undefmacro return type tuple? true trap-error track time thaw tc? tc tl tlstr tlv 
      tail systemf synonyms symbol symbol? string->symbol subst string? string->n stream string stinput 
      stoutput step spy specialise snd simple-error set save str run reverse remove read read-file 
      read-file-as-bytelist read-file-as-string read-byte read-from-string quit put preclude 
      preclude-all-but ps prolog? protect profile-results profile print pr pos package output out or 
      open occurrences occurs-check n->string number? number null nth not nl mode macro macroexpand
      maxinferences mapcan map make-string load loaded list lineread limit length let lazy lambda is 
      intersection inferences intern integer? input input+ include include-all-but if identical head 
      hd hdv hdstr hash get get-time gensym function fst freeze fix file fail fail-if fwhen findall 
      false enable-type-theory explode external exception eval-kl eval error-to-string error empty? 
      element? do difference destroy defun define defmacro defcc defprolog declare datatype cut cn 
      cons? cons cond concat compile cd cases call close bind bound? boolean? boolean bar! assoc arity 
      append and adjoin <-address address-> absvector? absvector abort])

(define specialise
  F -> (do (set *special* [F | (value *special*)]) F))

(define unspecialise
  F -> (do (set *special* (remove F (value *special*))) F)))