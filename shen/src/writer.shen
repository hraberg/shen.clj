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

(define print 
  X -> (let String (insert X "~S")
            Print (pr String (stoutput))
            X)) 

(define mkstr
  String Args -> (mkstr-l (proc-nl String) Args)   where (string? String)
  String Args -> (mkstr-r [proc-nl String] Args))

(define mkstr-l
  String [] -> String  
  String [Arg | Args] -> (mkstr-l (insert-l Arg String) Args))

(define insert-l
  _ "" -> ""
  Arg (@s "~A" S) -> [app Arg S a]
  Arg (@s "~R" S) -> [app Arg S r]
  Arg (@s "~S" S) -> [app Arg S s]
  Arg (@s S Ss) -> (factor-cn [cn S (insert-l Arg Ss)])
  Arg [cn S Ss] -> [cn S (insert-l Arg Ss)]
  Arg [app S Ss Mode] -> [app S (insert-l Arg Ss) Mode])

(define factor-cn
  [cn S1 [cn S2 S3]] -> [cn (cn S1 S2) S3]  where (and (string? S1) (string? S2))
  Cn -> Cn)
  
(define proc-nl
 "" -> ""
 (@s "~%" Ss) -> (cn "c#10;" (proc-nl Ss))
 (@s S Ss) -> (cn S (proc-nl Ss)))

(define mkstr-r
  String [] -> String  
  String [Arg | Args] -> (mkstr-r [insert Arg String] Args))

(define insert
  _ "" -> ""
  Arg (@s "~A" S) -> (app Arg S a)
  Arg (@s "~R" S) -> (app Arg S r)
  Arg (@s "~S" S) -> (app Arg S s)
  Arg (@s S Ss) -> (cn S (insert Arg Ss)))
  
(define app
  Arg String Mode -> (cn (arg->str Arg Mode) String))
  
(define arg->str
  F _ -> "..."	   		  where (= F (fail))
  L Mode -> (list->str L Mode)    where (list? L)  		
  S Mode -> (str->str S Mode)  	  where (string? S)
  V Mode -> (vector->str V Mode)  where (absvector? V)
  At _ -> (atom->str At))
   
(define list->str
  L r -> (@s "(" (iter-list L r (maxseq)) ")")
  L Mode -> (@s "[" (iter-list L Mode (maxseq)) "]"))

(define maxseq
  -> (value *maximum-print-sequence-size*))
  
(define iter-list
  [] _ _ -> ""
  _ _ 0 -> "... etc"
  [X] Mode _ -> (arg->str X Mode)
  [X | Y] Mode N -> (@s (arg->str X Mode) " " (iter-list Y Mode (- N 1)))
  X Mode N -> (@s " | " (arg->str X Mode)))
  
(define str->str
  S a -> S
  S _ -> (@s "c#34;" S "c#34;"))
  
(define vector->str
  V Mode -> (cases (print-vector? V) ((<-address V 
0) V)
                   (vector? V) (@s "<" (iter-vector V 1 Mode (maxseq)) ">")
                   true (@s "<<" (iter-vector V 0 Mode (maxseq)) ">>")))
              
(define print-vector?
  P -> (let Zero (<-address P 0)
                 (cases (= Zero tuple) true
                        (= Zero pvar) true
                        (not (number? Zero)) (fbound? Zero)
                        true false))) 
                        
(define fbound?
  F -> (trap-error (do (ps F) true) (/. E false)))

(define tuple
  P -> (make-string "(@p ~S ~S)" (<-address P 1) (<-address P 2)))
              
(define iter-vector
  _ _ _ 0 -> "... etc"
  V N Mode Max -> (let Item (trap-error (<-address V N) (/. E out-of-bounds))
                       Next (trap-error (<-address V (+ N 1)) (/. E out-of-bounds))
                       (cases (= Item out-of-bounds) ""
                              (= Next out-of-bounds) (arg->str Item Mode)
                              true (@s (arg->str Item Mode)
                                       " "
                                       (iter-vector V (+ N 1) Mode (- Max 1))))))
  
(define atom->str
  At -> (trap-error (str At) (/. E (funexstring))))
  
(define funexstring
  -> (@s "c#16;fune" (arg->str (gensym (intern "x")) a) "c#17;"))

(define list?
  X -> (or (empty? X) (cons? X)))		)