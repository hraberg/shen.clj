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

(define load
   FileName -> (let Load (time (load-help (value *tc*) (read-file FileName)))
                    Infs (if (value *tc*)
                             (output "~%typechecked in ~A inferences~%" (inferences))
                             skip)
                    loaded))

(define load-help
  false File -> (map (/. X (output "~S~%" (eval-without-macros X))) File)
  _ File -> (let RemoveSynonyms (mapcan (function remove-synonyms) File)
                 Table (mapcan (function typetable) RemoveSynonyms)
                 Assume (map (function assumetype) Table)
                 (trap-error (map (function typecheck-and-load) RemoveSynonyms) 
                             (/. E (unwind-types E Table)))))
                             
                             
(define remove-synonyms
  [synonyms-help | S] -> (do (eval [synonyms-help | S]) [])
  Code -> [Code])

(define typecheck-and-load
  X -> (do (nl) (typecheck-and-evaluate X (gensym (protect A)))))
                 
(define typetable
  [define F | X] -> (let Sig (compile (function <sig+rest>) X [])
                        (if (= Sig (fail))
                            (error "~A lacks a proper signature.~%" F)
                            [[F | Sig]]))
  [defcc F { [list A] ==> B } | _] -> [[F | [[list A] ==> B]]]
  [defcc F | _] -> (error "~A lacks a proper signature.~%" F)
  _ -> [])

(define assumetype
  [F | Type] -> (declare F Type))

(define unwind-types
  E [] -> (simple-error (error-to-string E))
  E [[F | _] | Table] -> (do (remtype F) (unwind-types E Table)))

(define remtype
  F -> (do (set *signedfuncs* (remove F (value *signedfuncs*))) F))       
                
(defcc <sig+rest>
  <signature> <any> := <signature>;) 
                   
(define write-to-file
   File Text -> (let Stream (open file File out)
                     String (if (string? Text) 
                                (make-string "~A~%~%" Text) 
                                (make-string "~S~%~%" Text))
                     Write (pr String Stream) 
                     Close (close Stream)
                     Text)))

