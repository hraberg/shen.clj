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

(define f_error 
  F -> (do (output "partial function ~A;~%" F)
           (if (and (not (tracked? F))
                    (y-or-n? (make-string "track ~A? " F)))
               (track-function (ps F))
               ok)
           (simple-error "aborted")))

(define tracked?
  F -> (element? F (value *tracking*)))

(define track
  F -> (let Source (ps F)
            (track-function Source)))

(define track-function
  [defun F Params Body]
   -> (let KL [defun F Params (insert-tracking-code F Params Body)]
           Ob (eval KL)
           Tr (set *tracking* [Ob | (value *tracking*)])
           Ob))

(define insert-tracking-code
  F Params Body -> [do [set *call* [+ [value *call*] 1]]
                       [do [input-track [value *call*] F (cons_form Params)]
                           [do [terpri-or-read-char]
                        [let (protect Result) Body
                             [do [output-track [value *call*] F (protect Result)]
                                 [do [set *call* [- [value *call*] 1]]
                                     [do [terpri-or-read-char]
                                         (protect Result)]]]]]]])

(set *step* false)

(define step 
  + -> (set *step* true)
  - -> (set *step* false)
  _ -> (error "step expects a + or a -.~%"))

(define spy 
  + -> (set *spy* true)
  - -> (set *spy* false)
  _ -> (error "spy expects a + or a -.~%"))

(define terpri-or-read-char
  -> (if (value *step*) 
         (check-byte (read-byte (value *stinput*))) 
         (nl)))

(define check-byte
  C -> (error "aborted")   where (= C (hat))
  _ -> true)

(define input-track
  N F Args
  -> (do (output "~%~A<~A> Inputs to ~A ~%~A" (spaces N) N F (spaces N) Args)
         (recursively-print Args)))

(define recursively-print
  [] -> (output " ==>")
  [X | Y] -> (do (print X) (do (output ", ") (recursively-print Y))))

(define spaces
 0 -> ""
 N -> (cn " " (spaces (- N 1))))

(define output-track
  N F Result -> (output "~%~A<~A> Output from ~A ~%~A==> ~S" (spaces N) N F (spaces N) Result))

(define untrack
  F -> (eval (ps F)))

(define profile
  Func -> (profile-help (ps Func)))

(define profile-help
  [defun F Params Code]
   -> (let G (gensym f)
           Profile [defun F Params (profile-func F Params [G | Params])]
           Def [defun G Params (subst G F Code)]
           CompileProfile (eval-without-macros Profile)
           CompileG (eval-without-macros Def)
           F)
  _ -> (error "Cannot profile.~%"))

(define unprofile
   Func -> (untrack Func))

(define profile-func 
  F Params Code -> [let (protect Start) [get-time run]
                     [let (protect Result) Code
                       [let (protect Finish) [- [get-time run] (protect Start)]
                         [let (protect Record) 
                              [put-profile F [+ [get-profile F] (protect Finish)]]
                              (protect Result)]]]])

(define profile-results 
   F -> (let Results (get-profile F) 
             Initialise (put-profile F 0)
             (@p F Results)))
             
(define get-profile
  F -> (trap-error (get F profile) (/. E 0)))
  
(define put-profile
  F Time -> (put F profile Time)))