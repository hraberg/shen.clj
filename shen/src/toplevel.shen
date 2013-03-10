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

(define shen 
  -> (do (credits) (loop)))

(define loop
   -> (do (initialise_environment)
          (prompt) 
          (trap-error (read-evaluate-print) (/. E (pr (error-to-string E) (stoutput)))) 
          (loop)))

(define version
  S -> (set *version* S))

(version "version 9")

(define credits
 -> (do (output "~%Shen 2010, copyright (C) 2010 Mark Tarver~%")
        (output "www.shenlanguage.org, ~A~%" (value *version*)) 
        (output "running under ~A, implementation: ~A" (value *language*) (value *implementation*))
        (output "~%port ~A ported by ~A~%" (value *port*) (value *porters*))))

(define initialise_environment 
  -> (multiple-set [*call* 0 *infs* 0 *process-counter* 0 *catch* 0]))  

(define multiple-set
  [] -> []
  [S V | M] -> (do (set S V) (multiple-set M)))
                 
(define destroy
  F -> (declare F []))

(set *history* [])

(define read-evaluate-print 
  -> (let Lineread (toplineread)  
          History (value *history*)
          NewLineread (retrieve-from-history-if-needed Lineread History)
          NewHistory (update_history NewLineread History)
          Parsed (fst NewLineread)         
          (toplevel Parsed)))

(define retrieve-from-history-if-needed
   (@p _ [C1 C2]) [H | _] -> (let PastPrint (prbytes (snd H))
                                    H)  where (and (= C1 (exclamation)) (= C2 (exclamation)))
   (@p _ [C | Key]) H -> (let Key? (make-key Key H)
                                Find (head (find-past-inputs Key? H)) 
                                PastPrint (prbytes (snd Find))
                                Find)   where (= C (exclamation))
   (@p _ [C]) H -> (do (print-past-inputs (/. X true) (reverse H) 0)
                         (abort))       where (= C (percent))
   (@p _ [C | Key]) H -> (let Key? (make-key Key H)
                                Pastprint (print-past-inputs Key? (reverse H) 0)
                                (abort))  where (= C (percent))
   Lineread _ -> Lineread)

(define percent
  -> 37)

(define exclamation
  ->  33) 

(define prbytes
  Bytes -> (do (map (/. Byte (pr (n->string Byte) (stoutput))) Bytes) 
               (nl)))

(define update_history 
  Lineread History -> (set *history* [Lineread  | History]))   

(define toplineread
  -> (toplineread_loop (read-byte (stinput)) []))

(define toplineread_loop
  Byte _ -> (error "line read aborted")  where (= Byte (hat))
  Byte Bytes -> (let Line (compile (function <st_input>) Bytes (/. E nextline))
                    (if (or (= Line nextline) (empty? Line))
                        (toplineread_loop (read-byte (stinput)) (append Bytes [Byte]))
                        (@p Line Bytes)))
                            	where (element? Byte [(newline) (carriage-return)])
  Byte Bytes -> (toplineread_loop (read-byte (stinput)) (append Bytes [Byte])))

(define hat
  -> 94)

(define newline
   -> 10)
     
(define carriage-return
    -> 13)    
  
(define tc
  + -> (set *tc* true)
  - -> (set *tc* false)
  _ -> (error "tc expects a + or -"))

(define prompt
  -> (if (value *tc*)
         (output  "~%~%(~A+) " (length (value *history*)))
         (output  "~%~%(~A-) " (length (value *history*)))))

(define toplevel
  Parsed -> (toplevel_evaluate Parsed (value *tc*)))

(define find-past-inputs
  Key? H -> (let F (find Key? H) 
              (if (empty? F) 
                  (error "input not found~%")
                  F)))

(define make-key
  Key H -> (let Atom (hd (compile (function <st_input>) Key))
              (if (integer? Atom)
                  (/. X (= X (nth (+ Atom 1) (reverse H))))
                  (/. X (prefix? Key (trim-gubbins (snd X)))))))

(define trim-gubbins
  [C | X] -> (trim-gubbins X)  where (= C (space))
  [C | X] -> (trim-gubbins X)  where (= C (newline))
  [C | X] -> (trim-gubbins X)  where (= C (carriage-return))
  [C | X] -> (trim-gubbins X)  where (= C (tab))
  [C | X] -> (trim-gubbins X)  where (= C (left-round))
  X -> X)
  
(define space
   -> 32)  
 
(define tab
   -> 9)

(define left-round
  -> 40)

(define find
  _ [] -> []
  F [X | Y] -> [X | (find F Y)]	where (F X)
  F [_ | Y] -> (find F Y))

(define prefix?
  [] _ -> true
  [X | Y] [X | Z] -> (prefix? Y Z)
  _ _ -> false)

(define print-past-inputs
  _ [] _ -> _
  Key? [H | Hs] N -> (print-past-inputs Key? Hs (+ N 1)) 	where (not (Key? H))
  Key? [(@p _ Cs) | Hs] N -> (do (output "~A. " N) 
                                 (do (prbytes Cs) 
                                     (print-past-inputs Key? Hs (+ N 1)))))
                                 
(define toplevel_evaluate
  [X : A] true -> (typecheck-and-evaluate X A)  
  [X Y | Z] Boolean -> (do (toplevel_evaluate [X] Boolean)
                            (nl)
                            (toplevel_evaluate [Y | Z] Boolean))
  [X] true -> (typecheck-and-evaluate X (gensym (protect A)))
  [X] false -> (let Eval (eval-without-macros X)
                    (print Eval)))

(define typecheck-and-evaluate
  X A -> (let Typecheck (typecheck X A)
              (if (= Typecheck false)
                  (error "type error~%")
                  (let Eval (eval-without-macros X)
                       Type (pretty-type Typecheck)
                       (output "~S : ~R" Eval Type)))))

(define pretty-type
  Type -> (mult_subst (value *alphabet*) (extract-pvars Type) Type))

(define extract-pvars
  X -> [X]  where (pvar? X)
  [X | Y] -> (union (extract-pvars X) (extract-pvars Y))
  _ -> [])

(define mult_subst
  [] _ X -> X
  _ [] X -> X
  [X | Y] [W | Z] A -> (mult_subst Y Z (subst X W A))))