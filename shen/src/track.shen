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

\Increment the call counter, print the inputs, evaluate the body, store the result,
 print the result and decrement the call counter.\
(define insert-tracking-code
  F Params Body -> [do [set *call* [+ [value *call*] 1]]
                       [do [input-track [value *call*] F (cons_form Params)]
                           [do [terpri-or-read-char]
                        [let Result Body
                             [do [output-track [value *call*] F Result]
                                 [do [set *call* [- [value *call*] 1]]
                                     [do [terpri-or-read-char]
                                         Result]]]]]]])

(set *step* false)

\Sets the trace stepper.\
(define step 
  + -> (set *step* true)
  - -> (set *step* false)
  _ -> (error "step expects a + or a -.~%"))

\Sets the spy stepper.\
(define spy 
  + -> (set *spy* true)
  - -> (set *spy* false)
  _ -> (error "spy expects a + or a -.~%"))

\Forks the printing to a new line or waits for the user if stepping is needed.\
(define terpri-or-read-char
  -> (if (value *step*) 
         (check-byte (read-byte (value *stinput*))) 
         (nl)))

\Abort on request.\
(define check-byte
  C -> (error "aborted")   where (= C (hat))
  _ -> true)

\Prints inputs to function.\
(define input-track
  N F Args
  -> (do (output "~%~A<~A> Inputs to ~A ~%~A" (spaces N) N F (spaces N) Args)
         (recursively-print Args)))

(define recursively-print
  [] -> (output " ==>")
  [X | Y] -> (do (print X) (do (output ", ") (recursively-print Y))))

\Makes the right number of spaces.\
(define spaces
 0 -> ""
 N -> (cn " " (spaces (- N 1))))

\Prints the output of the function.\
(define output-track
  N F Result -> (output "~%~A<~A> Output from ~A ~%~A==> ~S" (spaces N) N F (spaces N) Result))

\Grab the old source code and compile back in again.\
(define untrack
  F -> (eval (ps F)))

\Profile code.\
(define profile
  Func -> (profile-help (ps Func)))

\Grab the source code, and place in profiling code
 Create a copy of the original function that actually
 does the processing.  Then compile the lot to byte code.\ 

(define profile-help
  [defun F Params Code]
   -> (let G (gensym f)
           Profile [defun F Params (profile-func F Params [G | Params])]
           Def [defun G Params (subst G F Code)]
           CompileProfile (eval-without-macros Profile)
           CompileG (eval-without-macros Def)
           F)
  _ -> (error "Cannot profile.~%"))

\Reinstate the original code.\
(define unprofile
   Func -> (untrack Func))

\Insert profiling code.\
(define profile-func 
  F Params Code -> [let Start [get-time run]
                     [let Result Code
                       [let Finish [- [get-time run] Start]
                         [let Record 
                              [put-profile F [+ [get-profile F] Finish]]
                              Result]]]])

\Print the profile results.\
(define profile-results 
   F -> (let Results (get-profile F) 
             Initialise (put-profile F 0)
             (@p F Results)))
             
(define get-profile
  F -> (trap-error (get F profile) (/. E 0)))
  
(define put-profile
  F Time -> (put F profile Time))