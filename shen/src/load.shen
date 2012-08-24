(define load
   FileName -> (let Load (time (load-help (value *tc*) (read-file FileName)))
                    Infs (if (value *tc*)
                             (output "~%typechecked in ~A inferences~%" (inferences _))
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
  X -> (do (nl) (typecheck-and-evaluate X (gensym A))))
                 
(define typetable
  [define F | X] -> (let Sig (compile (function <sig+rest>) X [])
                        (if (= Sig (fail))
                            (error "~A lacks a proper signature.~%" F)
                            [[F | Sig]]))
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
   File Text -> (let AbsPath (make-string "~A~A" (value *home-directory*) File)
                     Stream (open file AbsPath out)
                     String (make-string "~S~%~%" Text)
                     Write (pr String Stream) 
                     Close (close Stream)
                     Text))