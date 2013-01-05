(define print 
  X -> (do (pr (ms-h ["~" "S"] (@p X skip)) (stoutput 0)) X))

(define intoutput
  String Args -> (pr (ms-h (explode-string String) Args) (stoutput 0)))

(define interror
  String Args -> (simple-error (ms-h (explode-string String) Args))) 

(define intmake-string
  String Args -> (ms-h (explode-string String) Args)) 

(define ms-h
  [] _ -> ""
  ["~" "%" | Cs] Args -> (cn (n->string 10) (ms-h Cs Args))                       
  ["~" C | Cs] (@p Arg Args) -> (cn (ob->str C Arg) (ms-h Cs Args))
                                       where (element? C ["A" "S" "R"])
  [C | Cs] Args -> (cn C (ms-h Cs Args)))

(define ob->str
  _ X -> "..."   where (= X (fail))
  C [] -> (if (= C "R") "()" "[]")
  C V ->  "<>"	where (= V (vector 0))
  C [X | Y] -> (cn-all (append (if (= C "R") ["("] ["["]) 
                                 [(ob->str C X)]  
                                 (xmapcan (value *maximum-print-sequence-size*)  
                                          (/. Z [" " (ob->str C Z)]) Y) 
                                 (if (= C "R") [")"] ["]"]) ))
  C X -> (let L (vector->list X 1)
              E (tlstr (cn-all (xmapcan (- (value *maximum-print-sequence-size*) 1)
                                        (/. Z [" " (ob->str C Z)]) L)))
              V (cn "<" (cn E ">"))
              V)   				where (vector? X)
  C X -> (trap-error (ob->str "A" ((<-address X 0) X))
                     (/. Ignore (let L (vector->list X 0)
                                     E (tlstr (cn-all 
                                        (xmapcan 
                                         (- (value *maximum-print-sequence-size*) 1) 
                                           (/. Z [" " (ob->str C Z)]) L)))
                                     V (cn "<" (cn E ">"))
                                     V)))  where (and (not (string? X)) (absvector? X)) 
  C X -> (if (and (= C "A") (string? X))
              X
              (str X)))

(define tuple
  X -> (make-string "(@p ~S ~S)" (fst X) (snd X)))              
              
(define cn-all
  [] -> ""
  [X | Y] -> (cn X (cn-all Y)))

(define xmapcan
  _ _ [] -> []
  0 _ _ -> ["... etc"]
  Max F [X | Y] -> (append (F X) (xmapcan (- Max 1) F Y))
  _ F X -> [" |" | (F X)])

(define vector->list
  X N -> (vector->listh X N []))

(define vector->listh
  X N L -> (let Y (trap-error (<-address X N) (/. E out-of-range))
              (if (= Y out-of-range)
                  (reverse L)
                  (vector->listh X (+ N 1) [Y | L]))))
