(benchmark "read a 10K binary file" (read-file-as-bytelist "plato.jpg"))

(benchmark "read a 105K binary file" (read-file-as-bytelist "heatwave.gif"))

(benchmark "parse a 7K Shen file" (read-file "interpreter.shen"))

(benchmark "compile a 130 LOC Qi program" (load "short.shen"))

(benchmark "compile a 27 line Prolog program" (load "einstein.shen"))

(benchmark "solve Einstein's puzzle" (prolog? (einsteins_riddle X) (return X)))

(load "powerset.shen")

(benchmark "powerset of 14 numbers" (powerset [1 2 3 4 5 6 7 8 9 10 11 12 13 14]))

(do (set *str* (hd (read-file "text.txt"))) ok)

(define remstr
  "" -> 0
  (@s "er" S) -> (+ 1 (remstr S))
  (@s _ Ss) -> (remstr Ss))

(benchmark "count 'er' in a string" (remstr (value *str*)))

(define vectorn
  0 -> <>
  N -> (@v N (vectorn (- N 1))))
  
(define vectorp
  <> -> <>
  (@v X Y) -> (@v (+ X 1) (vectorp Y))
  (@v X Y Z) -> (@v (+ X 1) (+ Y 2) (vectorp Z)))
    
(benchmark "vector of 1000 elements" (vectorn 1000))

(define tak
  X Y Z -> Z   where (not (< Y X))
  X Y Z -> (tak (tak (- X 1) Y Z)
                (tak (- Y 1) Z X)
                (tak (- Z 1) X Y)))

(benchmark "(tak 18 12 6)" (tak 18 12 6))

(benchmark "compile 10 line YACC program for paren checking" (load "br.shen")) 

(benchmark "paren check a 2000 line program" (compile <br> (read-file-as-bytelist "bigprog")))

(tc +)

(benchmark "type checking the N queens" (load "N queens.shen"))

(benchmark "solving the N queens for N = 6" (n-queens 6))

(benchmark "load and typecheck Qi interpreter" (load "interpreter.shen"))
  
