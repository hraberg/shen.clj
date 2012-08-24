(define thaw 
  F -> (F))

(define eval
  X -> (let Macroexpand (walk (function macroexpand) X)
            (if (packaged? Macroexpand)
                (map (function eval-without-macros) (package-contents Macroexpand))
                (eval-without-macros Macroexpand))))

(define eval-without-macros 
  X -> (eval-kl (elim-define (proc-input+ X)))) 

(define proc-input+ 
  [input+ Colon Type] -> [input+ Colon (rcons_form Type)]
  [X | Y] -> (map (function proc-input+) [X | Y])
  X -> X) 
  
(define elim-define
  [define F | Rest] -> (shen->kl F Rest)
  [X | Y] -> (map (function elim-define) [X | Y])
  X -> X)
                
(define packaged?
  [package P E | _] -> true
  _ -> false)

(define external
  Package -> (trap-error (get Package external-symbols) (/. E (error "package ~A has not been used.~"))))
  
(define package-contents
  [package null _ | Contents] -> Contents
  [package P E | Contents] -> (packageh P E Code))
              
(define walk
  F [X | Y] -> (F (map (/. Z (walk F Z)) [X | Y]))
  F X -> (F X))              

(define compile
   F X Err -> (let O (F (@p X []))
                   (if (or (= (fail) O) (not (empty? (fst O))))
                       (compile-error O Err)
                       (snd O))))

(define compile-error
  _ [] -> (fail)
  (@p [X | Y] _) Err -> (Err [X | Y])
  _ _ -> (error "syntax error~%")) 

(define <e>
  (@p X _) -> (@p X []))

(define fail-if
  F X -> (if (F X) (fail) X))  

(define @s
  X Y -> (cn X Y))

(define tc?
  _ -> (value *tc*))
 
(define ps
  Name -> (trap-error (get Name source) (/. E (error "~A not found.~%" Name))))

(define explode 
  X -> (if (string? X) 
           (explode-string X)
           (explode (make-string "~A" X))))

(define explode-string 
  "" -> []
  String -> (let S (pos String 0)
                 Ss (tlstr String)
                 (if (= Ss eos)
                     []
                     [S | (explode-string Ss)])))  

(define stinput 
  _ -> (value *stinput*))

(define +vector? 
 X -> (and (absvector? X) (> (<-address X 0) 0)))

(define vector
   N -> (let Vector (absvector (+ N 1))
             ZeroStamp (address-> Vector 0 N)
             Standard (if (= N 0) ZeroStamp (fillvector ZeroStamp 1 N (fail)))
             Standard))
                            
(define fillvector 
  Vector N N X -> (address-> Vector N X)
  Vector Counter N X -> (fillvector (address-> Vector Counter X) (+ 1 Counter) N X))

(define vector? 
  X -> (and (absvector? X) (trap-error (>= (<-address X 0) 0) (/. E false))))

(define vector-> 
  Vector N X -> (if (= N 0) 
                    (error "cannot access 0th element of a vector~%")
                    (address-> Vector N X)))

(define <-vector 
  Vector N -> (if (= N 0) 
                  (error "cannot access 0th element of a vector~%")
                  (let VectorElement (<-address Vector N)
                      (if (= VectorElement (fail))
                          (error "vector element not found~%")
                          VectorElement))))

(define posint? 
  X -> (and (integer? X) (>= X 0)))

(define limit 
  Vector -> (<-address Vector 0))

(define symbol?
  X -> false where (or (boolean? X) (number? X) (string? X))
  X -> (trap-error (let Explode (explode X)
                        (analyse-symbol? Explode)) (/. E false)))

(define analyse-symbol?
  [S | Ss] -> (and (alpha? S)
                   (alphanums? Ss)))

(define alpha?
  S ->  (element? S ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                     "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" 
                     "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                     "=" "*" "/" "+" "-" "_" "?" "$" "!" "@" "~" ">" "<" 
                     "&" "%" "{" "}" ":" ";" "`" "#" "'" "."])) 
                     
(define alphanums?
  [] -> true
  [S | Ss] -> (and (alphanum? S) (alphanums? Ss)))

(define alphanum?
  S -> (or (alpha? S) (digit? S)))

(define digit?
  S -> (element? S ["1" "2" "3" "4" "5" "6" "7" "8" "9" "0"]))
                              
(define variable?
  X -> false where (or (boolean? X) (number? X) (string? X))
  X -> (trap-error (let Explode (explode X)
                        (analyse-variable? Explode)) (/. E false)))

(define analyse-variable?
  [S | Ss] -> (and (uppercase? S)
                   (alphanums? Ss)))

(define uppercase?
  S ->  (element? S ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]))   
                      
(define gensym
  Sym -> (concat Sym (set *gensym* (+ 1 (value *gensym*)))))
  
(define concat
  S1 S2 -> (intern (cn (str S1) (str S2))))  

(define @p 
  X Y -> (let Vector (absvector 3)
              Tag (address-> Vector 0 tuple)
              Fst (address-> Vector 1 X)
              Snd (address-> Vector 2 Y)
              Vector))

(define fst 
  X -> (<-address X 1))
           
(define snd 
  X -> (<-address X 2))

(define tuple? 
  X -> (trap-error (and (absvector? X) (= tuple (<-address X 0))) (/. E false)))

(define append
  [] X -> X
  [X | Y] Z -> [X | (append Y Z)])

(define @v
  X Vector -> (let Limit (limit Vector)
                   NewVector (vector (+ Limit 1))
                   X+NewVector (vector-> NewVector 1 X)
                   (if (= Limit 0) 
                       X+NewVector
                       (@v-help Vector 1 Limit X+NewVector))))

(define @v-help
  OldVector N N NewVector -> (copyfromvector OldVector NewVector N (+ N 1))
  OldVector N Limit NewVector -> (@v-help OldVector (+ N 1) Limit 
                                     (copyfromvector OldVector NewVector N (+ N 1))))

(define copyfromvector
  OldVector NewVector From To -> (trap-error (vector-> NewVector To (<-vector OldVector From)) (/. E NewVector))) 

(define hdv
  Vector -> (trap-error (<-vector Vector 1) (/. E (error "hdv needs a non-empty vector as an argument; not ~S~%" Vector))))

(define tlv
  Vector -> (let Limit (limit Vector)
                 (cases (= Limit 0) (error "cannot take the tail of the empty vector~%")
                        (= Limit 1) (vector 0)
                        true (let NewVector (vector (- Limit 1))
                                  (tlv-help Vector 2 Limit (vector (- Limit 1)))))))

(define tlv-help
  OldVector N N NewVector -> (copyfromvector OldVector NewVector N (- N 1))
  OldVector N Limit NewVector -> (tlv-help OldVector (+ N 1) Limit 
                                     (copyfromvector OldVector NewVector N (- N 1))))

(define assoc
  _ [] -> []
  X [[X | Y] | _] -> [X | Y]
  X [_ | Y] -> (assoc X Y))

(define boolean?
  true -> true
  false -> true
  _ -> false)

(define nl
  0 -> 0
  N -> (do (output "~%") (nl (- N 1))))

(define difference
  [] _ -> []
  [X | Y] Z -> (if (element? X Z) (difference Y Z) [X | (difference Y Z)]))

(define do
  X Y -> Y)

(define element?
  _ [] -> false
  X [X | _] -> true
  X [_ | Z] -> (element? X Z))

(define empty?
  [] -> true
  _ -> false)

(define fix
  F X -> (fix-help F X (F X)))

(define fix-help
  _ X X -> X
  F _ X -> (fix-help F X (F X)))

(define put
  X Pointer Y Vector -> (let N (hash X (limit Vector))
                             Entry (trap-error (<-vector Vector N) (/. E []))
                             Change (vector-> Vector N (change-pointer-value X Pointer Y Entry))
                             Y))

(define change-pointer-value 
  X Pointer Y [] -> [[[X Pointer] | Y]]
  X Pointer Y [[[X Pointer] | _] | Entry] -> [[[X Pointer] | Y] | Entry]
  X Pointer Y [Z | Entry] -> [Z | (change-pointer-value X Pointer Y Entry)])

(define get
  X Pointer Vector -> (let N (hash X (limit Vector))
                           Entry (trap-error (<-vector Vector N) 
                                      (/. E (error "pointer not found~%")))
                           Result (assoc [X Pointer] Entry)
                           (if (empty? Result) (error "value not found~%") (tl Result)))) 

(define hash
  S Limit -> (let Hash (mod (sum (map (function string->n) (explode S))) Limit)
                  (if (= 0 Hash)
                      1
                      Hash)))
                      
(define mod
  N Div -> (modh N (multiples N [Div])))
  
(define multiples
  N [M | Ms] ->  Ms   where (> M N)
  N [M | Ms] -> (multiples N [(* 2 M) M | Ms]))
  
(define modh
   0 _ -> 0
   N [] -> N
   N [M | Ms] -> (if (empty? Ms)
                           N
                           (modh N Ms))   where (> M N)
   N [M | Ms] -> (modh (- N M) [M | Ms]))

(define sum
  [] -> 0
  [N | Ns] -> (+ N (sum Ns)))

(define head
  [X | _] -> X
  _ -> (error "head expects a non-empty list"))

(define tail
  [_ | Y] -> Y
  _ -> (error "tail expects a non-empty list"))

(define hdstr
  S -> (pos S 0))

(define intersection
  [] _ -> []
  [X | Y] Z -> (if (element? X Z) [X | (intersection Y Z)] (intersection Y Z)))

(define reverse
  X -> (reverse_help X []))

(define reverse_help
  [] R -> R
  [X | Y] R -> (reverse_help Y [X | R]))
  
(define union
  [] X -> X
  [X | Y] Z -> (if (element? X Z) (union Y Z) [X | (union Y Z)]))

(define y-or-n?
  String -> (let Message (output String)
                 Y-or-N (output " (y/n) ")
                 Input (make-string "~S" (input))
                 (cases (= "y" Input) true 
                        (= "n" Input) false 
                        true (do (output "please answer y or n~%")
                                 (y-or-n? String)))))

(define not
  X -> (if X false true))

(define subst
  X Y Y -> X
  X Y [W | Z] -> [(subst X Y W) | (subst X Y Z)]
  _ _ Z -> Z)

(define cd 
  Path -> (set *home-directory* (if (= Path "") "" (make-string "~A/" Path))))

(define map
  F X -> (map-h F X []))

(define map-h
  _ [] X -> (reverse X)
  F [X | Y] Z -> (map-h F Y [(F X) | Z]))

(define length
  X -> (length-h X 0))

(define length-h
  [] N -> N
  X N -> (length-h (tl X) (+ N 1)))

(define occurrences
  X X -> 1
  X [Y | Z] -> (+ (occurrences X Y) (occurrences X Z))
  _ _ -> 0)

(define nth 
  1 [X | _] -> X
  N [_ | Y] -> (nth (- N 1) Y))
  
(define integer? 
  N -> (and (number? N) (let Abs (abs N) (integer-test? Abs (magless Abs 1)))))

(define abs
  N -> (if (> N 0) N (- 0 N)))

(define magless
  Abs N -> (let Nx2 (* N 2)
                (if (> Nx2 Abs)
                    N
                    (magless Abs Nx2))))

(define integer-test?
  0 _ -> true
  Abs _ -> false    where (> 1 Abs)  
  Abs N -> (let Abs-N (- Abs N)
                (if (> 0 Abs-N)
                    (integer? Abs)
                    (integer-test? Abs-N N))))  

(define mapcan
  _ [] -> []
  F [X | Y] -> (append (F X) (mapcan F Y)))

(define read-file-as-bytelist
 File -> (let Stream (open file File in)
               Byte (read-byte Stream)
               Bytes (read-file-as-bytelist-help Stream Byte [])
               Close (close Stream)
               (reverse Bytes)))  

(define read-file-as-bytelist-help
  Stream -1 Bytes -> Bytes
  Stream Byte Bytes -> (read-file-as-bytelist-help Stream (read-byte Stream) [Byte | Bytes]))

(define read-file-as-string
   File -> (let Stream (open file File in) 
               (rfas-h Stream (read-byte Stream) "")))
               
(define rfas-h
  Stream -1 String -> (do (close Stream) String)
  Stream N String -> (rfas-h Stream (read-byte Stream) (cn String (n->string N))))  

(define ==
  X X -> true
  _ _ -> false)

(define abort
  -> (simple-error ""))

(define read
  -> (hd (lineread)))

(define input
  -> (eval (read)))

(define input+ 
  _ Type -> (let Input (read)
                 Check (typecheck Input Type)
                 (if (= false Check)
                     (do (output "input is not of type ~R: please re-enter " Type)
                         (input+ : Type))
                     (eval Input))))

(define bound? 
  Sym -> (and (symbol? Sym) 
              (let Val (trap-error (value Sym) (/. E this-symbol-is-unbound))
                          (if (= Val this-symbol-is-unbound)
                              false
                              true))))
                              
(define string->bytes
  "" -> []
  S -> [(string->n (pos S 0)) | (string->bytes (tlstr S))])

(define maxinferences
  N -> (set *maxinferences* N))

(define inferences 
  _ -> (value *infs*))

(define hush
  + -> (set *hush* hushed)
  - -> (set *hush* unhushed)
  _ -> (error "'hush' expects a + or a -~%"))

(define protect
  X -> X)

(define stoutput
  _ -> (value *stoutput*))