(set *symbolcodes* (vector 128))   
                  
(address-> (value *symbolcodes*)  126 "~")
(address-> (value *symbolcodes*)  122  "z")
(address-> (value *symbolcodes*)  121  "y")
(address-> (value *symbolcodes*)  120  "x")
(address-> (value *symbolcodes*)  119  "w")
(address-> (value *symbolcodes*)  118  "v")
(address-> (value *symbolcodes*)  117  "u")
(address-> (value *symbolcodes*)  116  "t")
(address-> (value *symbolcodes*)  115  "s")
(address-> (value *symbolcodes*)  114  "r")
(address-> (value *symbolcodes*)  113  "q")
(address-> (value *symbolcodes*)  112  "p")
(address-> (value *symbolcodes*)  111  "o")
(address-> (value *symbolcodes*)  110  "n")
(address-> (value *symbolcodes*)  109  "m")
(address-> (value *symbolcodes*)  108  "l")
(address-> (value *symbolcodes*)  107  "k")
(address-> (value *symbolcodes*)  106  "j")
(address-> (value *symbolcodes*)  105  "i")
(address-> (value *symbolcodes*)  104  "h")
(address-> (value *symbolcodes*)  103  "g")
(address-> (value *symbolcodes*)  102  "f")
(address-> (value *symbolcodes*)  101  "e")
(address-> (value *symbolcodes*)  100  "d")
(address-> (value *symbolcodes*)  99  "c")
(address-> (value *symbolcodes*)  98  "b")
(address-> (value *symbolcodes*)  97  "a")
(address-> (value *symbolcodes*)  96  "`")
(address-> (value *symbolcodes*)  95  "_")
(address-> (value *symbolcodes*)  90  "Z")
(address-> (value *symbolcodes*)  89  "Y")
(address-> (value *symbolcodes*)  88  "X")
(address-> (value *symbolcodes*)  87  "W")
(address-> (value *symbolcodes*)  86  "V")
(address-> (value *symbolcodes*)  85  "U")
(address-> (value *symbolcodes*)  84  "T")
(address-> (value *symbolcodes*)  83  "S")
(address-> (value *symbolcodes*)  82  "R")
(address-> (value *symbolcodes*)  81  "Q")
(address-> (value *symbolcodes*)  80  "P")
(address-> (value *symbolcodes*)  79  "O")
(address-> (value *symbolcodes*)  78  "N")
(address-> (value *symbolcodes*)  77  "M")
(address-> (value *symbolcodes*)  76  "L")
(address-> (value *symbolcodes*)  75  "K")
(address-> (value *symbolcodes*)  74  "J")
(address-> (value *symbolcodes*)  73  "I")
(address-> (value *symbolcodes*)  72  "H")
(address-> (value *symbolcodes*)  71  "G")
(address-> (value *symbolcodes*)  70  "F")
(address-> (value *symbolcodes*)  69  "E")
(address-> (value *symbolcodes*)  68  "D")
(address-> (value *symbolcodes*)  67  "C")
(address-> (value *symbolcodes*)  66  "B")
(address-> (value *symbolcodes*)  65  "A")
(address-> (value *symbolcodes*)  64  "@")
(address-> (value *symbolcodes*)  63  "?")
(address-> (value *symbolcodes*)  62  ">")
(address-> (value *symbolcodes*)  61  "=")
(address-> (value *symbolcodes*)  60  "<")
(address-> (value *symbolcodes*)  57  "9")
(address-> (value *symbolcodes*)  56  "8")
(address-> (value *symbolcodes*)  55  "7")
(address-> (value *symbolcodes*)  54  "6")
(address-> (value *symbolcodes*)  53  "5")
(address-> (value *symbolcodes*)  52  "4")
(address-> (value *symbolcodes*)  51  "3")
(address-> (value *symbolcodes*)  50  "2")
(address-> (value *symbolcodes*)  49  "1")
(address-> (value *symbolcodes*)  48  "0")
(address-> (value *symbolcodes*)  47  "/")
(address-> (value *symbolcodes*)  46  ".")
(address-> (value *symbolcodes*)  45  "-")
(address-> (value *symbolcodes*)  43  "+")
(address-> (value *symbolcodes*)  42  "*")
(address-> (value *symbolcodes*)  39  "'")
(address-> (value *symbolcodes*)  38  "&")
(address-> (value *symbolcodes*)  37  "%")
(address-> (value *symbolcodes*)  36  "$")
(address-> (value *symbolcodes*)  35  "#")
(address-> (value *symbolcodes*)  33  "!")     

(define lineread
  -> (lineread-loop (read-byte) []))

(define lineread-loop
  Byte _ -> (error "line read aborted")  where (= Byte (hat))
  Byte Bytes -> (let Line (compile (function <st_input>) Bytes [])
                      (if (or (= Line (fail)) (empty? Line))
                          (lineread-loop (read-byte) (append Bytes [Byte]))
                          Line))	where (element? Byte [(newline) (carriage-return)])
  Byte Bytes -> (lineread-loop (read-byte) (append Bytes [Byte])))

(define read-file
  File -> (let Bytelist (read-file-as-bytelist File)
               (compile (function <st_input>) Bytelist (function read-error))))

(define read-error
  Bytes -> (error "read error here:~%~% ~A~%" (compress-50 50 Bytes)))

(define compress-50
  _ [] -> ""
  0 _ -> ""
  N [Byte | Bytes] -> (cn (n->string Byte) (compress-50 (- N 1) Bytes)))

(defcc <st_input>
  <lsb> <st_input1> <rsb> <st_input2> 
    := [(macroexpand (cons_form <st_input1>)) | <st_input2>];
  <lrb>  <st_input1> <rrb> <st_input2> 
   := (package-macro (macroexpand <st_input1>) <st_input2>);
  <lcurly> <st_input> := [{ | <st_input>];
  <rcurly> <st_input> := [} | <st_input>];    
  <bar> <st_input> := [bar! | <st_input>];  
  <semicolon> <st_input> := [; | <st_input>];
  <colon> <equal> <st_input> := [:= | <st_input>];
  <colon> <minus> <st_input> := [:- | <st_input>];
  <colon> <st_input> := [: | <st_input>];
  <comma> <st_input> := [, | <st_input>];
  <comment> <st_input> := <st_input>;
  <atom> <st_input> := [(macroexpand <atom>) | <st_input>];
  <whitespaces> <st_input> := <st_input>;
  <e> := [];)
  
(defcc <lsb>
   -*- := (if (= -*- 91) skip (fail));)  
   
(defcc <rsb>
   -*- := (if (= -*- 93) skip (fail));)     
  
(defcc <lcurly>
  -*- := (if (= -*- 123) skip (fail));)
  
(defcc <rcurly>
  -*- := (if (= -*- 125) skip (fail));)
  
(defcc <bar>
  -*- := (if (= -*- 124) skip (fail));)  
  
(defcc <semicolon>
  -*- := (if (= -*- 59) skip (fail));) 
  
(defcc <colon>
  -*- := (if (= -*- 58) skip (fail));)     
      
(defcc <comma>
  -*- := (if (= -*- 44) skip (fail));)  
  
(defcc <equal>
   -*- := (if (= -*- 61) skip (fail));)     
   
(defcc <minus>
   -*- := (if (= -*- 45) skip (fail));)      
  
(defcc <lrb>
  -*- := (if (= -*- 40) skip (fail));)
  
(defcc <rrb>
  -*- := (if (= -*- 41) skip (fail));)   
  
(defcc <atom>
  <str> := (control-chars <str>); 
  <number>; 
  <sym>;)

(define control-chars
  [] -> ""
  ["c" "#" | Ss]
   -> (let CodePoint (code-point Ss)
           AfterCodePoint (after-codepoint Ss)
           (@s (n->string (decimalise CodePoint)) (control-chars AfterCodePoint)))  
  [S | Ss] -> (@s S (control-chars Ss)))
                          
(define code-point
  [";" | _] -> ""
  [S | Ss] -> [S | (code-point Ss)]  
                     where (element? S ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"])
  S -> (error "code point parse error ~A~%" S))                          
                          
(define after-codepoint
   [] -> []
   [";" | Ss] -> Ss
   [_ | Ss] -> (after-codepoint Ss))                          
                          
(define decimalise
  S -> (pre (reverse (digits->integers S)) 0))

(define digits->integers
  ["0" | S] -> [0 | (digits->integers S)]
  ["1" | S] -> [1 | (digits->integers S)]
  ["2" | S] -> [2 | (digits->integers S)]
  ["3" | S] -> [3 | (digits->integers S)]
  ["4" | S] -> [4 | (digits->integers S)]
  ["5" | S] -> [5 | (digits->integers S)]
  ["6" | S] -> [6 | (digits->integers S)]
  ["7" | S] -> [7 | (digits->integers S)]
  ["8" | S] -> [8 | (digits->integers S)]
  ["9" | S] -> [9 | (digits->integers S)]
   _ -> [])

(defcc <sym>
  <alpha> <symchars> := (intern (cn <alpha> <symchars>));
  <alpha> := (intern <alpha>);)
  
(defcc <symchars>
   <symchar> <symchars> := (cn <symchar> <symchars>);
   <symchar> := <symchar>;)  
   
(defcc <symchar>
    <alpha>;
    <digit->string>;)

(defcc <digit->string>   
  -*- := (if (digit-byte? -*-) 
             (n->string -*-) 
             (fail));)   

(define digit-byte?
  48 -> true
  49 -> true
  50 -> true
  51 -> true 
  52 -> true
  53 -> true 
  54 -> true 
  55 -> true
  56 -> true 
  57 -> true
  _ -> false)
   
(defcc <alpha>
  -*- := (let S (symbol-byte->string -*-)
             (if (= S (fail))  
                 (fail)
                 S));)
                 
(define symbol-byte->string
  Byte -> (<-address (value *symbolcodes*) Byte))              
  
(defcc <str>
  <dbq> <strcontents> <dbq> := <strcontents>;)
  
(defcc <dbq>
  -*- := (if (= -*- 34) skip (fail));)    
  
(defcc <strcontents>
  <strc> <strcontents> := [<strc> | <strcontents>];
  <e> := [];)
  
(defcc <byte>
  -*- := (n->string -*-);)  
  
(defcc <strc>
  -*- := (if (= -*- 34) (fail) (n->string -*-));)
  
(defcc <backslash>
  -*- := (if (= -*- 92) skip (fail));)
  
(defcc <number>
   <minus> <number> := (- 0 <number>);
   <plus> <number> := <number>;
   <predigits> <stop> <postdigits> <E> <log10> 
   := (* (expt 10 <log10>) (+ (pre (reverse <predigits>) 0) (post <postdigits> 1)));
   <digits> <E> <log10> := (* (expt 10 <log10>) (pre (reverse <digits>) 0));
   <predigits> <stop> <postdigits> 
   := (+ (pre (reverse <predigits>) 0) (post <postdigits> 1));
   <digits> := (pre (reverse <digits>) 0);)

(defcc <E>
   101;)

(defcc <log10>
  <minus> <digits> := (- 0 (pre (reverse <digits>) 0));
  <digits> := (pre (reverse <digits>) 0);)
   
(defcc <plus>
  -*- := (if (= -*-  43) skip (fail));)
  
(defcc <stop>
  -*- := (if (= -*- 46) skip (fail));)      
   
(defcc <predigits>
    <digits>;
    <e> := [];)
    
(defcc <postdigits>
    <digits>;)

(defcc <digits>
   <digit> <digits> := [<digit> | <digits>];
   <digit> := [<digit>];)
 
(defcc <digit>
  -*- := (if (digit-byte? -*-) (byte->digit -*-) (fail));)
  
(define byte->digit  
  48 -> 0   
  49 -> 1  
  50 -> 2  
  51 -> 3  
  52 -> 4  
  53 -> 5  
  54 -> 6
  55 -> 7   
  56 -> 8   
  57 -> 9)
  
(define pre
  [] _ -> 0
  [N | Ns] Expt -> (+ (* (expt 10 Expt) N) (pre Ns (+ Expt 1))))
  
(define post
  [] _ -> 0
  [N | Ns] Expt -> (+ (* (expt 10 (- 0 Expt)) N) (post Ns (+ Expt 1)))) 
    
(define expt
  _ 0 -> 1
  Base Expt -> (* Base (expt Base (- Expt 1)))  where (> Expt 0)
  Base Expt -> (* 1.0 (/ (expt Base (+ Expt 1)) Base)))  
  
(defcc <st_input1>
  <st_input> := <st_input>;)

(defcc <st_input2>
  <st_input> := <st_input>;)

(defcc <comment>
  <backslash> <times> <any> <times> <backslash> := skip;)  
 
(defcc <times>
  -*- := (if (= -*- 42) skip (fail));)       

(defcc <any>
  <comment> <any> := skip;
  <blah> <any> := skip;
  <e> := skip;)

(defcc <blah>
  -*- := (if (end-of-comment? -s-) (fail) skip);)

(define end-of-comment?
  [42 92 | _] -> true   
  _ -> false)  

(defcc <whitespaces>
  <whitespace> <whitespaces> := skip;
  <whitespace> := skip;)

(defcc <whitespace>
  -*- := (let Case -*-
              (cases (= Case 32) skip 
                     (= Case 13) skip
                     (= Case 10) skip
                     (= Case 9) skip
                     true (fail)));)                      

(define cons_form
  [] -> []
  [X bar! Y] -> [cons X Y]	
  [X | Y] -> [cons X (cons_form Y)])  
 
(define package-macro
    [$ S] Stream -> (append (explode S) Stream)
    [package null _ | Code] Stream -> (append Code Stream)
    [package PackageName Exceptions | Code] Stream
     -> (let ListofExceptions (eval-without-macros Exceptions)
             Record (record-exceptions ListofExceptions PackageName)
             (append (packageh PackageName ListofExceptions Code) Stream))
    X Stream -> [X | Stream])  

(define record-exceptions 
  ListofExceptions PackageName 
   -> (let CurrExceptions (trap-error (get PackageName external-symbols) (/. E []))
           AllExceptions (union ListofExceptions CurrExceptions)
           (put PackageName external-symbols AllExceptions)))   
       
(define packageh
    PackageName Exceptions [X | Y] 
      -> [(packageh PackageName Exceptions X) | (packageh PackageName Exceptions Y)]
    PackageName Exceptions X -> X  
                 where (or (sysfunc? X) (variable? X) (element? X Exceptions)
                           (doubleunderline? X) (singleunderline? X))
    PackageName Exceptions X -> (concat PackageName X)   
             where (and (symbol? X) (not (prefix? ["s" "h" "e" "n" "-"] (explode X))))
    _ _ X -> X) 

(define read-from-string
  S -> (let Ns (map (function string->n) (explode S))
            (compile (function shen-<st_input>) 
                     Ns 
                     (function shen-read-error))))