(set *failed* (set *passed* 0))

(define test-is
	X -> (if (= X true)
				(do (set *passed* (+ (value *passed*) 1))
				    passed)
				(do (set *failed* (+ (value *failed*) 1))
				    failed)))

"testing ="

(test-is (= 2 2))
(test-is (= true true))
(test-is (= "foo" "foo"))
(test-is (= bar bar))

(test-is (= (= 2 3) false))
(test-is (= (= "foo" "bar") false))
(test-is (= (= foo bar) false))
(test-is (= (= true false) false))


"testing defun, lambda & let"
(defun f (x) (/. y (+ x y)))
(test-is (= ((f 3) 2) 5))
(test-is (= 10 (let x 5 (* 2 x))))

"testing errors"
(test-is (= 1 (/ 1 1)))
(test-is (=  (trap-error (/ 1 0) (/. E -1)) -1))
(test-is (=  (trap-error (/ 1 0) (/. E -1)) -1))
(test-is (=  (trap-error (/ 1 1) (/. E -1)) 1))
(trap-error (set newError (simple-error "testError")) (/. E (error-to-string E)))


"testing +"
(test-is (=  (+  2 3) 5))
(test-is (=  (+  2 -3) -1))

"testing -"
(test-is (=  (- 2 3) -1))
(test-is (=  (- 2 -3) 5))

"testing *"
(test-is (=  (*  2 3) 6))
(test-is (=  (*  2 -3) -6))

"testing /"
(test-is (=  (/  5 2) 2.5))
(test-is (=  (/  4 -4) -1))

"testing <"
(test-is (=  (<  1 2) true))
(test-is (=  (<  4 4) false))
(test-is (=  (<  4 0) false))

"testing <="
(test-is (=  (<=  1 2) true))
(test-is (=  (<=  4 4) true))
(test-is (=  (<=  4 0) false))

"testing >"
(test-is (=  (>  3 2) true))
(test-is (=  (>  4 4) false))
(test-is (=  (>  2 4) false))

"testing >="
(test-is (=  (>=  3 2) true))
(test-is (=  (>= 4 4) true))
(test-is (=  (>  2 4) false))

"testing if"
(test-is  (if true true false))
(test-is  (if false false true))
(test-is (= (trap-error (if 5 true true) (/. _ -1)) -1))

"testing number?"
(test-is (=  (number?  3) true))
(test-is (=  (number?  -3.4) true))
(test-is (=  (number?  "fuu") false))
(test-is (=  (number?  bar) false))

"testing and"
(test-is (and true true true))
(test-is (= (and true false true) false))
(test-is (= (and true false true) false))
(test-is (= -1 (trap-error (and 2 true) (/. E -1))))

"testing or"
(test-is (or true true true))
(test-is (or true false true))
(test-is (or (and false false false) true))
(test-is (= -1 (trap-error (or 2 true) (/. E -1))))

"testing value"
(set x 5)
(test-is (=  (value x) 5))
(set x "foo")
(test-is (=  (value x) "foo"))
(test-is (= -1  (trap-error (value valueTest) (/. E -1))))

"testing string?, str, tlstr, cn & pos"
(test-is (string? "bar"))
(test-is (string? (str "foo")))
(test-is (string? (str 2)))
(test-is (string? (str true)))
(test-is (string? (str bar)))
(test-is (= "bar" (str bar)))
(test-is (= (string? 55) false))
(test-is (= (tlstr "foobar") "oobar"))
(test-is (= (trap-error (cn bla blub) (/. _ -1)) -1))
(test-is (= (cn "foo" "bar") "foobar"))
(test-is (= (pos "bar" 2) "r"))

"testing cons?, cons, hd & tl"
(test-is (cons? [2 1 1 "foo"]))
(test-is (cons? (cons 3 [2 1 1 "foo"])))
(test-is (= (cons? 55) false))
(test-is (= (hd [2 1 1]) 2))
(test-is (= false (= (hd [2 1 1]) 1)))
(test-is (= (hd (tl [2 1 1])) 1))

"testing absvector, absvector?, address-> & <-address"
(set v (absvector 5))
(test-is (absvector? (value v)))
(test-is (= (absvector? v) false))
(test-is (= (absvector? 2) false))
(test-is (= (absvector? "foo") false))
(address-> (value v) 2 5)
(test-is (= (<-address (value v) 2) 5))

"testing eval-without-macros, freeze & thaw"
(test-is (= (shen-eval-without-macros (+ 4 5)) 9))
(test-is (= (shen-eval-without-macros 4) (+ 2 2)))
(test-is (= (shen-eval-without-macros hello) hello))
(test-is (= (= 4 (freeze (+ 2 2))) false))
(test-is (= 4 (thaw (freeze (+ 2 2)))))

"testing set, value & intern"
(set x 5)
(test-is (= (value x) 5))
(test-is (= (value (intern "x")) 5))

"testing get-time"
(test-is (number?  (get-time run)))

"testing Streams"
(set fileName (cn (str (get-time run)) ".txt"))
(set writeFile (open file (value fileName) out))
(pr "foobar" (value writeFile))
(close (value writeFile))
(set readFile (open file (value fileName) in))
(test-is (= 102 (read-byte (value readFile))))
(test-is (= 111 (read-byte (value readFile))))
(test-is (= (= 102 (read-byte (value readFile))) false))
(close (value readFile))

"testing n->string"
(test-is (= "d" (n->string 100)))
(test-is (= "h" (n->string 104)))
(test-is (= "(" (n->string 40)))
(test-is (= false (= "d" (n->string 101))))
(test-is (= -1  (trap-error (n->string -10) (/. E -1))))

"testing string->n"
(test-is (= 100 (string->n "d")))
(test-is (= 104 (string->n "h")))
(test-is (= 40 (string->n "(")))
(test-is (= false (= 101 (string->n "d"))))
(test-is (= -1 (trap-error (string->n "") (/. E -1))))

"special tests"
(test-is (= [] ()))
(test-is (= -1 (trap-error ((4 3 2)) (/. E -1))))
(test-is (= -1 (trap-error ([4 3 2]) (/. E -1))))
(test-is (= -1 (trap-error (+4 2) (/. E -1))))
(test-is (= -1 (trap-error (+ 4 "2") (/. E -1))))
(test-is (= -1 (trap-error (+ 4 specialTest) (/. E -1))))
(test-is (= -1 (trap-error (+ 4 true) (/. E -1))))

"Lists"
(test-is (= (trap-error (hd 5) (/. E -1)) -1))
(test-is (= (trap-error (tl 5) (/. E -1)) -1))
(test-is (= (hd [1 2 3]) 1))
(test-is (= (tl [1 2 3]) [2 3]))
(test-is (= (cons 5 10) [5 10]))

"Strings"
(test-is (= (str hello) "hello"))
(test-is (= (str 5) "5"))

(test-is (= (str "hello") "c#34;helloc#34;"))

(test-is (string? (str 5)))
(test-is (string? (str hello)))
(test-is (string? (str "hello")))

(test-is (= "helloWorld" (cn "hello" "World")))
(test-is (= "hello" (cn "hello" "")))
(test-is (= "hello" (cn "" "hello")))

(map (/. X (test-is (= (pos "hello" X) (pos (tlstr "hello") (- X 1))))) [1 2 3 4])

"Tuples"
(test-is (tuple? (@p hello ())))
(test-is (= (fst (@p hello ())) hello))
(test-is (= (snd (@p hello ())) ()))

"Symbols"
(test-is (symbol? x))
(test-is (= x (intern "x")))
(test-is (= (intern (cn "x" "y")) (concat x y)))

"Vectors"
(set x (vector 100))
(test-is (= (<-address (value x) 0) 100))
(test-is (= (<-address (value x) 1) (fail)))
(test-is (= (<-address (value x) 99) (fail)))
(test-is (= (trap-error (<-address (value x) 101) (/. E -1)) -1))
(address-> (value x) 10 100)
(test-is (= (<-address (value x) 10) 100))

"Absvectors"
(set x (absvector 100))
(test-is (= (<-address (value x) 1) (fail)))
(test-is (= (<-address (value x) 99) (fail)))
(test-is (= (trap-error (<-address (value x) 100) (/. E -1)) -1))
(address-> (value x) 10 100)
(test-is (= (<-address (value x) 10) 100))

"Exceptions"
(test-is (= (trap-error (/ 1 0) (/. E -1)) -1))
(test-is (= (trap-error (/ 1 0) (/. E (error-to-string E))) "division by zero"))

"Numbers"
(test-is (= 10e2 1000))
(test-is (= 1 1.0))
(test-is (= --3 3))
(test-is (= ---5 -5.0))


(intoutput "~%passed ... ~A~%failed ... ~A~%Passrate ... ~A %~%" (@p (value *passed*) (@p (value *failed*) (@p (/ (* 100 (value *passed*)) (+ (value *failed*) (value *passed*))) ()))))