(ns shen.test
  (:use [clojure.test]
        [shen.primitives :only (value set shen-kl-to-clj λ 神 define defmacro defprolog prolog?
                                      reset-macros! package parse-shen parse-and-eval-shen)])
  (:refer-clojure :exclude [eval defmacro set for filter])
  (:require [shen]))

(define super
  [Value Succ End] Action Combine Zero ->
  (if (End Value)
    Zero
    (Combine (Action Value)
             (super [(Succ Value) Succ End]
                    Action Combine Zero))))

(define for
  Stream Action -> (super Stream Action do 0))


(define filter
  Stream Condition ->
  (super Stream
         (λ Val (if (Condition Val) [Val] []))
         append
         []))

(deftest shenlanguage.org
  (are [shen out] (= out (with-out-str (shen/print shen)))

       (神
        (c/with-out-str
          (for [0 (+ 1) (= 10)] print)))
       "\"0123456789\""

       (神
        (filter [0 (+ 1) (= 100)]
                (λ X (integer? (/ X 3)))))
       "[0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60... etc]"

       ))

(deftest interop
  (defprolog mem
    X [X | _] <--\;
    X [Y | Z] <-- (mem X Z)\;)

  (define factorial
    0 -> 1
    X -> (* X (factorial (- X 1))))

  (are [shen out] (= out (with-out-str (shen/print shen)))

       (filter [0 (partial + 1) (partial = 100)]
               #(integer? (/ % 3)))
       "[0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60... etc]"

       (prolog? (mem 1 [X | 2]) (return X))
       "1"

       (factorial 5)
       "120"

       (let [n 5]
         (神
          (factorial n)))
       "120"

       ))

(deftest shen-defmacro
  (are [shen out] (re-find (re-pattern out) (with-out-str (shen/print shen)))

       (神
        (clj-exec (/ 8 2)))
       (str
        "run time: .+ secs" "\n"
        "4")

       (神
        (parsed-exec (/ 2 0)))
       "failed"

       ))

(deftest partials
  (are [shen result] ((if (fn? result) result #{result}) shen)

       (神
        ((λ X Y (/ X Y)) 10 5))
       2

       (神
        ((λ X Y (+ X Y)) 2))
       fn?

       (神
        ((λ X (integer? (/ X 3))) 3))
       true

       ))

(deftest packages
  (are [shen out] (= out (with-out-str (-> shen parse-and-eval-shen)))

       "(package null () (print 1) (print 2))"
       "12"

       ))

(deftest cons-pair
  (are [shen result] ((if (fn? result) result #{result}) shen)

       (神
        (cons 1 2))
       [1 2]

       (神
        (cons 1 (cons 2 ())))
       '(1 2)

       (神
        [1 2])
       '(1 2)

       ))

(deftest printer
  (are [shen out] (= out (with-out-str (shen/print shen)))

       (神
        ())
       "[]"

       (神
        (cons 1 (cons 2 ())))
       "[1 2]"

       (神
        (cons 1 2))
       "[1 | 2]"

       (神
        (cons (cons 1 2) 3))
       "[[1 | 2] | 3]"

       (神
        (cons 1 (cons 2 3)))
       "[1 2 | 3]"

       (神
        (absvector 1))
       "<fail!>"

       (神
        (vector 1))
       "<...>"

       (神
        (vector 0))
       "<>"

       (神
        (@p 1 2))
       "(@p 1 2)"

       ))

(deftest eval
  (are [shen result] ((if (fn? result) result #{result})
                      (-> shen parse-and-eval-shen))

       "((/. X (+ X 2)) 1)"
       3

       "((/. X Y (+ X Y)) 2 2)"
       4

       "((/. X Y (+ X Y)) 2)"
       fn?

       "(filter [0 (+ 1) (= 100)] (/. X (integer? (/ X 3))))"
       seq?

       "(defprolog f a <--;)"
       'f

       "(cond (true \"/\"))"
       "/"

       "(= 1.0 1)"
       true?

       ))

(deftest dual-namespace
  (set 'dual-namespace true)
  (is (true? @(resolve 'shen.globals/dual-namespace)))
  (is (true? (value 'dual-namespace)))
  (is (nil? (resolve 'shen/dual-namespace)))

  (set 'element? nil)
  (is (nil? (value 'element?)))
  (is (fn? shen/element?)))

(deftest parser
  (are [kl-str clj] (= clj (-> kl-str parse-shen first
                               shen-kl-to-clj))
       "1"
       1

       "1.0"
       1.0

       "symbol"
       ''symbol

       ""
       nil

       "nil"
       `'~(symbol "nil")

       "true"
       true

       "false"
       false

       "\"String\""
       "String"

       "()"
       ()

       "(+ 1 1)"
       '(+ 1 1)

       ))

(use-fixtures :once (fn [suite]
                      (defmacro clj-exec-macro
                        [clj-exec Expr] -> [trap-error [time Expr] [λ E failed]])
                      (parse-and-eval-shen "(defmacro parsed-exec-macro [parsed-exec Expr] -> [trap-error [time Expr] [/. E failed]])")

                      (suite)

                      (reset-macros!)))

(defn toggle-trace [tfn]
  (require 'clojure.tools.trace)
  (doseq [ns '[shen shen.primitives]]
    ((ns-resolve 'clojure.tools.trace tfn) ns)))


;; CLisp

;; passed ... 146
;; failed ...0
;; pass rate ...100%

;; ok
;; 0

;; run time: 25.129999235272408 secs
;; loaded


(defn test-programs []
  (神
   (cd "shen/test-programs")
   (load "README.shen")
   (load "tests.shen")))

(defn -main []
  (test-programs))
