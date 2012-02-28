(ns shen.test
  (:use [clojure.test]
        [shen.primitives :only (value intern shen-kl-to-clj λ 神 define)])
  (:refer-clojure :exclude [intern eval])
  (:require [shen]
            [shen.primitives :as primitives]))

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
        (clj/with-out-str
          (for [0 (+ 1) (= 10)] print)))
       "0123456789"

       (神
        (filter [0 (+ 1) (= 100)]
                (λ X (integer? (/ X 3)))))
       "[0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60... etc]"

       (神
        ((λ X Y (/ X Y)) 10 5))
       "2"

       (神
        ((λ X (integer? (/ X 3))) 3))
       "true"

       ))

(defn read-bytes [s]
  ((value (intern "@p")) (map int s) ()))

(defn parse-shen [s]
  (-> s read-bytes shen/shen-<st_input> shen/snd first))

(deftest parser
  (are [clj kl-str] (= clj (-> kl-str parse-shen
                               shen-kl-to-clj))
       1 "1"
       1.0 "1.0"
       ''a-symbol "a-symbol"
       nil ""
       nil "nil"
       true "true"
       false "false"
       "String" "\"String\""
       () "()"
       '(+ 1 1) "(+ 1 1)"))

(deftest eval
  (are [shen result] ((if (fn? result) result #{result})
                      (-> shen parse-shen shen/eval))

       "((/. X (+ X 2)) 1)"
       3

       "((/. X Y (+ X Y)) 2 2)"
       4

       "((/. X Y (+ X Y)) 2)"
       fn?

       "(filter [0 (+ 1) (= 100)] (/. X (integer? (/ X 3))))"
       seq?

       ))

(defn test-programs []
  (shen/cd "shen/test-programs")
  (shen/load "README.shen")
  (shen/load "tests.shen"))

;; (deftest README.shen
;;   (is (test-programs))
;;   (is (= 0 (shen.primitives/value '*failed*))))

(defn toggle-trace [tfn]
  (require 'clojure.tools.trace)
  (doseq [ns '[shen shen.primitives]]
    ((ns-resolve 'clojure.tools.trace tfn) ns)))
