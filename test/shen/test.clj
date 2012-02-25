(ns shen.test
  (:use [clojure.test])
  (:require [shen]
            [shen.primitives :as primitives]))

(defn read-bytes [s]
  ((intern 'shen (symbol "@p")) (map int s) ()))

(defn parse-shen [s]
  (let [tuple (if (string? s) (read-bytes s) s)]
    (shen/shen-<st_input> tuple)))

(deftest parser
  (are [clj kl-str] (= clj (primitives/shen-kl-to-clj
                            (first (shen/snd (parse-shen kl-str)))))
       1 "1"
       1.0 "1.0"
       ''a-symbol "a-symbol"
       nil "nil"
       true "true"
       false "false"
       "String" "\"String\""
       () "()"))

;; (deftest README.shen
;;   (is (shen/read-file "shen/test-programs/README.shen"))
;;   (is (= 0 (shen.primitives/value '*failed*))))