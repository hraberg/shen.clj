(ns shen.test
  (:use [clojure.test]
        [shen.primitives :only (value shen-symbol shen-kl-to-clj)])
  (:require [shen]
            [shen.primitives :as primitives]))

(defn toggle-trace [tfn]
  (defmethod print-method Object [o ^java.io.Writer w]
    (if (-> o class .isArray) (print-method (vec o) w)
        (do
          (.write w "#<")
          (.write w (.getSimpleName (class o)))
          (.write w " ")
          (.write w (str o))
          (.write w ">"))))

  (require 'clojure.tools.trace)
  (doseq [ns '[shen shen.primitives]]
    ((ns-resolve 'clojure.tools.trace tfn) ns)))

(defn read-bytes [s]
  ((value (shen-symbol "@p")) (map int s) ()))

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
       () "()"))

;; (deftest README.shen
;;   (is (shen/read-file "shen/test-programs/README.shen"))
;;   (is (= 0 (shen.primitives/value '*failed*))))