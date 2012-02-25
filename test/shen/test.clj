(ns shen.test
  (:use [clojure.test])
  (:require [shen]
            [shen.primitives :as primitives]))

(defn parse [s]
  ((intern 'shen (symbol "@p")) (map int s) ()))

(deftest  README.shen
  (is (shen/read-file "shen/test-programs/README.shen"))
  (is (= 0 (shen.primitives/value '*failed*))))