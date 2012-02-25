(ns shen.test
  (:use [clojure.test])
  (:require [shen]
            [shen.primitives]))

(deftest README.shen
  (shen/read-file "shen/test-programs/README.shen")
  (is (= 0 (shen.primitives/value '*failed*))))
