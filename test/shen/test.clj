(ns shen.test
  (:use [clojure.test])
  (:require [shen]
            [shen.primitives]))

(deftest README.shen
  (shen/read-file "shen/test-programs/README.shen")
  (is (zero? (shen.primitives/value '*failed*))))
