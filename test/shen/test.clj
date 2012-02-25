(ns shen.test
  (:use [clojure.test])
  (:require [shen]
            [shen.primitives]))

(deftest README.shen
  (is (shen/read-file "shen/test-programs/README.shen"))
  (is (zero? (shen.primitives/value '*failed*))))
