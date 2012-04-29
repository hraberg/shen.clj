(ns shen.primitives-test
  (:use [clojure.test]
        [shen.primitives :only (value 神)])
  (:require [shen]
            [clojure.java.io :as io]))

;; Dominik's test suite for the primitives from
;; http://code.google.com/p/shen-to-clojure/

(deftest primitives
  (let [stdout (with-out-str
                 (神
                  (cd "test/shen")
                  (load "primitives.shen")))]
    (is (zero? (value '*failed*)) stdout)))

(use-fixtures :each (fn [test]
                      (test)
                      (try
                        (.delete (io/file "test/shen" (value 'fileName)))
                        (catch Exception _))))
