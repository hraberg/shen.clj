(ns shen.benchmarks
  (:use [clojure.test]
        [shen.primitives :only (神)])
  (:require [shen]))

(defn benchmarks []
  (神
   (shen-initialise_environment)
   (cd "shen/benchmarks")
   (load "README.shen")
   (load "benchmarks.shen")))

(defn -main []
  (benchmarks))
