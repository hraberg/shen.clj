(defproject shen-clj "3.0-SNAPSHOT"
  :description "Shen is a portable functional programming language"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :dependencies [[org.clojure/clojure "1.4.0-beta1"]]
  :extra-classpath-dirs ["shen/platforms/clj"]
  :main shen
  :aot [shen.core
        shen.primitives
        shen])
