(defproject shen-clj "0.1.0-SNAPSHOT"
  :description "Shen is a portable functional programming language"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :dependencies [[org.clojure/clojure "1.4.0-beta1"]]
  :extra-classpath-dirs ["shen/platforms/clj"]
  :main shen
  :aot [shen.primitives
        shen] )
