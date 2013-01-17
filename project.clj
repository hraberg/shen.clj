(defproject shen.clj/shen.clj "0.1.8-SNAPSHOT"
  :description "Shen is a portable functional programming language by Mark Tarver"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :url "https://github.com/hraberg/shen.clj"
  :repositories {"sonatype snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :dependencies [[org.clojure/clojure "1.5.0-RC2"]]
  :profiles {:dev {:dependencies [[marginalia "0.7.1"]]}}
  :plugins [[lein-swank "1.4.4"]
            [lein-difftest "2.0.0"]]
  :compile-path "classes"
  :target-path ""
  :aot [shen.primitives shen.install shen]
  :main shen.install
  :min-lein-version "2.0.0")
