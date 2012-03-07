(defproject shen.clj "0.1.0-SNAPSHOT"
  :description "Shen is a portable functional programming language"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :dependencies [[org.clojure/clojure "1.4.0-beta1"]]
  :dev-dependencies [[org.clojure/tools.trace "0.7.2-SNAPSHOT"]
                     [marginalia "0.7.0"]
                     [lein-difftest "1.3.7"]]
  :repositories [["sonatype snapshots"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :main shen.core
  :aot [shen.primitives shen.core shen])
