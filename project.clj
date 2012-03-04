(defproject shen.clj "0.1.0-SNAPSHOT"
  :description "Shen is a portable functional programming language"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :dependencies [[org.clojure/clojure "1.4.0-beta1"]]
  :dev-dependencies [[org.clojure/tools.trace "0.7.2-SNAPSHOT"]
                     [marginalia "0.7.0-SNAPSHOT"]]
  :repositories [["sonatype snapshots"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :jvm-opts ["-Xss128m"]
  :main shen.core
  :aot [shen.primitives shen.core shen])
