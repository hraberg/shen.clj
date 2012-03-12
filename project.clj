(defproject shen.clj "0.1.0"
  :description "Shen is a portable functional programming language by Mark Tarver"
  :license {:name "Shen License"
            :url "http://www.shenlanguage.org/license.html"}
  :url "https://github.com/hraberg/shen.clj"
  :dependencies [[clojure "1.4.0-beta4"]]
  :dev-dependencies [[org.clojure/tools.trace "0.7.2-20120223.025622-2"]
                     [marginalia "0.7.0"]
                     [lein-difftest "1.3.7"]]
  :repositories [["sonatype snapshots"
                  "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :main shen
  :aot [shen.primitives shen.install shen])
