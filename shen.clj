#!/bin/bash

version=1.4.0-beta5
clojure=clojure-$version.jar
repo=http://repo1.maven.org/maven2

if [ -e  shen.clj-*-standalone.jar ]; then
	java -Xss4m -jar shen.clj-*-standalone.jar
else
	mkdir -p classes lib
	test -e lib/$clojure || curl $repo/org/clojure/clojure/$version/$clojure > lib/$clojure
	java -cp lib/$clojure:src:classes clojure.main -m shen.install
fi
