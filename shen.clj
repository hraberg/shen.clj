#!/bin/bash -e

version=1.5.0-RC2
clojure=clojure-$version.jar
repo=http://repo1.maven.org/maven2
rlwrap=$(which rlwrap) || "" &> /dev/null
java="$rlwrap java -Xss4m $JAVA_OPTS"

if [ -e shen.clj-*-standalone.jar ]; then
	$java -jar shen.clj-*-standalone.jar
else
	mkdir -p classes lib
	test -e lib/$clojure || curl $repo/org/clojure/clojure/$version/$clojure > lib/$clojure
	$java -cp lib/$clojure:src:classes clojure.main -m shen.install
fi
