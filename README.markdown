# 神.clj | Shen for Clojure

http://shenlanguage.org/

Shen is a portable functional programming language by [Mark Tarver](http://www.lambdassociates.org/) that offers

* pattern matching,
* λ calculus consistency,
* macros,
* optional lazy evaluation,
* static type checking,
* an integrated fully functional Prolog,
* and an inbuilt compiler-compiler.


## This Clojure Port

`[shen.clj "0.1.6"]` | [Marginalia](http://ghettojedi.org/shen.clj/)

Is a work in progress. Passes the Shen 6.0 test suite.

Uses [Leiningen 2](https://github.com/technomancy/leiningen) to build.
The script [`build`](https://github.com/hraberg/shen.clj/blob/master/build) is used for full, repeatable builds.


### To run the REPL:

#### Standalone release

`java -jar `[`shen.clj-0.1.6-standalone.jar`](https://github.com/downloads/hraberg/shen.clj/shen.clj-0.1.6-standalone.jar)


#### Leiningen

    lein trampoline run

    # If shen.clj already exists, for readline support:
    lein repl

    # java:
    java -cp lib/clojure-1.4.0.jar:shen.clj-0.1.6-SNAPSHOT.jar shen


#### Plain

    ./shen.clj

---

    Shen 2010, copyright (C) 2010 Mark Tarver
    www.lambdassociates.org, version 6.0
    running under Clojure, implementation: Clojure 1.4.0 [jvm 1.8.0-ea]
    port 0.1.6 ported by Håkan Råberg


    (0-) (define super
           [Value Succ End] Action Combine Zero ->
             (if (End Value)
                 Zero
                 (Combine (Action Value)
                          (super [(Succ Value) Succ End]
                                 Action Combine Zero))))
    super

    (1-) (define for
           Stream Action -> (super Stream Action do 0))
    for

    (2-) (define filter
           Stream Condition ->
             (super Stream
                    (/. Val (if (Condition Val) [Val] []))
                    append
                    []))
    filter

    (3-) (for [0 (+ 1) (= 10)] print)
    01234567890

    (4-) (filter [0 (+ 1) (= 100)]
                 (/. X (integer? (/ X 3))))
    [0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60... etc]


### The Shen Test Suite

The Shen test suite is now running, slowly but surely:

    [... loads of output ...]
    passed ... 146
    failed ...0
    pass rate ...100%

    ok
    0

    run time: 16.713 secs
    loaded

The suite can be run via:

    yes | lein trampoline run -m shen.test


The benchmarks can be run via:

    JAVA_OPTS="-Xss6m" lein run -m shen.benchmarks


* Performance is not a goal for 0.1.x, but some tuning has been made to ease development.

### Libraries

* [`strlib.shen`](http://www.shenlanguage.org/library.html) loads and type checks ok.
* [`vectors.shen`](http://www.shenlanguage.org/library.html) loads and type checks ok.
* [`maths-lib.shen`](http://www.shenlanguage.org/library.html) loads and type checks ok.
* [shen-libs](https://github.com/vasil-sd/shen-libs) part of it loads, but [`file-system.shen`](https://github.com/vasil-sd/shen-libs/blob/master/file-system/file-system.shen) needs a java.io version to proceeed.


### 神, define, prolog? and defprolog macros

Instead of using Shen's reader, you can embed Shen directly in Clojure using these macros.
For simplicity, all Shen code lives and is evaluated in the `shen` namespace for now (this will likely change).

```clojure
; shen.test/shenlanguage.org
(define for
  Stream Action -> (super Stream Action do 0))

; shen.test/printer
(神
 (cons 1 2))
"[1 | 2]"

(神
 (@p 1 2))
"(@p 1 2)"

; shen.test/partials
(神
 ((λ X Y (+ X Y)) 2))
fn?
```

As can be seen `λ` stands in for `/.` in Shen to avoid Clojure reader macros.
`@p`, `@s` and `@v` are converted from Clojure deref to their Shen symbols.
Characters, like `\;`, will also be converted to symbols.

Note that `[]` in Shen are lists, and not Clojure vectors. A Clojure vector with a count of 2 is used to represent a cons pair internally.

See [`shen.test`](https://github.com/hraberg/shen.clj/blob/master/test/shen/test.clj) for more examples.


#### Shen calling Clojure

##### Embedded

Shen code can (but this is not very tested) access `clojure.core`, which is required as `c`:

```clojure
(神
  (c/with-out-str
    (for [0 (+ 1) (= 10)] print)))
"\"0123456789\""
```

##### Shen FFI

[Shen FFI](http://www.shenlanguage.org/library.html) can be used for (basic) interaction with Clojure from Shen:

    (load "ffi.shen")
    (ffi clj (@p shen->clj send-clj) (@p clj->shen receive-clj))

    ; Clojure map entries as Shen lists
    (call-ffi clj *clojure-version*)
    [[:major | 1] [:minor | 4] [:incremental | 0] [:qualifier | nil]]

    ; Calling Java
    (call-ffi clj (System/currentTimeMillis))
    1336093159995


More advanced mixing and requiring of Clojure packages isn't supported yet.


## Roadmap

This port, while aiming to conform closely (and hopefully fully) to the [Shen specification](http://shenlanguage.org/Documentation/shendoc.htm), has its primary goal to enable Shen's power in real world Clojure code.

* Shen / Clojure interop:
  * Shen packages as namespaces?
  * Hiding Shen internal names.
  * Bringing smaller parts of Shen goodness back into Clojure: predicate dispatch, pattern matching, prolog. Maybe even the type system.
  * Ensuring Shen can call Clojure/Java properly.
* Future / Questions
  * More TCO than implicit recur for KLambda?
  * Making Shen as lazy as its host?
  * Existing Shen libraries and portability?
  * ClojureScript.
  * overwrite.clj - rewriting more parts of Shen into Clojure if interop or performance requires it.
* [Shen in 15 minutes](http://www.shenlanguage.org/learn-shen/tutorials/shen_in_15mins.html#shen-in-15mins) as smoke test for the REPL


#### The other port, Shen to Clojure

http://code.google.com/p/shen-to-clojure/

## License

http://shenlanguage.org/license.html

Shen, Copyright © 2010-2012 Mark Tarver

shen.clj, Copyright © 2012 Håkan Råberg

---
YourKit is kindly supporting open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
<a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
<a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.
