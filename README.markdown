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

Is a work in progress. The example on the Shen homepage now works. The test suite is not running properly yet.

Uses [Leiningen](https://github.com/technomancy/leiningen) to build.

### To run the REPL:

    lein trampoline run

    # If shen.clj already exists, for readline support:
    lein repl

    Shen 2010, copyright (C) 2010 Mark Tarver
    www.lambdassociates.org, version 3.0
    running under Clojure, implementation: Clojure 1.4.0-beta1 [jvm 1.8.0-ea]
    port 0.1.0-SNAPSHOT ported by Håkan Råberg


    (0-) (define super
           [Value Succ End] Action Combine Zero ->
             (if (End Value)
                 Zero
                 (Combine (Action Value)
                          (super [(Succ Value) Succ End]
                                 Action Combine Zero))))
    #'shen/super

    (1-) (define for
           Stream Action -> (super Stream Action do 0))
    #'shen/for

    (2-) (define filter
           Stream Condition ->
             (super Stream
                    (/. Val (if (Condition Val) [Val] []))
                    append
                    []))
    #'shen/filter

    (3-) (for [0 (+ 1) (= 10)] print)
    01234567890

    (4-) (filter [0 (+ 1) (= 100)]
                 (/. X (integer? (/ X 3))))
    [0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60... etc]


## Roadmap

This port, while aiming to conform closely (and hopefully fully) to the [Shen specification](http://shenlanguage.org/Documentation/shendoc.htm), has its primiary goal to enable Shen's power in real world Clojure code.

* Test suite running properly. (day/s)
* Test suite passing. (days to week/s)
* Shen / Clojure interop:
    * define macro to embed Shen in Clojure (if needed: https://github.com/klutometis/reader-macros)
    * Shen packages as namespaces?
    * Hiding KLambda and its names a bit.
    * Bringing smaller parts of Shen goodness back into Clojure: predicate dispatch, pattern matching, prolog. Maybe even the type system.
    * Ensuring Shen can call Clojure/Java properly.
* Future / Questions
    * Some implicit recur, but not too much focus on premature-TCO.
    * Revist using STM (refs/atoms) instead of inten for set/value.
* Exisiting Shen libraries and portability?
    * overrides.clj - rewriting parts of Shen into Clojure if interop or performance requires it.


#### The other port, Shen to Clojure

http://code.google.com/p/shen-to-clojure/

## License

http://shenlanguage.org/license.html

Shen, Copyright © 2010-2012 Mark Tarver

Shen.clj, Copyright © 2012 Håkan Råberg
