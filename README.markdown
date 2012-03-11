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

Is a work in progress. The example on the Shen homepage works, and the test suite is currently running: 97% passed.

Uses [Leiningen](https://github.com/technomancy/leiningen) to build.

**0.1.0 will be released once the test suite passes.**

### To run the REPL:

    lein trampoline run

    # If shen.clj already exists, for readline support:
    lein repl

    Shen 2010, copyright (C) 2010 Mark Tarver
    www.lambdassociates.org, version 3.1
    running under Clojure, implementation: Clojure 1.4.0-beta4 [jvm 1.8.0-ea]
    port 0.1.0-SNAPSHOT ported by Håkan Råberg


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


### Known Issues

The Shen test suite is now running, slowly but surely:

    [... loads of output ...]
    passed ... 142
    failed ...4
    pass rate ...97.26027397260275%

    ok
    0

    run time: 18.337 secs
    loaded

The suite can be run via:

    yes | lein trampoline run -m shen.test


* 4 failing tests:
  * 3 `tc+` tests fails with `type-error in rule`.
  * [`einstein.shen`](https://github.com/hraberg/shen.clj/blob/master/shen/test-programs/einstein.shen) returns `false` instead of `german`.
* Cannot print empty vectors (vector 0).
* Performance is not a goal for 0.1.0, but some tuning has been made to ease development.
* Next step: [`benchmarks.shen`](https://github.com/hraberg/shen.clj/blob/master/shen/benchmarks/benchmarks.shen) running without errors.

### 神 and define macros

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

See [`shen.test`](https://github.com/hraberg/shen.clj/blob/master/test/shen/test.clj) for more examples.


### Leiningen

`[shen.clj "0.1.0-SNAPSHOT"]` - not yet in public repo.

The script `build` is used for full, repeatable builds.


### Marginalia

http://ghettojedi.org/shen.clj/


## Roadmap

This port, while aiming to conform closely (and hopefully fully) to the [Shen specification](http://shenlanguage.org/Documentation/shendoc.htm), has its primary goal to enable Shen's power in real world Clojure code.

* Test suite passing. (days to week/s)
* [Shen in 15 minutes](http://www.shenlanguage.org/learn-shen/tutorials/shen_in_15mins.html#shen-in-15mins) as smoke test for the REPL
* Shen / Clojure interop:
  * Shen packages as namespaces?
  * Hiding KLambda and its names a bit.
  * Bringing smaller parts of Shen goodness back into Clojure: predicate dispatch, pattern matching, prolog. Maybe even the type system.
  * Ensuring Shen can call Clojure/Java properly.
* Docstrings for Shen, maybe from [shen-mode.el](https://github.com/eschulte/shen-mode/blob/master/shen-mode.el).
* Future / Questions
  * More TCO than implicit recur for KLambda?
  * Making Shen as lazy as its host?
  * Revisit using STM (refs/atoms) instead of intern for set/value.
  * Existing Shen libraries and portability?
  * ClojureScript.
  * overwrite.clj - rewriting parts of Shen into Clojure if interop or performance requires it.
  * This project is part of my long term wish for a semi-automated global socialistic utopia, which requires some further work.


#### The other port, Shen to Clojure

http://code.google.com/p/shen-to-clojure/

## License

http://shenlanguage.org/license.html

Shen, Copyright © 2010-2012 Mark Tarver

shen.clj, Copyright © 2012 Håkan Råberg
