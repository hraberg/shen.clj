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

Is a work in progress. The example on the Shen homepage works, and the test suite is currently running: 79% passed.

Uses [Leiningen](https://github.com/technomancy/leiningen) to build.

**0.1.0 will be released once the test suite passes.**

### To run the REPL:

    lein trampoline run

    # If shen.clj already exists, for readline support:
    lein repl

    Shen 2010, copyright (C) 2010 Mark Tarver
    www.lambdassociates.org, version 3.1
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


### Known Issues

The Shen test suite is now running, slowly but surely:

    [... loads of output ...]
    passed ... 117
    failed ...31
    pass rate ...79.05405405405405%

    ok
    0

    run time: 254.07 secs
    loaded

The suite isn't part of the normal build yet, but can be run via:

    yes | lein trampoline run -m shen.test


* ~20% failures in the test suite.
* Most Prolog tests are broken, prompting `failed; continue?`.
* Many tc+ (typechecking) tests fails with `type-error`.
  * `#<shen$top shen$top@c6c2bc3> is not an operation on stacks` - symbol vs fn issue.
* `prime*` in `primes.shen` blows the stack unless increased.
  * Uses `-Xss256mb` to run for now (no TCO).
* It's currently 10 times slower than the CLisp version.
  * Performance is not a goal for 0.1.0, but some tuning will be made to ease development.


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



## Roadmap

This port, while aiming to conform closely (and hopefully fully) to the [Shen specification](http://shenlanguage.org/Documentation/shendoc.htm), has its primary goal to enable Shen's power in real world Clojure code.

* Test suite passing. (days to week/s)
* Shen / Clojure interop:
  * define macro to embed Shen in Clojure (if needed: https://github.com/klutometis/reader-macros)
  * Shen packages as namespaces?
  * Hiding KLambda and its names a bit.
  * Bringing smaller parts of Shen goodness back into Clojure: predicate dispatch, pattern matching, prolog. Maybe even the type system.
  * Ensuring Shen can call Clojure/Java properly.
* Docstrings for Shen, maybe from [shen-mode.el](https://github.com/eschulte/shen-mode/blob/master/shen-mode.el).
* Future / Questions
  * Some implicit recur, but not too much focus on premature TCO.
  * Revisit using STM (refs/atoms) instead of intern for set/value.
  * Existing Shen libraries and portability?
  * ClojureScript.
  * overrides.clj - rewriting parts of Shen into Clojure if interop or performance requires it.


#### The other port, Shen to Clojure

http://code.google.com/p/shen-to-clojure/

## License

http://shenlanguage.org/license.html

Shen, Copyright © 2010-2012 Mark Tarver

Shen.clj, Copyright © 2012 Håkan Råberg
