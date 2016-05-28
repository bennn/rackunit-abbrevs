rackunit-abbrevs
================
[![Build Status](https://travis-ci.org/bennn/rackunit-abbrevs.svg)](https://travis-ci.org/bennn/rackunit-abbrevs)
[![Coverage Status](https://coveralls.io/repos/bennn/rackunit-abbrevs/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/rackunit-abbrevs?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/rackunit-abbrevs/index.html)

Abbreviations for iterated [RackUnit](http://docs.racket-lang.org/rackunit/api.html) tests.


Examples
--------

```
(check-true* (lambda (x y #:z [z 0]) (= (+ x y z) 3))
 [3 0]
 [1 2]
 [1 1 #:z 1])

(check-false* string?
 [1]
 ['lemons])

(check-apply* +
 [1 2 3
  => 6]
 [2 2
  != 5])

(check-exn* exn:fail:contract? +
 ['(1 2 3)]
 [1 "2 3"])
```

Test failures are reported in terms of the source location on each test:
```
#lang racket/base
(require rackunit rackunit-abbrevs)

(check-apply* +
 [1 1
  => 2]
 [2 2
  => 5])

;; --------------------
;; FAILURE
;; actual:     4
;; expected:   5
;; name:       check-equal?
;; expression: (check-equal? (+ 2 2) 5)
;; location:   (#<path:/tmp/example.rkt> 7 1 87 12)
;; 
;; Check failure
;; --------------------
```

See the tests in `./private/test-rackunit-abbrevs.rkt` for more examples.


Install
-------

Either:
```
  raco pkg install rackunit-abbrevs
```

Or:
```
  git clone https://github.com/bennn/rackunit-abbrevs;
  raco pkg install ./rackunit-abbrevs
```


Use
---

This library defines 3 macros:

##### `(check-true* f [arg* ..._0] ..._1)`
  Accepts a function `f` and a series of arguments `arg* ...` to `f`.
  Expands into a call `(f arg* ...)` for each parenthesized group of arguments `[arg* ...]`.
  The result of each call is passed to `check-true`.
##### `(check-false* f [arg* ...] ...)`
  Same as `check_true*`, but asserts that each call to `f` returns `#f`.
##### `(check-apply* f [arg* ... == r] ...)`
  Calls `(f arg* ...)` for each group of arguments.
  Results are compared to the matching `r` via `check-equal?`, or with
   `check-not-equal?` if `!=` is used instead of `==`.

At least `=>` and `==>` are synonyms for `==`.
At least `!=>` is a synonym for `!=`.


Notes
-----

You might also like Jay McCarthy's [rackunit-chk](https://github.com/jeapostrophe/rackunit-chk)
 and [chk](https://github.com/jeapostrophe/chk) packages.

