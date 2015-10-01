rackunit-abbrevs
================
[![Build Status](https://travis-ci.org/bennn/rackunit-abbrevs.svg)](https://travis-ci.org/bennn/rackunit-abbrevs)
[![Coverage Status](https://coveralls.io/repos/bennn/rackunit-abbrevs/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/rackunit-abbrevs?branch=master)

Abbreviations for iterated [RackUnit](http://docs.racket-lang.org/rackunit/api.html) tests.

This is a small collection of macros for applying one function many times.


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
 [1 2 3 == 6]
 [2 2   != 5])
```

See the test within `./private/rackunit-abbrevs.rkt` for more.


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
  Works great with keywords!
##### `(check-false* f [arg* ...] ...)`
  Same as `check_true*`, but asserts that each call to `f` returns `#f`.
##### `(check-apply* f [arg* ... == r] ...)`
  Similarly calls `(f arg* ...)` for each group of arguments.
  This time, results are compared to the matching `r` via `check-equal?`.
##### `(check-apply* f [arg* ... != r] ...)`
  Ditto, but using `check-not-equal?`.

Note: you can mix `==` and `!=` tests in a call to `check-apply*`.


Notes
-----

You might also like Jay McCarthy's [rackunit-chk](https://github.com/jeapostrophe/rackunit-chk) package.
