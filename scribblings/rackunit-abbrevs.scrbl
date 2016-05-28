#lang scribble/manual
@require[racket/include]
@require[scribble/eval]

@title[#:tag "top"]{RackUnit Abbrevs}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[rackunit-abbrevs]
@(define abbreval (make-base-eval))
@(interaction-eval #:eval abbreval (require rackunit rackunit-abbrevs))

For writing multiple unit tests on a single function.
The macros here repeatedly apply a standard RackUnit assertion on a sequence
 of argument (and result) pairs.

Test failures are reported at the unit test, not at the call to one of our
 @racket[check-] macros.


@section{API Functions}

@defform[(check-true* f [arg* ...] ...)]{
  Apply @racket[f] to each sequence of arguments @racket[arg*] and assert @racket[check-true] on each result.
}

@examples[#:eval abbreval
(check-true* (lambda (x y #:z [z 0]) (= (+ x y z) 3))
 [3 0]
 [1 2]
 [1 1 #:z 1])

(check-true* integer?
  ["hello"])
]

@defform[(check-false* f [arg* ...] ...)]{
  Apply @racket[f] to each sequence of arguments @racket[arg*] and assert @racket[check-false] on each result.
}

@examples[#:eval abbreval
(check-false* string?
 [1]
 ['lemons])

(check-false* =
 [2 2])
]

@defform[(check-apply* f [arg* ... (~or != !=> == => ==>) result] ...)]{
  Apply @racket[f] to each sequence of arguments @racket[arg*] and compare to @racket[result] using @racket[equal?].
  When @tt{==} or @tt{=>} or @tt{==>} is used as a delimiter, calls @racket[(check-equal? (f arg* ...) result)].
  Otherwise, calls @racket[(check-not-equal? (f arg* ...) result)].
}

@examples[#:eval abbreval
(check-apply* map
 [add1 '(1 2 3) == '(2 3 4)]
 [append '((c)) '((a)) '((t)) != '((d o g))])
]

@defform[(check-exn* exn-predicate f [arg* ...] ...)]{
  Apply @racket[f] to the arguments @racket[arg*] and assert @racket[(check-exn exn-predicate (lambda () (f arg*)))] for each.
}

@examples[#:eval abbreval
(check-exn* exn:fail:contract? vector-ref
 [0 #'()]
 ["hi"])

(check-exn* #rx"\\+: contract violation" +
 [0 #\0 'O]
 ['() '()])
]


@section{Library Functions}
@defmodule[rackunit-abbrevs/error-reporting]

These functions are internal to the library but may be useful elsewhere.

@defproc[(syntax->location [stx syntax?]) (list/c any/c (or/c number? #f) (or/c number? #f) (or/c number? #f) (or/c number? #f))]{
  Convert a syntax object to a list containing location information for RackUnit's
   @racket[make-check-location].
}

@defidform[procedure]{
  Syntax class used to rule out patterns that are definitely not procedures.
}

@defidform[exn-predicate]{
  Syntax class used to rule out patterns that are definitely not predicates
   or regular expressions.
}


@section{Notes}

You might also like Jay McCarthy's @hyperlink["https://github.com/jeapostrophe/rackunit-chk"]{rackunit-chk}
 and @hyperlink["https://github.com/jeapostrophe/chk"]{chk}.


