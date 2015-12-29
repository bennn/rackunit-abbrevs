#lang scribble/manual
@require[racket/include]
@require[scribble/eval]

@title[#:tag "top"]{RackUnit Abbrevs}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[rackunit-abbrevs]
@(define abbreval (make-base-eval))
@(interaction-eval #:eval abbreval (require rackunit-abbrevs))

A few helpers for writing many unit tests on a single function.
The macros here repeatedly apply a standard RackUnit assertion.
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

@defform[(check-apply* f [arg* ... (~or == !=) result] ...)]{
  Apply @racket[f] to each sequence of arguments @racket[arg*] and compare to @racket[result] using @racket[equal?].
  More precisely, calls @racket[check-equal? (f arg* ...) result] if @racket[sym] is @racket[==] and calls @racket[check-not-equal? (f arg* ...) result] if @racket[sym] is @racket[!=].
}

@examples[#:eval abbreval
(check-apply* map
 [add1 '(1 2 3) == '(2 3 4)]
 [append '((c)) '((a)) '((t)) != '((d o g))])
]

@defform[(check-exn* p f [arg* ...] ...)]{
  Apply @racket[f] to the arguments @racket[arg*] and assert @racket[(check-exn p (lambda () (f arg*)))] for each.
}

@examples[#:eval abbreval
(check-exn* exn:fail:contract? vector-ref
 [0 #'()]
 ["hi"])

(check-exn* "\\+: contract violation" +
 [0 #\0 'O]
 ['() '()])
]


@section{Notes}

You might also like Jay McCarthy's @hyperlink["https://github.com/jeapostrophe/rackunit-chk"]{rackunit-chk}.


