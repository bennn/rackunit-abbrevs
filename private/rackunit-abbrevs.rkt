#lang racket/base

(provide
  check-true*
  ;; (check-true* f [arg* ...] ...)
  ;; Desugar into `check-true` tests applying `f` to arguments `[arg* ...]`

  check-false*
  ;; (check-false* f [arg* ...] ...)
  ;; Desugar into `check-false` tests applying `f` to arguments `[arg* ...]`

  check-apply*
  ;; (check-apply* f [arg* ... == res] ...)
  ;; Desugar into `check-equal?` matching `f arg* ...` against `res`

  check-exn*
  ;; (check-exn* p f [arg* ...] ...)
  ;; Assert that each call `(f arg* ...)` raises an exception matching
  ;;  the predicate `p`.
)

;; -----------------------------------------------------------------------------

(require
  rackunit
  (for-syntax racket/base syntax/parse syntax/stx)
)

;; =============================================================================

;;bg; copied from rackunit library (location.rkt)
(define-for-syntax (syntax->location stx)
  (list (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define-syntax (check-true* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...+)
     (define loc (syntax->location stx))
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda () (check-true (f arg* ...)) ...)))]
    [_ (error 'check-true* "Expected (check-true* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-false* stx)
  (syntax-parse stx
    [(_ f [arg* ...] ...+)
     (define loc (syntax->location stx))
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda () (check-false (f arg* ...)) ...)))]
    [_ (error 'check-false* "Expected (check-false* f [arg* ...] ...). In other words, a function and parentheses-delimited lists of arguments.")]))

(define-syntax (check-apply* stx)
  (define loc (syntax->location stx))
  (syntax-parse stx #:datum-literals (== != =>)
    [(_ f [arg* ... (~or != == =>) res] ...+)
     ;; Well-formed call, map each [arg ... res] to a check
     (quasisyntax/loc stx
       (with-check-info* (list (make-check-location '#,loc))
         (lambda ()
           #,@(stx-map
             (lambda (s)
               (syntax-parse s #:datum-literals (== != =>)
                [[arg* ... (~or == =>) res]
                 (syntax/loc stx (check-equal? (f arg* ...) res))]
                [[arg* ... != res]
                 (syntax/loc stx (check-not-equal? (f arg* ...) res))]
                [_
                 (syntax/loc stx (void))]))
           stx))))]
    [_ (error 'check-apply* (format "~e\n    Expected (check-apply* f [arg* ... == res] ...) or (check-apply* f [arg* ... != res] ...). In other words, a function and parentheses-delimited lists of arguments & equality or dis-equality symbol & a result value to compare with.\n    Got ~a" loc (syntax->datum stx)))]))

(define-syntax (check-exn* stx)
  (define loc (syntax->location stx))
  (syntax-parse stx
   [(_ p f [arg* ...] ...+)
    (quasisyntax/loc stx
      (with-check-info* (list (make-check-location '#,loc))
        (lambda ()
          (check-exn p (lambda () (f arg* ...))) ...)))]
   [_ (error 'check-exn* (format "~e\n    Expected (check-exn* p f [arg* ...] ...)."))]))

;;; =============================================================================

(module+ test

  ;; -- check-true
  (check-true* (lambda (x) x)
    [#t]
    [#t]
  )

  (check-true* (lambda (x y) x)
    [#t #f]
    [#t 'a]
    [#t "yolo"]
  )

  (check-true* (lambda (x #:other-arg y) y)
    [#f #:other-arg #t]
  )

  (check-true* (lambda (x . ys*) (for/and ([y (in-list ys*)]) y))
    [#f #t]
    [#f #t #t #t]
    [#f #t #t #t #t]
  )

  (check-true* (lambda (x y #:z [z 0]) (= (+ x y z) 3))
   [3 0]
   [1 2]
   [1 1 #:z 1])

  ;; -- check-false*
  (check-false* (lambda (x) x)
    [#f]
    [#f]
  )

  (check-false* (lambda (x y) x)
    [#f #f]
    [#f 'a]
    [#f "yolo"]
  )

  (check-false* (lambda (x #:other-arg y) y)
    [#t #:other-arg #f]
  )

  (check-false* (lambda (x . ys*) (for/and ([y (in-list ys*)]) y))
    [#f #f]
    [#f #t #f #t]
    [#f #t #t #f #t]
  )

  (check-false* string?
   [1]
   ['lemons])

  ;; -- check-apply
  (check-apply* (lambda (x) x)
    [#t == #t]
    [#f == #f]
    ['A == 'A]
  )

  (check-apply* (lambda (x . y) (car y))
    ['A 'B '() == 'B]
  )

  (check-apply* (lambda ANY (cdr ANY))
    [1 2 3 4 == '(2 3 4)]
  )

  (check-apply* (lambda (a #:b b #:c [c #f]) (or c b))
    [1 #:b 2 == 2]
    [1 #:b 2 #:c 3 == 3]
  )

  (check-apply* (lambda (x) x)
    [1 != 2]
    [1 != 3]
    [1 != -1]
  )

  (check-apply* (lambda (x) x)
    [1 == 1]
    [1 != 2]
    [2 == 2]
    [3 != 0])

  (check-apply* +
   [1 2 3 == 6]
   [2 2 3 => 7]
   [2 2   != 5])

  ;; -- check-exn
  (check-exn* exn:fail:contract? +
   [1 "two"]
   ['yolo])

  (check-exn* #rx"\\+: contract violation" +
   [1 "two"]
   ["three"]
   ['1 '2 '(3)])

)
