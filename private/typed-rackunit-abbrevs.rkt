#lang typed/racket/base

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
  typed/rackunit
  (for-syntax typed/racket/base syntax/parse syntax/stx)
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

