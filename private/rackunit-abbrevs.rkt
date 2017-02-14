#lang racket/base

(provide
  check-true*
  ;; (check-true* f [arg* ...] ...)
  ;; Expand into `check-true` tests applying `f` to arguments `[arg* ...]`

  check-false*
  ;; (check-false* f [arg* ...] ...)
  ;; Expand into `check-false` tests applying `f` to arguments `[arg* ...]`

  check-apply*
  ;; (check-apply* f [arg* ... => res] ...)
  ;; Expand into `check-equal?` matching `f arg* ...` against `res`
  ;; If delimiter symbol `=>` is `!=`, use `check-not-equal?` instead

  check-exn*
  ;; (check-exn* p f [arg* ...] ...)
  ;; Assert that each call `(f arg* ...)` raises an exception matching
  ;;  the predicate `p`.
)

;; -----------------------------------------------------------------------------

(require
  rackunit
  (for-syntax
    rackunit-abbrevs/private/error-reporting
    rackunit
    racket/base
    syntax/parse
    syntax/stx))

;; -----------------------------------------------------------------------------

(define-for-syntax (make-simple-check check-fn-stx)
  (syntax-parser
    [(_ f-expr:procedure arg** ...+)
     #:with f (gensym (if (identifier? #'f-expr) (syntax-e #'f-expr) 'check-apply-proc))
     (quasisyntax/loc #'f-expr
       (let ([f f-expr])
         #,@(for/list ([stx (in-list (syntax-e #'(arg** ...)))])
           (unless (stx-list? stx)
             (raise-argument-error
               (string->symbol (format "~a*" (syntax-e check-fn-stx)))
               (format "List of arguments to '~a'" (syntax->datum #'f))
               (syntax->datum stx)))
           (quasisyntax/loc stx
             (with-check-info* (list (make-check-location '#,(syntax->location stx)))
               (lambda () (#,check-fn-stx (f #,@stx))))))))]))

(define-syntax check-true*
  (make-simple-check #'check-true))

(define-syntax check-false*
  (make-simple-check #'check-false))

;; 2016-05-28: should be able to re-use common structure in check-true/false
(define-syntax (check-exn* stx)
  (syntax-parse stx
   [(_ p:exn-predicate f-expr:procedure arg** ...+)
    #:with f (gensym (if (identifier? #'f-expr) (syntax-e #'f-expr) 'check-apply-proc))
    (quasisyntax/loc stx
      (let ([f f-expr])
        #,@(for/list ([stx (in-list (syntax-e #'(arg** ...)))])
          (unless (stx-list? stx)
            (raise-argument-error
              'check-exn*
              (format "List of arguments to '~a'" (syntax->datum #'f))
              (syntax->datum stx)))
         (quasisyntax/loc stx
           (with-check-info* (list (make-check-location '#,(syntax->location stx)))
             (lambda () (check-exn p (lambda () (f #,@stx)))))))))]))

(define-syntax (check-apply* stx)
  (syntax-parse stx
   [(_ f-expr:procedure case* ...+)
    #:with f (gensym (if (identifier? #'f-expr) (syntax-e #'f-expr) 'check-apply-proc))
    (quasisyntax/loc stx
      (let ([f f-expr])
        #,@(for/list ([stx (in-list (syntax-e #'(case* ...)))])
          (syntax-parse stx
           [case:args-and-result-pattern
            (quasisyntax/loc stx
              (with-check-info* (list (make-check-location '#,(syntax->location stx)))
                (lambda ()
                  (case.check-fn?  (f #,@#'case.arg*) case.result))))]))))]))

