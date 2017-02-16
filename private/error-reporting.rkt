#lang racket/base

(provide
  syntax->location
  procedure
  exn-predicate
  args-and-result-pattern
)

(require
  racket/syntax
  syntax/parse
  (only-in syntax/srcloc [build-source-location-list syntax->location])
)

;; =============================================================================

(define-syntax-class procedure
  ;; cat in the hat style
  (pattern (~not (~or _:number _:boolean _:char _:str _:keyword))))

(define-syntax-class exn-predicate
  ;; exn-predicate is a regular expression or a predicate function
  (pattern (~not (~or _:number _:boolean _:char _:str _:keyword))))

(define-syntax-class args-and-result-pattern
  #:attributes (arg* check-fn? result)
  #:datum-literals (!= !=>
                    == => ==>)
  (pattern [args ...
            (~or (~and (~or != !=>)
                       (~var make-fail))
                 (~and (~or == => ==>)
                       (~var make-pass)))
            res]
   #:attr arg* #'(args ...)
   #:attr check-fn? (if (attribute make-pass)
                        (format-id #'res "check-equal?")
                        (format-id #'res "check-not-equal?"))
   #:attr result #'res))
