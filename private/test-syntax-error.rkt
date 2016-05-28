#lang racket/base
(require rackunit rackunit-abbrevs)

;; 2016-05-28: For now, manully inspect the output of these

;; =============================================================================

;(check-true* (lambda (x) #t)
;  4
;)
;
;(check-false*
;  4
;  5
;)
;(check-false* (lambda (x) #t)
;  4
;)
;
;(check-exn* (lambda (x) #t)
;  4
;  5
;)
;(check-apply* +
; 1 1
;  => 2
; [2 2
;  => 5])
;(check-apply* +
; [1 1
;  => 2]
; [2 2
;  => 5])
