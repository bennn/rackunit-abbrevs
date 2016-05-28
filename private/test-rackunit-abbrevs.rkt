#lang racket/base

(module+ test
  (require rackunit rackunit-abbrevs)

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

