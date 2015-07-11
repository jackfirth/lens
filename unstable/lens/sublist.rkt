#lang racket/base

(provide sublist-lens)

(require lens/base/main
         lens/list/main
         )
(module+ test
  (require rackunit))

(define (sublist-lens i j)
  (lens-thrush (take-lens j) (drop-lens i)))

(module+ test
  (check-equal? (lens-view (sublist-lens 1 4) '(0 1 2 3 4 5))
                '(1 2 3))
  (check-equal? (lens-set (sublist-lens 1 4) '(0 1 2 3 4 5) '(a b c))
                '(0 a b c 4 5))
  )
