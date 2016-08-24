#lang sweet-exp racket/base

provide functional-set?

require racket/set
module+ test
  require rackunit

(define (functional-set? st)
  (and (generic-set? st)
       (set-implements? st 'set-add 'set-remove)
       (not (set-mutable? st))))

module+ test
  (check-true (functional-set? (set 1 2 3)))
  (check-true (functional-set? '(1 2 3)))
  (check-false (functional-set? (mutable-set 1 2 3)))
