#lang sweet-exp racket/base

provide functional-dict?

require racket/dict
module+ test
  require rackunit

(define (functional-dict? v)
  (and (dict? v) (dict-can-functional-set? v)))

module+ test
  (check-true (functional-dict? (hash 'a 1 'b 2)))
  (check-true (functional-dict? '((a . 1) (b . 2))))
  (check-false (functional-dict? (make-hash '((a . 1) (b . 2)))))
