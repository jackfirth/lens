#lang racket

(require "base.rkt")

(module+ test
  (require rackunit
           "view-set.rkt"))

(provide
 (contract-out [identity-lens lens?]))


(define (second-value _ v) v)

(define identity-lens
  (make-lens identity second-value))


(module+ test
  (check-equal? (lens-view identity-lens 'foo) 'foo)
  (check-equal? (lens-set identity-lens 'foo 'bar) 'bar))
