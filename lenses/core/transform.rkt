#lang racket

(require "core.rkt")

(provide lens-transform)

(module+ test
  (require rackunit))


(define (lens-transform lens f v)
  (let-lens (view setter) lens v
    (setter (f view))))

(module+ test
  (define (set-first lst v)
    (list* v (rest lst)))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-transform first-lens number->string '(1 2 3)) '("1" 2 3)))

