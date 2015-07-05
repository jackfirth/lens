#lang racket

(require "core.rkt")

(provide lens-view
         lens-set)

(module+ test
  (require rackunit))


(define (lens-view lens v)
  (let-lens (view _) lens v
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) lens v
    (setter x)))

(module+ test
  (define (set-first lst v)
    (list* v (rest lst)))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3)))


