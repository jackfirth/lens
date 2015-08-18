#lang racket

(provide
 (contract-out [car-lens (lens/c pair? any/c)]
               [cdr-lens (lens/c pair? any/c)]))

(require "../base/main.rkt")

(module+ test
  (require rackunit
           "../test-util/test-lens.rkt"))


(define (set-car pair v)
  (cons v (cdr pair)))

(define (set-cdr pair v)
  (cons (car pair) v))

(define car-lens (make-lens car set-car))
(define cdr-lens (make-lens cdr set-cdr))

(module+ test
  (check-view car-lens '(1 . 2) 1)
  (check-set car-lens '(1 . 2) 'a '(a . 2))
  (test-lens-laws car-lens '(1 . 2) 'a 'b)

  (check-view cdr-lens '(1 . 2) 2)
  (check-set cdr-lens '(1 . 2) 'a '(1 . a))
  (test-lens-laws cdr-lens '(1 . 2) 'a 'b))
