#lang racket/base

(require racket/contract/base)
(provide
 (contract-out [car-lens (lens/c pair? any/c)]
               [cdr-lens (lens/c pair? any/c)]))

(require "../base/main.rkt" "../base/rename.rkt")

(module+ test
  (require rackunit
           "../test-util/test-lens.rkt"))


(define (set-car pair v)
  (cons v (cdr pair)))

(define (set-cdr pair v)
  (cons (car pair) v))

(define car-lens (lens-rename (make-lens car set-car) 'car-lens))
(define cdr-lens (lens-rename (make-lens cdr set-cdr) 'cdr-lens))

(module+ test
  (check-lens-view car-lens '(1 . 2) 1)
  (check-lens-set car-lens '(1 . 2) 'a '(a . 2))
  (test-lens-laws car-lens '(1 . 2) 'a 'b)

  (check-lens-view cdr-lens '(1 . 2) 2)
  (check-lens-set cdr-lens '(1 . 2) 'a '(1 . a))
  (test-lens-laws cdr-lens '(1 . 2) 'a 'b))
