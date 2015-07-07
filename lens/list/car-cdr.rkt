#lang racket/base

(provide car-lens cdr-lens)

(require "../base/main.rkt")

(module+ test
  (require rackunit))


(define (set-car pair v)
  (cons v (cdr pair)))

(define (set-cdr pair v)
  (cons (car pair) v))

(define car-lens (make-lens car set-car))
(define cdr-lens (make-lens cdr set-cdr))

(module+ test
  (check-equal? (lens-view car-lens '(1 . 2)) 1)
  (check-equal? (lens-view cdr-lens '(1 . 2)) 2))
