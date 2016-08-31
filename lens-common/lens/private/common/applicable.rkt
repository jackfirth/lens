#lang racket/base

(require reprovide/reprovide)
(reprovide lens/common)

(require (only-in lens/private/base/base use-applicable-lenses!))

(module+ test
  (require rackunit))

(use-applicable-lenses!)

(module+ test
  (define (set-car p a)
    (cons a (cdr p)))
  (define car-lens (make-lens car set-car))
  (check-equal? (car-lens '(a b c)) 'a)
  (check-equal? (lens-view car-lens '(a b c)) 'a)
  (check-equal? (lens-set car-lens '(a b c) 97) '(97 b c)))
