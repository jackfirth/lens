#lang racket/base

(require reprovide/reprovide)
(reprovide "main.rkt")

(require (only-in "private/base/base.rkt" use-applicable-lenses!))

(module+ test
  (require rackunit))


(use-applicable-lenses!)

(module+ test
  (check-equal? (first-lens '(a b c)) 'a))
