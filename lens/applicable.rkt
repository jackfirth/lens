#lang racket/base

(provide (all-from-out "main.rkt"))

(require "main.rkt"
         (only-in "base/base.rkt" use-applicable-lenses!))

(module+ test
  (require rackunit))


(use-applicable-lenses!)

(module+ test
  (check-equal? (first-lens '(a b c)) 'a))
