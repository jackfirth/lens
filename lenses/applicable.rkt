#lang racket

(require "main.rkt"
         "core/main.rkt")

(use-applicable-lenses!)

(provide (all-from-out "main.rkt"))

(module+ test
  (require rackunit)
  (check-equal? (identity-lens 3) 3)
  (check-equal? (lens-view identity-lens 3) 3)
  (check-equal? (lens-set identity-lens 3 'a) 'a))
