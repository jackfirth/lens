#lang racket/base

(provide (all-from-out "main.rkt"))

(require "main.rkt")

(use-applicable-lenses!)

(module+ test
  (require rackunit)
  (check-equal? (first-lens '(1 2 3)) 1))
