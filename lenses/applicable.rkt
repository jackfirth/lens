#lang racket

(require "main.rkt"
         "core.rkt")

(lens-application-context? #t)

(provide (all-from-out "main.rkt"))

(module+ test
  (require rackunit)
  (check-equal? (identity-lens 3) 3))
