#lang racket

(require "core.rkt")

(provide identity-lens)


(define (second-value _ v) v)
(define identity-lens (make-lens identity second-value))
