#lang racket/base

(provide lens-lambda)

(require syntax/parse/define
         "base.rkt")

(define-simple-macro (lens-lambda (target:id) body:expr ...+)
  (lens (lambda (target) body ...)))

