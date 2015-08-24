#lang racket/base

(provide reprovide)

(define-syntax-rule (reprovide mod-path ...)
  (begin
    (require mod-path ...)
    (provide (all-from-out mod-path ...))))
