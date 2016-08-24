#lang racket/base

(require scribble/manual)

(provide lens-tech)

(define (lens-tech . pre-content)
  (apply tech #:key "lens" #:normalize? #f pre-content))
