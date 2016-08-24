#lang racket/base

(provide include-sections)

(require (only-in scribble/base include-section))

(define-syntax-rule (include-sections mod-path ...)
  (begin (include-section mod-path) ...))
