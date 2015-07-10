#lang racket

(provide lenses-examples
         lenses-applicable-examples
         lenses-unstable-examples)

(require scribble/eval)


(define-syntax-rule (define-examples-form id require-spec ...)
  (begin
    (define (eval-factory)
      (define base-eval (make-base-eval))
      (base-eval '(require require-spec)) ...
      base-eval)
    (define-syntax-rule (id datum (... ...))
      (examples #:eval (eval-factory) datum (... ...)))))


(define-examples-form lenses-examples
  lens racket/list)

(define-examples-form lenses-applicable-examples
  lens/applicable racket/list)

(define-examples-form lenses-unstable-examples
  lens unstable/lens racket/list)
