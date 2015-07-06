#lang racket

(provide lenses-examples
         lenses-applicable-examples)

(require scribble/eval)


(define-syntax-rule (define-examples-form id require-spec ...)
  (begin
    (define base-eval (make-base-eval))
    (base-eval '(require require-spec)) ...
    (define-syntax-rule (id datum (... ...))
      (examples #:eval base-eval datum (... ...)))))


(define-examples-form lenses-examples
  lenses racket/list)

(define-examples-form lenses-applicable-examples
  lenses/applicable racket/list)
