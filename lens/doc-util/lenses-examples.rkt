#lang racket

(provide lenses-examples
         lenses-applicable-examples
         lenses-unstable-examples
         define-persistant-lenses-unstable-examples)

(require scribble/eval
         racket/splicing)


(define-syntax-rule (define-examples-form id require-spec ...)
  (begin
    (define (eval-factory)
      (define base-eval (make-base-eval))
      (base-eval '(require require-spec)) ...
      base-eval)
    (define-syntax-rule (id datum (... ...))
      (examples #:eval (eval-factory) datum (... ...)))))

(define-syntax-rule (define-examples/persistance-syntax id require-spec ...)
  (begin
    (define (eval-factory)
      (define base-eval (make-base-eval))
      (base-eval '(require require-spec)) ...
      base-eval)
    (define-syntax-rule (id examples-id)
      (begin
        (splicing-let ([the-eval (eval-factory)])
          (define-syntax-rule (examples-id datum (... (... ...)))
            (examples #:eval the-eval datum (... (... ...)))))))))

(define-examples-form lenses-examples
  lens racket/list racket/vector racket/stream)

(define-examples-form lenses-applicable-examples
  lens/applicable racket/list racket/vector racket/stream)

(define-examples-form lenses-unstable-examples
  lens unstable/lens racket/list racket/vector racket/stream)

(define-examples/persistance-syntax define-persistant-lenses-unstable-examples
  lens unstable/lens racket/list racket/vector racket/stream)
