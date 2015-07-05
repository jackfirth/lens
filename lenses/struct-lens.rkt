#lang racket/base

(provide struct/lens)

(require syntax/parse/define
         lenses/applicable
         alexis/util/struct
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))
(module+ test
  (require rackunit fancy-app (only-in lenses lens-transform*)))

(define-simple-macro (struct/lens s:id (fld:id ...) option ...)
  #:with [[s-fld s-fld-set s-fld-lens] ...]
  (for/list ([fld (in-list (syntax->list #'(fld ...)))])
    (list (format-id #'s "~a-~a" #'s fld #:source fld)
          (format-id #'s "~a-~a-set" #'s fld #:source fld)
          (format-id #'s "~a-~a-lens" #'s fld #:source fld)))
  (begin
    (struct s (fld ...) option ...)
    (define-struct-updaters s)
    (define s-fld-lens
      (make-lens s-fld s-fld-set))
    ...))

(module+ test
  (struct/lens foo (a b c) #:transparent)
  (define f (foo 1 2 3))
  (check-equal? (lens-transform* f foo-a-lens (* 100 _)) (foo 100 2 3))
  (check-equal? (lens-transform* f foo-b-lens (* 100 _)) (foo 1 200 3))
  (check-equal? (lens-transform* f foo-c-lens (* 100 _)) (foo 1 2 300))
  )
