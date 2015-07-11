#lang racket/base

(provide define-struct-lenses struct/lens)

(require syntax/parse/define
         lens/base/main
         alexis/util/struct
         (submod alexis/util/struct get-struct-accessors)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/struct-info
                     ))
(module+ test
  (require rackunit fancy-app))

(define-syntax define-struct-lenses
  (syntax-parser
    [(define-struct-lenses s:id)
     #:do [(define-values (ids1 ids2)
             (get-struct-accessors (extract-struct-info (syntax-local-value #'s)) #'s))]
     #:with [s-fld:id ...] ids2
     #:with [[s-fld-set s-fld-lens] ...]
     (for/list ([s-fld (in-list ids2)])
       (list (format-id #'s "~a-set" s-fld #:source s-fld)
             (format-id #'s "~a-lens" s-fld #:source s-fld)))
     #'(begin
         (define-struct-updaters s)
         (define s-fld-lens (make-lens s-fld s-fld-set))
         ...)]))

(define-simple-macro (struct/lens s:id (field-spec ...) option ...)
  (begin
    (struct s (field-spec ...) option ...)
    (define-struct-lenses s)))

(module+ test
  (require rackunit)
  (struct/lens foo (a b c) #:transparent)
  (check-equal? (lens-transform foo-a-lens (foo 1 2 3) (* 100 _))
                (foo 100 2 3))
  (struct bar foo (d e f) #:transparent)
  (define-struct-lenses bar)
  (check-equal? (lens-transform bar-d-lens (bar 1 2 3 4 5 6) (* 100 _))
                (bar 1 2 3 400 5 6))
  )
