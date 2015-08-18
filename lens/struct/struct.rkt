#lang racket/base

(require syntax/parse/define
         lens/base/main
         alexis/util/struct
         (submod alexis/util/struct get-struct-accessors)
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/struct-info))

(module+ test
  (require rackunit
           fancy-app
           "../test-util/test-lens.rkt"))

(provide define-struct-lenses
         struct/lens)


(define-for-syntax (get-struct-field-ids struct-info failure-context)
  (define-values (_ field-ids)
    (get-struct-accessors struct-info failure-context))
  field-ids)

(define-for-syntax (get-struct-id-field-ids struct-id-stx)
  (define info (extract-struct-info (syntax-local-value struct-id-stx)))
  (get-struct-field-ids info struct-id-stx))

(define-for-syntax (map-format-id lex-context format-str ids)
  (define (format-one-id id)
    (format-id lex-context format-str id #:source id))
  (map format-one-id ids))

(define-for-syntax (struct-get-set-lens-ids struct-id-stx)
  (define field-ids (get-struct-id-field-ids struct-id-stx))
  (define set-ids (map-format-id struct-id-stx "~a-set" field-ids))
  (define lens-ids (map-format-id struct-id-stx "~a-lens" field-ids))
  (list field-ids set-ids lens-ids))

(define-syntax define-struct-lenses
  (syntax-parser
    [(define-struct-lenses s:id)
     #:with [(s-fld ...)
             (s-fld-set ...)
             (s-fld-lens ...)] (struct-get-set-lens-ids #'s)
     #'(begin
         (define-struct-updaters s)
         (define s-fld-lens (make-lens s-fld s-fld-set))
         ...)]))


(define-simple-macro (struct/lens s:id (field-spec ...) option ...)
  (begin
    (struct s (field-spec ...) option ...)
    (define-struct-lenses s)))

(module+ test
  (struct/lens foo (a b c d) #:transparent)
  (check-view foo-b-lens (foo 1 2 3 4) 2)
  (check-set foo-c-lens (foo 1 2 3 4) 'a (foo 1 2 'a 4))
  (test-lens-laws foo-a-lens (foo 1 2 3 4) 'a 'b))
