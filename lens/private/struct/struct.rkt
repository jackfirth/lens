#lang sweet-exp racket/base

provide define-struct-lenses
        struct/lens
        struct-lenses-out
        struct+lenses-out

require syntax/parse/define
        alexis/util/struct
        racket/provide-syntax
        submod alexis/util/struct get-struct-accessors
        "../base/main.rkt"
        for-syntax racket/base
                   syntax/parse
                   racket/syntax
                   racket/struct-info

module+ test
  require rackunit
          fancy-app
          "../test-util/test-lens.rkt"

(begin-for-syntax

  (define (derive-id format id)
    (format-id id format id))
  
  (define (get-struct-field-ids struct-info failure-context)
    (define-values (_ field-ids)
      (get-struct-accessors struct-info failure-context))
    field-ids)

  (define (get-struct-id-field-ids struct-id-stx)
    (define info (extract-struct-info (syntax-local-value struct-id-stx)))
    (get-struct-field-ids info struct-id-stx))

  (define (map-format-id lex-context format-str ids)
    (define (format-one-id id)
      (format-id lex-context format-str id #:source id))
    (map format-one-id ids))

  (define (struct-get-set-lens-ids struct-id-stx)
    (define field-ids (get-struct-id-field-ids struct-id-stx))
    (define set-ids (map-format-id struct-id-stx "~a-set" field-ids))
    (define lens-ids (map-format-id struct-id-stx "~a-lens" field-ids))
    (list field-ids set-ids lens-ids))

  (struct struct-lens-info (field-lens-ids))

  (define (struct-field-lens-ids struct-id-stx)
    (define struct-lens:id-stx (derive-id "struct-lens:~a" struct-id-stx))
    (struct-lens-info-field-lens-ids (syntax-local-value struct-lens:id-stx))))

(define-syntax-rule (define-struct-lens-info id (field-lens-id ...))
  (define-syntax id
    (struct-lens-info (list #'field-lens-id ...))))

(define-syntax define-struct-lenses
  (syntax-parser
    [(define-struct-lenses s:id)
     #:with [(s-fld ...)
             (s-fld-set ...)
             (s-fld-lens ...)] (struct-get-set-lens-ids #'s)
     #:with s-lens-info (derive-id "struct-lens:~a" #'s)
     #'(begin
         (define-struct-updaters s)
         (define s-fld-lens (make-lens s-fld s-fld-set))
         ...
         (define-struct-lens-info s-lens-info (s-fld-lens ...)))]))


(define-simple-macro (struct/lens s:id (field-spec ...) option ...)
  (begin
    (struct s (field-spec ...) option ...)
    (define-struct-lenses s)))

(define-provide-syntax struct-lenses-out
  (syntax-parser
    [(struct-lenses-out struct-type:id)
     #:with [lens-id ...] (struct-field-lens-ids #'struct-type)
     #'(combine-out lens-id ...)]))

(define-provide-syntax struct+lenses-out
  (syntax-parser
    [(struct+lenses-out struct-type:id)
     #'(combine-out (struct-out struct-type) (struct-lenses-out struct-type))]))

(module+ test
  (struct/lens foo (a b c d) #:transparent)
  (check-lens-view foo-b-lens (foo 1 2 3 4) 2)
  (check-lens-set foo-c-lens (foo 1 2 3 4) 'a (foo 1 2 'a 4))
  (test-lens-laws foo-a-lens (foo 1 2 3 4) 'a 'b))
