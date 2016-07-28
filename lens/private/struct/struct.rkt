#lang racket/base

(require syntax/parse/define
         struct-update
         racket/provide-syntax
         "../base/main.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/class/struct-id
                     racket/syntax
                     racket/struct-info))

(module+ test
  (require rackunit
           fancy-app
           "../test-util/test-lens.rkt"))

(provide define-struct-lenses
         struct/lens
         struct-lenses-out
         struct+lenses-out)


(define-for-syntax (get-struct-own-accessor-ids struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.own-accessor-id)]))

(define-for-syntax (map-format-id lex-context format-str ids)
  (define (format-one-id id)
    (format-id lex-context format-str id #:source id))
  (map format-one-id ids))

(define-for-syntax (struct-get-set-lens-ids struct-id-stx)
  (define accessor-ids (get-struct-own-accessor-ids struct-id-stx))
  (define set-ids (map-format-id struct-id-stx "~a-set" accessor-ids))
  (define lens-ids (map-format-id struct-id-stx "~a-lens" accessor-ids))
  (list accessor-ids set-ids lens-ids))

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

(define-provide-syntax struct-lenses-out
  (syntax-parser
    [(struct-lenses-out struct-type:id)
     #:do [(define accessor-ids (get-struct-own-accessor-ids #'struct-type))]
     #:with [lens-id ...] (map-format-id #'struct-type "~a-lens" accessor-ids)
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
