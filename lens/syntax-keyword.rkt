#lang racket

(require "base/main.rkt"
         fancy-app
         syntax/parse)

(module+ test
  (require rackunit))

(provide
 (contract-out
  [syntax-keyword-seq-lens (-> keyword? lens?)]))


(define-syntax-rule (syntax-parse/default-noop stx option-or-clause ...)
  (syntax-parse stx
    option-or-clause ...
    [default #'default]))


(define (syntax-keyword-seq-get keyword stx)
  (syntax-parse/default-noop stx
    [(a rest ...)
     (if (eq? (syntax->datum #'a) keyword)
         (syntax-get-until-keyword #'(rest ...))
         (syntax-keyword-seq-get keyword #'(rest ...)))]))


(define (syntax-get-until-keyword stx)
  (syntax-parse/default-noop stx
    [((~and before-kw (~not _:keyword)) ... a:keyword rest ...)
     #'(before-kw ...)]))


(define (syntax-keyword-seq-set keyword stx new-stx)
  (syntax-parse/default-noop stx
    [(a rest ...+)
     (if (eq? (syntax->datum #'a) keyword)
         (syntax-set-until-keyword keyword #'(rest ...) new-stx)
         #`(a #,@(syntax-keyword-seq-set keyword #'(rest ...) new-stx)))]))


(define (cons-stx v-stx vs-stx)
  (syntax-parse vs-stx
    [(v ...) #`(#,v-stx v ...)]))


(define (syntax-set-until-keyword keyword-val stx new-stx)
  (define new-stx-with-keyword (cons-stx keyword-val new-stx))
  (syntax-parse stx
    [((~and before-kw (~not _:keyword)) ... a:keyword rest ...)
     #`(#,@(syntax->list new-stx-with-keyword) a rest ...)]
    [other new-stx-with-keyword]))


(define (syntax-keyword-seq-lens keyword)
  (define getter (syntax-keyword-seq-get keyword _))
  (define setter (syntax-keyword-seq-set keyword _ _))
  (make-lens getter setter))

(module+ test
  (define-check (check-stx-equal? actual-stx expected-stx)
    (check-equal? (syntax->datum actual-stx)
                  (syntax->datum expected-stx)))
  (define foo-kw-seq-lens (syntax-keyword-seq-lens '#:foo))
  (check-stx-equal? (lens-view foo-kw-seq-lens #'(a #:foo c d #:bar f))
                    #'(c d))
  (check-stx-equal? (lens-set foo-kw-seq-lens #'(a #:foo c d #:bar f) #'(1 2 3 4 5 6))
                    #'(a #:foo 1 2 3 4 5 6 #:bar f))
  (check-stx-equal? (lens-view foo-kw-seq-lens #'(a b f g))
                    #'())
  (check-stx-equal? (lens-view foo-kw-seq-lens #'(a #:foo #:bar f))
                    #'())
  (check-stx-equal? (lens-set foo-kw-seq-lens #'(a #:foo #:bar f) #'(1 2 3 4 5 6))
                    #'(a #:foo 1 2 3 4 5 6 #:bar f))
  (check-stx-equal? (lens-set foo-kw-seq-lens #'(a b f g) #'(these are ignored))
                    #'(a b f g)))
 