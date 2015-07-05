#lang racket

(require "core/main.rkt"
         fancy-app
         syntax/parse)

(provide syntax-keyword-seq-lens)


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
