#lang racket/base

(require racket/contract
         syntax/parse/define
         "../base/main.rkt"
         "../compound/main.rkt"
         "car-cdr.rkt"
         (for-syntax racket/base
                     racket/syntax))

(module+ test
  (require rackunit))


(define-simple-macro (provide-c_r-lenses id:id ...)
  (begin (provide-c_r-lens id) ...))

(define-for-syntax (c_r-lens-id id-stx)
  (format-id id-stx "c~ar-lens" id-stx #:source id-stx #:props id-stx))

(define-simple-macro (provide-c_r-lens id:id)
  #:with c_r-lens (c_r-lens-id #'id)
  (provide (contract-out [c_r-lens lens?])))

(provide-c_r-lenses
 aa ad da dd
 aaa aad ada add
 daa dad dda ddd
 aaaa aaad aada aadd
 adaa adad adda addd
 daaa daad dada dadd
 ddaa ddad ddda dddd)


(define (c_r->lens sym)
  (apply lens-compose (c_r->lenses sym)))

(define (c_r->lenses sym)
  (map c_r-char->lens (symbol->chars sym)))

(define (c_r-char->lens char)
  (case char [(#\a) car-lens] [(#\d) cdr-lens]))

(define symbol->chars (compose string->list symbol->string))

(define-simple-macro (define-c_r-lens id:id)
  #:with c_r-lens (c_r-lens-id #'id)
  (define c_r-lens (c_r->lens 'id)))

(define-simple-macro (define-c_r-lenses id:id ...)
  (begin (define-c_r-lens id) ...))

(define-c_r-lenses
  aa ad da dd
  aaa aad ada add
  daa dad dda ddd
  aaaa aaad aada aadd
  adaa adad adda addd
  daaa daad dada dadd
  ddaa ddad ddda dddd)

(module+ test
  (check-equal? (lens-transform cdaddr-lens '(9 8 (6 5 4 3 2 1) 7) list->vector)
                '(9 8 (6 . #(5 4 3 2 1)) 7)))
