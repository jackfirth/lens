#lang racket/base

(require syntax/parse/define
         "../core/main.rkt"
         "car-cdr.rkt"
         (for-syntax racket/base
                     racket/syntax
                     ))
(module+ test
  (require rackunit))

(define (c_r->lens sym)
  (apply lens-compose
         (for/list ([char (in-string (symbol->string sym))])
           (case char [(#\a) car-lens] [(#\d) cdr-lens]))))

(define-simple-macro (define-c_r-lens id:id)
  #:with c_r-lens (format-id #'id "c~ar-lens" #'id #:source #'id #:props #'id)
  (begin (provide c_r-lens) (define c_r-lens (c_r->lens 'id))))

(define-simple-macro (define-c_r-lenses id:id ...)
  (begin (define-c_r-lens id) ...))

(define-c_r-lenses
  aa ad da dd
  aaa aad ada add
  daa dad dda ddd
  aaaa aaad aada aadd
  adaa adad adda addd
  daaa daad dada dadd
  ddaa ddad ddda dddd
  )

(module+ test
  (check-equal? (lens-transform cdaddr-lens list->vector '(9 8 (6 5 4 3 2 1) 7))
                '(9 8 (6 . #(5 4 3 2 1)) 7))
  )

