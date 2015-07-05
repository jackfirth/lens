#lang racket/base

(provide (all-defined-out)
         lens/c let-lens lens-view lens-set lens-transform lens-struct lens-proc)

(require (prefix-in - "main.rkt")
         (only-in "main.rkt"
                  lens/c let-lens lens-view lens-set lens-transform lens-struct lens-proc))

(define (make-lens getter setter)
  (lens-struct (-make-lens getter setter)))

(define (lens-compose . args)
  (lens-struct (apply -lens-compose args)))

(define identity-lens
  (lens-struct -identity-lens))

(define (list-lens n)
  (lens-struct (-list-lens n)))

(define first-lens (lens-struct -first-lens))
(define second-lens (lens-struct -second-lens))
(define third-lens (lens-struct -third-lens))
(define fourth-lens (lens-struct -fourth-lens))
(define fifth-lens (lens-struct -fifth-lens))

(define (assoc-lens key #:is-equal? [key-equal? equal?])
  (lens-struct (-assoc-lens key #:is-equal? key-equal?)))

(define (assv-lens key)
  (lens-struct (-assv-lens key)))

(define (assq-lens key)
  (lens-struct (-assq-lens key)))

(define-syntax-rule (syntax-lens target-id pattern)
  (lens-struct (-syntax-lens target-id pattern)))

(define-syntax-rule (syntax-keyword-seq-lens kw)
  (lens-struct (-syntax-keyword-seq-lens kw)))

