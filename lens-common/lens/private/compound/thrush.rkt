#lang racket/base

(require racket/contract
         racket/list
         fancy-app
         "../base/main.rkt"
         "compose.rkt")

(module+ test
  (require rackunit
           "../test-util/test-lens.rkt"))

(provide
 (contract-out [lens-thrush (->* () () #:rest (listof lens?) lens?)]))


(define (lens-thrush . args)
  (apply lens-compose (reverse args)))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (define test-alist '((a 1) (b 2) (c 3)))
  (define first-of-second-lens (lens-thrush second-lens first-lens))
  (check-lens-view first-of-second-lens test-alist 'b)
  (check-lens-set first-of-second-lens test-alist 'B '((a 1) (B 2) (c 3))))
