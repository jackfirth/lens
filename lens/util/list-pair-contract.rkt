#lang racket

(provide
 (contract-out [listof2 (-> contract? contract? contract?)]))


(define (listof2 first-val/c second-val/c)
  (define c
    (or/c empty? (cons/c first-val/c (cons/c second-val/c (recursive-contract c)))))
  c)
