#lang racket/base

(require racket/contract
         racket/list
         racket/match)

(provide
 (contract-out [listof2 (-> contract? contract? contract?)]))


(define (list*/c . contracts)
  (match contracts
    [(list end-contract)
     end-contract]
    [(list* head-contract rest-contracts)
     (cons/c head-contract
             (apply list*/c rest-contracts))]))

(define (repeating-list/c . contracts)
  (define c
    (or/c empty?
          (apply list*/c (append contracts (list (recursive-contract c))))))
  c)

(define (listof2 first-val/c second-val/c)
  (repeating-list/c first-val/c second-val/c))
