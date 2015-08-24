#lang racket/base

(require racket/contract
         "../base/main.rkt"
         "../compound/main.rkt"
         "../util/immutable.rkt"
         "../util/rest-contract.rkt"
         "ref.rkt")

(module+ test
  (require rackunit))

(provide
 (contract-out
  [vector-ref-nested-lens (rest-> exact-nonnegative-integer?
                                  (lens/c immutable-vector? any/c))]))


(define (vector-ref-nested-lens . is)
  (apply lens-thrush (map vector-ref-lens is)))

(module+ test
  (check-equal? (lens-transform (vector-ref-nested-lens 2 1)
                                #(a #(b c) #(d e f))
                                symbol->string)
                #(a #(b c) #(d "e" f))))

