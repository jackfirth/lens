#lang racket/base

(require racket/contract
         "../base/main.rkt"
         "../compound/main.rkt"
         "../util/immutable.rkt"
         "../util/rest-contract.rkt"
         "ref.rkt")

(module+ test
  (require rackunit "../test-util/test-lens.rkt"))

(provide
 (contract-out
  [vector-pick-lens (rest-> exact-nonnegative-integer?
                            (lens/c immutable-vector? immutable-vector?))]))


(define (vector-pick-lens . is)
  (apply lens-join/vector (map vector-ref-lens is)))

(module+ test
  (define 1-5-6-lens (vector-pick-lens 1 5 6))
  (check-lens-view 1-5-6-lens #(a b c d e f g)
                   #(b f g))
  (check-lens-set 1-5-6-lens #(a b c d e f g) #(1 2 3)
                  #(a 1 c d e 2 3)))
