#lang racket/base

(require racket/contract
         lens/private/base/main
         lens/private/compound/main
         lens/private/util/rest-contract
         "../util/immutable.rkt"
         "../vector/join-vector.rkt"
         "ref.rkt")

(module+ test
  (require rackunit lens/private/test-util/test-lens))

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
