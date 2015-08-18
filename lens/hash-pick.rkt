#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [hash-pick-lens
           (->* [] #:rest list? (lens/c immutable-hash? immutable-hash?))]
          ))

(require racket/list
         lens/base/main
         lens/util/immutable
         "hash.rkt"
         "join.rkt")

(module+ test
  (require rackunit))

(define (hash-pick-lens . ks)
  (apply lens-join/hash
         (append-map
          (λ (k)
            (list k (hash-ref-lens k)))
          ks)))

(module+ test
  (check-equal? (lens-view (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3))
                (hash 'a 1 'c 3))
  (check-equal? (lens-set (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3) (hash 'a 4 'c 5))
                (hash 'a 4 'b 2 'c 5))
  )
