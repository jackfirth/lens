#lang racket/base

(provide hash-pluck-lens)

(require racket/list
         lens/base/main
         "hash.rkt"
         "join.rkt")
(module+ test
  (require rackunit))

(define (hash-pluck-lens . ks)
  (apply lens-join/hash
         (append-map
          (λ (k)
            (list k (hash-ref-lens k)))
          ks)))

(module+ test
  (check-equal? (lens-view (hash-pluck-lens 'a 'c) (hash 'a 1 'b 2 'c 3))
                (hash 'a 1 'c 3))
  (check-equal? (lens-set (hash-pluck-lens 'a 'c) (hash 'a 1 'b 2 'c 3) (hash 'a 4 'c 5))
                (hash 'a 4 'b 2 'c 5))
  )
