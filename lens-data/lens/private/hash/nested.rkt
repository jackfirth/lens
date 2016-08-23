#lang racket/base

(require racket/contract/base
         lens/private/base/main
         lens/private/compound/main
         lens/private/util/rest-contract
         "../util/immutable.rkt"
         "ref.rkt")

(module+ test
  (require rackunit
           fancy-app))

(provide
 (contract-out
  [hash-ref-nested-lens (rest-> any/c (lens/c immutable-hash? any/c))]))


(define (hash-ref-nested-lens . keys)
  (apply lens-thrush (map hash-ref-lens keys)))

(module+ test
  (define a-x (hash-ref-nested-lens 'a 'x))
  (let-lens [val ctxt] a-x (hash 'a (hash 'x 1 'y 2) 'b (hash 'z 3))
    (check-equal? val 1)
    (check-equal? (ctxt 100) (hash 'a (hash 'x 100 'y 2) 'b (hash 'z 3))))
  (check-equal? (lens-transform/list (hash 'a (hash 'x 1 'y 2) 'b (hash 'z 3)) a-x (* 10 _))
                (hash 'a (hash 'x 10 'y 2) 'b (hash 'z 3))))
