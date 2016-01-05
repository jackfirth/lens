#lang sweet-exp racket/base

require racket/contract
        unstable/lens/isomorphism/base
        "../base/main.rkt"
        "../util/immutable.rkt"
        "../util/rest-contract.rkt"
        "compose.rkt"
        "../list/join-list.rkt"

module+ test
  require rackunit
          "../list/list-ref-take-drop.rkt"
          "../test-util/test-lens.rkt"

provide
  contract-out
    lens-join/vector (rest-> lens? (lens/c any/c immutable-vector?))


(define (lens-join/vector . lenses)
  (lens-compose list->vector-lens (apply lens-join/list lenses)))

(define list->vector-lens
  (make-isomorphism-lens list->immutable-vector vector->list))

(module+ test
  (define vector-first-third-fifth-lens
    (lens-join/vector first-lens
                      third-lens
                      fifth-lens))
  (check-lens-view vector-first-third-fifth-lens '(a b c d e f)
                #(a c e))
  (check-pred immutable? (lens-view vector-first-third-fifth-lens '(a b c d e f)))
  (check-lens-set vector-first-third-fifth-lens '(a b c d e f) #(1 2 3)
                  '(1 b 2 d 3 f)))

