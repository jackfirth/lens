#lang sweet-exp racket/base

require racket/contract
        lens/private/base/main
        lens/private/isomorphism/base
        lens/private/compound/compose
        lens/private/util/rest-contract
        "../util/immutable.rkt"
        "../list/join-list.rkt"

module+ test
  require rackunit
          lens/private/test-util/test-lens
          "../list/list-ref-take-drop.rkt"

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

