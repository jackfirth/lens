#lang racket/base

(require racket/contract
         "../base/main.rkt"
         "../util/immutable.rkt"
         "compose.rkt"
         unstable/lens/isomorphism/base
         "join-list.rkt")

(module+ test
  (require rackunit
           "../list/list-ref-take-drop.rkt"))

(provide
 (contract-out
  [lens-join/vector (->* () #:rest (listof lens?) (lens/c any/c immutable-vector?))]))


(define (lens-join/vector . lenses)
  (lens-compose list->vector-lens (apply lens-join/list lenses)))

(define list->vector-lens
  (isomorphism-lens list->immutable-vector vector->list))

(module+ test
  (define vector-first-third-fifth-lens
    (lens-join/vector first-lens
                      third-lens
                      fifth-lens))
  (check-equal? (lens-view vector-first-third-fifth-lens '(a b c d e f))
                #(a c e))
  (check-pred immutable? (lens-view vector-first-third-fifth-lens '(a b c d e f)))
  (check-equal? (lens-set vector-first-third-fifth-lens '(a b c d e f) #(1 2 3))
                '(1 b 2 d 3 f)))

