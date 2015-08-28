#lang racket/base

(require racket/contract
         racket/match
         unstable/sequence
         fancy-app
         "../base/main.rkt"
         "../util/alternating-list.rkt"
         "../util/immutable.rkt"
         "../util/list-pair-contract.rkt"
         "join-list.rkt")

(module+ test
  (require rackunit
           "../list/list-ref-take-drop.rkt"))

(provide
 (contract-out
  [lens-join/hash (->* () #:rest (listof2 any/c lens?) (lens/c any/c immutable-hash?))]))


(define (keys+values->hash keys vs)
  (make-immutable-hash (keys+values->assoc-list keys vs)))

(define (lens-join/hash . keys/lenses)
  (define-values [keys lenses] (alternating-list->keys+values keys/lenses))
  (define list-lens (apply lens-join/list lenses))
  (define (get target)
    (keys+values->hash keys (lens-view list-lens target)))
  (define (set target new-view-hash)
    (lens-set list-lens target (map (hash-ref new-view-hash _) keys)))
  (make-lens get set))

(module+ test
  (define a-b-lens (lens-join/hash 'b third-lens
                                   'a first-lens))
  (check-equal? (lens-view a-b-lens '(1 2 3))
                (hash 'a 1 'b 3))
  (check-equal? (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
                '(100 2 200)))

