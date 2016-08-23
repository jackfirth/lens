#lang sweet-exp racket/base

require racket/list
        racket/contract
        lens/private/base/main
        lens/private/util/alternating-list
        lens/private/util/rest-contract

module+ test
  require rackunit
          "../list/list-ref-take-drop.rkt"
          lens/private/test-util/test-lens

provide
  contract-out
    lens-join/list (rest-> lens? (lens/c any/c list?))


(define (lens-join/list . lenses)
  (define (get target)
    (apply lens-view/list target lenses))
  (define (set target new-views)
    (apply lens-set/list target (keys+values->alternating-list lenses new-views)))
  (make-lens get set))


(module+ test
  (define first-third-fifth-lens
    (lens-join/list first-lens
                    third-lens
                    fifth-lens))
  (check-lens-view first-third-fifth-lens '(a b c d e f)
                   '(a c e))
  (check-lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3)
                  '(1 b 2 d 3 f)))
