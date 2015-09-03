#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    isomorphism-compose
      (rest-> isomorphism-lens? isomorphism-lens?)
    isomorphism-thrush
      (rest-> isomorphism-lens? isomorphism-lens?)

require racket/match
        lens/private/util/rest-contract
        "base.rkt"
module+ test
  require lens/private/base/main
          lens/private/compound/identity
          rackunit
          "data.rkt"

(define (isomorphism-compose . args)
  (match args
    [(list (make-isomorphism-lens fs invs) ...)
     (make-isomorphism-lens
      (apply compose1 fs)
      (apply compose1 (reverse invs)))]))

(define (isomorphism-thrush . args)
  (apply isomorphism-compose (reverse args)))

module+ test
  (define string->vector-lens (isomorphism-thrush string->list-lens list->vector-lens))
  (check-equal? (lens-view string->vector-lens "abc") #(#\a #\b #\c))
  (check-equal? (lens-set string->vector-lens "abc" #(#\1 #\2 #\3)) "123")
