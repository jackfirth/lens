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

(define (isomorphism-compose . args)
  (match args
    [(list (make-isomorphism-lens fs invs) ...)
     (make-isomorphism-lens
      (apply compose1 fs)
      (apply compose1 (reverse invs)))]))

(define (isomorphism-thrush . args)
  (apply isomorphism-compose (reverse args)))
