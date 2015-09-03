#lang sweet-exp racket/base

provide isomorphism-compose
        isomorphism-thrush

require racket/match
        "base.rkt"

(define (isomorphism-compose . args)
  (match args
    [(list (make-isomorphism-lens fs invs) ...)
     (make-isomorphism-lens
      (apply compose1 fs)
      (apply compose1 (reverse invs)))]))

(define (isomorphism-thrush . args)
  (apply isomorphism-compose (reverse args)))

