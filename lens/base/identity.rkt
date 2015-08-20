#lang racket/base

(require racket/function
         racket/contract/base
         "base.rkt"
         unstable/lens/isomorphism/base
         )

(module+ test
  (require rackunit
           "view-set.rkt"))

(provide
 (contract-out [identity-lens lens?]))

(define identity-lens
  (isomorphism-lens identity identity))


(module+ test
  (check-equal? (lens-view identity-lens 'foo) 'foo)
  (check-equal? (lens-set identity-lens 'foo 'bar) 'bar))
