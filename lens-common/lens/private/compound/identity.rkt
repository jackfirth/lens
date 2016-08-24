#lang sweet-exp racket/base

require racket/function
        racket/contract/base
        "../base/main.rkt"
        lens/private/isomorphism/base

module+ test
  require rackunit
          "../base/main.rkt"
          "../test-util/test-lens.rkt"

provide
  contract-out
    identity-lens lens?


(define identity-lens
  (make-isomorphism-lens identity identity))

(module+ test
  (check-lens-view identity-lens 'foo 'foo)
  (check-lens-set identity-lens 'foo 'bar 'bar))
