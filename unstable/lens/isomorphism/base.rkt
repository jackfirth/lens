#lang sweet-exp racket/base

provide isomorphism-lens?
        isomorphism-lens-inverse
        rename-out [isomorphism-lens make-isomorphism-lens]
                   [isomorphism-lenses make-isomorphism-lenses]

require racket/match
        lens/base/gen-lens


(struct isomorphism-lens (f inv) #:transparent
  #:methods gen:lens
  [(define (lens-view lens tgt)
     ((isomorphism-lens-f lens) tgt))
   (define (lens-set lens tgt v)
     ((isomorphism-lens-inv lens) v))])

(define (isomorphism-lens-inverse lens)
  (match lens
    [(isomorphism-lens f inv)
     (isomorphism-lens inv f)]))

(define (isomorphism-lenses f inv)
  (values (isomorphism-lens f inv)
          (isomorphism-lens inv f)))
