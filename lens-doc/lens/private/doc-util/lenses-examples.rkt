#lang sweet-exp racket

provide lens-examples
        lens-applicable-examples
        lens-unstable-examples
        persistent-lens-unstable-examples

require scribble-example


(define-examples-form lens-examples
  lens racket/list racket/vector racket/stream racket/set racket/contract)

(define-examples-form lens-applicable-examples
  lens/applicable racket/list racket/vector racket/stream racket/set racket/contract)

(define-examples-form lens-unstable-examples
  lens unstable/lens racket/list racket/vector racket/stream racket/set racket/contract)

(define-persistent-examples-form persistent-lens-unstable-examples
  lens unstable/lens racket/list racket/vector racket/stream racket/set racket/contract)
