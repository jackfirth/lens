#lang sweet-exp racket/base

provide lazy-lens
        rec-lens

require fancy-app lens/private/base/main racket/promise

(define-syntax-rule (lazy-lens expr)
  (let ([p (delay expr)])
    (make-lens (lens-view (force p) _) (lens-set (force p) _ _))))

(define-syntax-rule (rec-lens name expr)
  (letrec ([name (lazy-lens expr)])
    name))
