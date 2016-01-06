#lang sweet-exp racket/base

require racket/contract/base

provide
  contract-out
    reverse-lens (lens/c list? list?)
    last-lens (lens/c list? any/c)

require lens/private/base/main
        lens/private/list/main
        lens/private/compound/main
        lens/private/isomorphism/base

module+ test
  require rackunit fancy-app


(define reverse-lens
  (make-isomorphism-lens reverse reverse))

module+ test
  (check-equal? (lens-view reverse-lens '(1 2 3)) '(3 2 1))
  (check-equal? (lens-transform reverse-lens '(1 2 3) (cons 4 _)) '(1 2 3 4))


(define last-lens
  (lens-thrush reverse-lens first-lens))

module+ test
  (check-equal? (lens-view last-lens '(1 2 3)) 3)
  (check-equal? (lens-set last-lens '(1 2 3) 'a) '(1 2 a))
