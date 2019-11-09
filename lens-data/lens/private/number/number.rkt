#lang sweet-exp racket/base

provide add-lens
        subtract-lens
        modulo-lens

require fancy-app
        lens/common
        lens/private/isomorphism/base
module+ test
  require rackunit

(define (add-lens x)
  (make-isomorphism-lens
   (+ _ x)
   (- _ x)))

(define (subtract-lens x)
  (make-isomorphism-lens
   (- _ x)
   (+ _ x)))

(define (modulo-lens n)
  ;; a = n*q + r
  (make-lens
   (modulo _ n)
   (λ (a r*)
     (define r (modulo a n))
     (define q (/ (- a r) n))
     (+ (* n q) r*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (lens-view (add-lens 5) 3) 8)
  (check-equal? (lens-set (add-lens 5) 3 8) 3)
  (check-equal? (lens-set (add-lens 5) 3 27) 22)

  (check-equal? (lens-view (subtract-lens 5) 3) -2)
  (check-equal? (lens-set (subtract-lens 5) 3 -2) 3)
  (check-equal? (lens-set (subtract-lens 5) 3 17) 22)

  (define mod2 (modulo-lens 2))
  (define mod10 (modulo-lens 10))

  (check-equal? (lens-view mod2 0) 0)
  (check-equal? (lens-view mod2 1) 1)
  (check-equal? (lens-view mod2 2) 0)
  (check-equal? (lens-view mod2 3) 1)
  (check-equal? (lens-view mod2 10) 0)
  (check-equal? (lens-view mod2 11) 1)
  (check-equal? (lens-set mod2 0 0) 0)
  (check-equal? (lens-set mod2 0 1) 1)
  (check-equal? (lens-set mod2 1 0) 0)
  (check-equal? (lens-set mod2 1 1) 1)
  (check-equal? (lens-set mod2 2 0) 2)
  (check-equal? (lens-set mod2 2 1) 3)
  (check-equal? (lens-set mod2 3 0) 2)
  (check-equal? (lens-set mod2 3 1) 3)
  (check-equal? (lens-set mod2 10 0) 10)
  (check-equal? (lens-set mod2 10 1) 11)
  (check-equal? (lens-set mod2 11 0) 10)
  (check-equal? (lens-set mod2 11 1) 11)

  (check-equal? (lens-view mod10 0) 0)
  (check-equal? (lens-view mod10 1) 1)
  (check-equal? (lens-view mod10 2) 2)
  (check-equal? (lens-view mod10 9) 9)
  (check-equal? (lens-view mod10 10) 0)
  (check-equal? (lens-view mod10 11) 1)
  (check-equal? (lens-view mod10 19) 9)
  (check-equal? (lens-view mod10 20) 0)
  (check-equal? (lens-set mod10 0 0) 0)
  (check-equal? (lens-set mod10 0 4) 4)
  (check-equal? (lens-set mod10 8 0) 0)
  (check-equal? (lens-set mod10 8 4) 4)
  (check-equal? (lens-set mod10 11 0) 10)
  (check-equal? (lens-set mod10 11 4) 14)
  (check-equal? (lens-set mod10 25 0) 20)
  (check-equal? (lens-set mod10 25 9) 29)
  )

