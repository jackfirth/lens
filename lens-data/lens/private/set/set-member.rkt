#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    set-member-lens (-> any/c (lens/c functional-set? boolean?))

require fancy-app
        lens/private/base/main
        lens/private/util/functional-set
        racket/set
module+ test
  require rackunit

(define (set-member-lens v)
  (make-lens
   (set-member? _ v)
   (λ (tgt nvw)
     (if nvw
         (set-add tgt v)
         (set-remove tgt v)))))

module+ test
  (define 2-lens (set-member-lens 2))
  (check-equal? (lens-view 2-lens (set 1 2 3)) #t)
  (check-equal? (lens-view 2-lens (set 1 3)) #f)
  (check-equal? (lens-set 2-lens (set 1 2 3) #t) (set 1 2 3))
  (check-equal? (lens-set 2-lens (set 1 2 3) #f) (set 1 3))
  (check-equal? (lens-set 2-lens (set 1 3) #t) (set 1 2 3))
  (check-equal? (lens-set 2-lens (set 1 3) #f) (set 1 3))
