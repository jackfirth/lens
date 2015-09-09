#lang sweet-exp racket/base

require racket/contract
        racket/list
        racket/match
        fancy-app
        "../base/main.rkt"
        "../base/rename.rkt"
        "../util/rest-contract.rkt"
        "identity.rkt"

module+ test
  require rackunit
          racket/set
          "../test-util/test-lens.rkt"

provide
  contract-out
    lens-compose (rest-> lens? lens?)


(define (lens-compose2 sub-lens super-lens)
  (define (get target)
    (lens-view sub-lens (lens-view super-lens target)))
  (define (set target new-view)
    (define sub-view (lens-view super-lens target))
    (define new-sub-view (lens-set sub-lens sub-view new-view))
    (lens-set super-lens target new-sub-view))
  (lens-rename (make-lens get set) 'composed))


(define (lens-compose . args)
  (foldr lens-compose2 identity-lens args))


module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (define test-alist '((a 1) (b 2) (c 3)))
  (define first-of-second-lens (lens-compose first-lens second-lens))
  (check-lens-view first-of-second-lens test-alist 'b)
  (check-lens-set first-of-second-lens test-alist 'B '((a 1) (B 2) (c 3)))
  (check-eq? (lens-compose) identity-lens)
