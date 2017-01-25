#lang racket/base

(require racket/sequence
         racket/match
         racket/contract/base
         fancy-app
         lens/private/base/base
         "../util/list-pair-contract.rkt")

(module+ test
  (require rackunit racket/list))

(provide
 lens-view
 lens-set
 (contract-out [lens-view/list (->* (any/c) #:rest (listof lens?) list?)]
               [lens-set/list (->* (any/c) #:rest (listof2 lens? any/c) any/c)]))


(define (lens-view/list target . lenses)
  (map (lens-view _ target) lenses))

(define (lens-set/list target . lenses/xs)
  (for/fold ([target target]) ([lens/x (in-slice 2 lenses/xs)])
    (match-define (list lens x) lens/x)
    (lens-set lens target x)))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define first-lens (make-lens first set-first))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-view/list '(1 2 3) first-lens second-lens)
                '(1 2))
  (check-equal? (lens-set/list '(1 2 3)
                               first-lens 10
                               second-lens 20)
                '(10 20 3))
  (check-equal? (lens-set/list '(1 2 3)
                               first-lens 'a
                               first-lens 'b)
                '(b 2 3)))
