#lang racket

(require unstable/sequence
         fancy-app
         "base.rkt")
(module+ test
  (require rackunit))

(provide
 lens-view*
 lens-set*
 (contract-out [lens-view (-> lens? any/c any/c)]
               [lens-set (-> lens? any/c any/c any/c)]))


(define (lens-view lens v)
  (let-lens (view _) lens v
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) lens v
    (setter x)))

(define (lens-view* v . lenses)
  (for/fold ([v v]) ([lens (in-list lenses)])
    (lens-view lens v)))

(define (lens-set* v . lenses/xs)
  (unless (even? (length lenses/xs))
    (error 'lens-set*
           "expected an even number of association elements\n  association elements: ~v"
           lenses/xs))
  (for/fold ([v v]) ([lens/x (in-slice 2 lenses/xs)])
    (match-define (list lens x) lens/x)
    (lens-set lens v x)))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define first-lens (make-lens first set-first))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-view* '((1 2) 3) first-lens second-lens)
                2)
  (check-equal? (lens-set* '(1 2 3)
                           first-lens 10
                           second-lens 20)
                '(10 20 3))
  )
