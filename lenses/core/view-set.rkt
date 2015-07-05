#lang racket

(require unstable/sequence
         fancy-app
         "base.rkt")

(provide lens-view
         lens-set
         lens-view*
         lens-set*)


(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
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
