#lang racket

(require unstable/sequence
         "base.rkt"
         "../util/list-pair-contract.rkt")

(module+ test
  (require rackunit
           fancy-app))

(provide
 (contract-out
  [lens-transform (-> lens? any/c (-> any/c any/c) any/c)]
  [lens-transform/list (->* (any/c) #:rest (listof2 lens? (-> any/c any/c)) any/c)]))



(define (lens-transform lens v f)
  (let-lens (view setter) lens v
    (setter (f view))))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-transform first-lens '(1 2 3) number->string)
                '("1" 2 3)))
  

(define (lens-transform/list v . lenses/fs)
  (for/fold ([v v]) ([lens/f (in-slice 2 lenses/fs)])
    (match-define (list lens f) lens/f)
    (lens-transform lens v f)))

(module+ test
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-transform/list '(1 2 3)
                                     first-lens number->string
                                     second-lens (* 10 _))
                '("1" 20 3)))
