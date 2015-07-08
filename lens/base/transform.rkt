#lang racket

(require unstable/sequence
         "base.rkt")

(module+ test
  (require rackunit
           fancy-app))

(provide lens-transform
         lens-transform*)


(define (lens-transform lens v f)
  (let-lens (view setter) lens v
    (setter (f view))))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-transform first-lens '(1 2 3) number->string)
                '("1" 2 3)))
  

(define (lens-transform* v . lenses/fs)
  (unless (even? (length lenses/fs))
    (error 'lens-transform*
           "expected an even number of association elements\n  association elements: ~v"
           lenses/fs))
  (for/fold ([v v]) ([lens/f (in-slice 2 lenses/fs)])
    (match-define (list lens f) lens/f)
    (lens-transform lens v f)))

(module+ test
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-transform* '(1 2 3)
                                 first-lens number->string
                                 second-lens (* 10 _))
                '("1" 20 3)))
