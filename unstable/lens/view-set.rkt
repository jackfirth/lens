#lang racket

(require lens
         fancy-app)

(module+ test
  (require rackunit))

(provide
 (contract-out
  [lens-set-all (->* (any/c any/c) #:rest (listof lens?) any/c)]))


(define (lens-set-all target new-view . lenses)
  (foldl (lens-set _ _ new-view) target lenses))

(module+ test
  (check-equal? (lens-set-all '(1 2 3 4 5) 'a first-lens third-lens fourth-lens)
                '(a 2 a a 5)))
