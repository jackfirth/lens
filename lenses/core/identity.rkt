#lang racket

(require fancy-app)

(module+ test
  (require rackunit
           "view-set.rkt"))

(provide identity-lens)


(define identity-lens
  (values _ identity))


(module+ test
  (check-equal? (lens-view identity-lens 'foo) 'foo)
  (check-equal? (lens-set identity-lens 'foo 'bar) 'bar))
