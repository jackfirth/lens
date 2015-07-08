#lang racket/base

(provide dict-ref-lens)

(require racket/dict fancy-app "base/main.rkt")
(module+ test
  (require rackunit))

(define (dict-ref-lens key)
  (make-lens (dict-ref _ key)
             (dict-set _ key _)))

(module+ test
  (check-equal? (lens-transform* '((a . 1) (b . 2) (c . 3)) (dict-ref-lens 'a) (* 100 _))
                '((a . 100) (b . 2) (c . 3)))
  )
