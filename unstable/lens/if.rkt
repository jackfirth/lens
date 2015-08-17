#lang racket/base

(provide lens-if)

(require lens/base/main)
(module+ test
  (require rackunit lens/list/main lens/vector/main))

(define (lens-if pred lens1 lens2)
  (make-lens
   (λ (tgt)
     (if (pred tgt)
         (lens-view lens1 tgt)
         (lens-view lens2 tgt)))
   (λ (tgt nvw)
     (if (pred tgt)
         (lens-set lens1 tgt nvw)
         (lens-set lens2 tgt nvw)))))

(module+ test
  (define if-lens (lens-if list? first-lens (vector-ref-lens 0)))
  (check-equal? (lens-view if-lens '(1 2 3)) 1)
  (check-equal? (lens-view if-lens '#(1 2 3)) 1)
  (check-equal? (lens-set if-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set if-lens '#(1 2 3) 'a) '#(a 2 3))
  )
