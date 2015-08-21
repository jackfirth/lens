#lang racket/base

(require fancy-app
         lens
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit))

(provide struct-nested-lens)


(define-syntax struct-nested-lens
  (syntax-parser
    [(_ struct-id:id field-id:id)
     #'(struct-lens struct-id field-id)]
    [(_ struct-id:id [field-id:id field-struct-id:id] rest ...)
     #'(lens-thrush (struct-lens struct-id field-id)
                    (struct-nested-lens field-struct-id rest ...))]
    [(_ struct-id:id field-and-field-struct-id:id rest ...)
     #'(struct-nested-lens struct-id
                           [field-and-field-struct-id field-and-field-struct-id]
                           rest ...)]))

(module+ test
  (struct a (b b2) #:prefab)
  (struct b (b1 b2 b3) #:prefab)
  (define a-b-b1-lens (struct-nested-lens a b b1))
  (define a-b2-b3-lens (struct-nested-lens a [b2 b] b3))
  (check-equal? (lens-view a-b-b1-lens (a (b 1 2 3) 'foo)) 1)
  (check-equal? (lens-set a-b-b1-lens (a (b 1 2 3) 'foo) 10) (a (b 10 2 3) 'foo))
  (check-equal? (lens-view a-b2-b3-lens (a 'foo (b 1 2 3))) 3)
  (check-equal? (lens-set a-b2-b3-lens (a 'foo (b 1 2 3)) 10) (a 'foo (b 1 2 10))))
