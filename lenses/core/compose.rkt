#lang racket

(require fancy-app
         "core.rkt"
         "identity.rkt"
         "sugar.rkt")

(provide lens-compose)

(module+ test
  (require rackunit))


(define (lens-compose2 sub-lens super-lens)
  (define (getter target)
    (lens-view sub-lens (lens-view super-lens target)))
  (define (setter target v)
    (lens-set super-lens target (lens-set sub-lens (lens-view super-lens target) v)))
  (make-lens getter setter))

(define lens-compose
  (compose (foldr lens-compose2 identity-lens _) list))

(module+ test
  (define (set-first lst v)
    (list* v (rest lst)))
  (define first-lens (make-lens first set-first))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (define first-of-second-lens (lens-compose first-lens second-lens))
  (define test-alist '((a 1) (b 2) (c 3)))
  (check-equal? (lens-view first-of-second-lens test-alist) 'b)
  (check-equal? (lens-set first-of-second-lens test-alist 'B)
                '((a 1) (B 2) (c 3))))
