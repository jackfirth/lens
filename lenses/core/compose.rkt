#lang racket

(require fancy-app
         "base.rkt"
         "lens-lambda.rkt"
         "identity.rkt")

(module+ test
  (require rackunit
           "view-set.rkt"))

(provide lens-compose
         lens-thrush)


(define (lens-compose2 sub-lens super-lens)
  (lens-lambda (v)
    (let-lens (super-view super-setter) super-lens v
      (let-lens (sub-view sub-setter) sub-lens super-view
        (values sub-view
                (compose super-setter sub-setter))))))


(define (lens-compose . args)
  (foldr lens-compose2 identity-lens args))


(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second set-second))
  (define first-of-second-lens (lens-compose first-lens second-lens))
  (define test-alist '((a 1) (b 2) (c 3)))
  (check-eq? (lens-view first-of-second-lens test-alist) 'b)
  (check-equal? (lens-set first-of-second-lens test-alist 'B)
                '((a 1) (B 2) (c 3))))


(define (lens-thrush . args)
  (apply lens-compose (reverse args)))

(module+ test
  (define first-of-second-lens* (lens-thrush second-lens first-lens))
  (let-lens [val ctxt] first-of-second-lens* test-alist
    (check-equal? val 'b)
    (check-equal? (ctxt 'B) '((a 1) (B 2) (c 3)))))
