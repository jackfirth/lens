#lang racket

(require fancy-app
         "base.rkt"
         "view-set.rkt"
         "identity.rkt")

(module+ test
  (require rackunit))

(provide lens-compose
         lens-thrush)


(define (lens-compose2 sub-lens super-lens)
  (define (get target)
    (lens-view sub-lens (lens-view super-lens target)))
  (define (set target new-view)
    (define sub-view (lens-view super-lens target))
    (define new-sub-view (lens-set sub-lens sub-view new-view))
    (lens-set super-lens target new-sub-view))
  (make-lens get set))


(define lens-compose
  (compose (foldr lens-compose2 identity-lens _) list))


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
