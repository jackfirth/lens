#lang racket

(require fancy-app
         "core/main.rkt")

(provide list-lens
         first-lens
         second-lens
         third-lens
         fourth-lens
         fifth-lens)

(module+ test
  (require rackunit))


(define (list-set-first lst v)
  (cons v (drop lst 1)))

(define (list-set-not-first i lst v)
  (append (take lst i)
          (list v)
          (drop lst (add1 i))))

(define (list-setter i)
  (if (zero? i)
      list-set-first
      (list-set-not-first i _ _)))

(define (list-getter i)
  (list-ref _ i))

(define (list-lens i)
  (make-lens (list-getter i)
             (list-setter i)))


(define first-lens (list-lens 0))
(define second-lens (list-lens 1))
(define third-lens (list-lens 2))
(define fourth-lens (list-lens 3))
(define fifth-lens (list-lens 4))

(module+ test
  (check-equal? (lens-view first-lens '(1 2 3 4 5)) 1)
  (check-equal? (lens-view second-lens '(1 2 3 4 5)) 2)
  (check-equal? (lens-view third-lens '(1 2 3 4 5)) 3)
  (check-equal? (lens-view fourth-lens '(1 2 3 4 5)) 4)
  (check-equal? (lens-view fifth-lens '(1 2 3 4 5)) 5)
  (check-equal? (lens-set first-lens '(1 2 3 4 5) 'a) '(a 2 3 4 5))
  (check-equal? (lens-set second-lens '(1 2 3 4 5) 'a) '(1 a 3 4 5))
  (check-equal? (lens-set third-lens '(1 2 3 4 5) 'a) '(1 2 a 4 5))
  (check-equal? (lens-set fourth-lens '(1 2 3 4 5) 'a) '(1 2 3 a 5))
  (check-equal? (lens-set fifth-lens '(1 2 3 4 5) 'a) '(1 2 3 4 a)))
