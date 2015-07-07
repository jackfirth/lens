#lang racket/base

(provide list-ref-lens
         list-ref-nested-lens
         take-lens
         drop-lens
         first-lens
         second-lens
         third-lens
         fourth-lens
         fifth-lens
         sixth-lens
         seventh-lens
         eighth-lens
         ninth-lens
         tenth-lens)

(require racket/list
         fancy-app
         "../base/main.rkt"
         "car-cdr.rkt")

(module+ test
  (require rackunit))


(define (set-take n lst new-head)
  (append new-head (drop lst n)))

(module+ test
  (check-equal? (set-take 2 '(1 2 3 4 5) '(a b)) '(a b 3 4 5)))


(define (set-drop n lst new-tail)
  (append (take lst n) new-tail))

(module+ test
  (check-equal? (set-drop 2 '(1 2 3 4 5) '(a b c)) '(1 2 a b c)))


(define (take-lens n)
  (make-lens (take _ n) (set-take n _ _)))

(module+ test
  (define take2-lens (take-lens 2))
  (check-equal? (lens-view take2-lens '(1 2 3 4 5)) '(1 2))
  (check-equal? (lens-set take2-lens '(1 2 3 4 5) '(a b)) '(a b 3 4 5)))


(define (drop-lens n)
  (make-lens (drop _ n) (set-drop n _ _)))

(module+ test
  (define drop2-lens (drop-lens 2))
  (check-equal? (lens-view drop2-lens '(1 2 3 4 5)) '(3 4 5))
  (check-equal? (lens-set drop2-lens '(1 2 3 4 5) '(a b c)) '(1 2 a b c)))


(define (list-ref-lens i)
  (lens-compose car-lens (drop-lens i)))

(define (list-ref-nested-lens . args)
  (apply lens-thrush (map list-ref-lens args)))

(define first-lens (list-ref-lens 0))
(define second-lens (list-ref-lens 1))
(define third-lens (list-ref-lens 2))
(define fourth-lens (list-ref-lens 3))
(define fifth-lens (list-ref-lens 4))
(define sixth-lens (list-ref-lens 5))
(define seventh-lens (list-ref-lens 6))
(define eighth-lens (list-ref-lens 7))
(define ninth-lens (list-ref-lens 8))
(define tenth-lens (list-ref-lens 9))


(module+ test
  (check-eqv? (lens-view first-lens  '(1 2 3 4 5)) 1)
  (check-eqv? (lens-view second-lens '(1 2 3 4 5)) 2)
  (check-eqv? (lens-view third-lens  '(1 2 3 4 5)) 3)
  (check-eqv? (lens-view fourth-lens '(1 2 3 4 5)) 4)
  (check-eqv? (lens-view fifth-lens  '(1 2 3 4 5)) 5)
  (check-equal? (lens-set first-lens  '(1 2 3 4 5) 'a) '(a 2 3 4 5))
  (check-equal? (lens-set second-lens '(1 2 3 4 5) 'a) '(1 a 3 4 5))
  (check-equal? (lens-set third-lens  '(1 2 3 4 5) 'a) '(1 2 a 4 5))
  (check-equal? (lens-set fourth-lens '(1 2 3 4 5) 'a) '(1 2 3 a 5))
  (check-equal? (lens-set fifth-lens  '(1 2 3 4 5) 'a) '(1 2 3 4 a))
  (check-equal? (lens-transform* '(a (b c) (d e f)) (list-ref-nested-lens 2 1) symbol->string)
                '(a (b c) (d "e" f))))
