#lang racket

(provide
 (contract-out
  [list-ref-lens
   (->i ([i exact-nonnegative-integer?])
        [lens (i) (lens/c (list*-length-at-least/c (add1 i)) any/c)])]
  [take-lens
   (->i ([i exact-nonnegative-integer?])
        [lens (i) (lens/c (list*-length-at-least/c (add1 i)) any/c)])]
  [drop-lens
   (->i ([i exact-nonnegative-integer?])
        [lens (i) (lens/c (list*-length-at-least/c (add1 i)) any/c)])]
  [first-lens   (lens/c (list*-length-at-least/c 1) any/c)]
  [second-lens  (lens/c (list*-length-at-least/c 2) any/c)]
  [third-lens   (lens/c (list*-length-at-least/c 3) any/c)]
  [fourth-lens  (lens/c (list*-length-at-least/c 4) any/c)]
  [fifth-lens   (lens/c (list*-length-at-least/c 5) any/c)]
  [sixth-lens   (lens/c (list*-length-at-least/c 6) any/c)]
  [seventh-lens (lens/c (list*-length-at-least/c 7) any/c)]
  [eighth-lens  (lens/c (list*-length-at-least/c 8) any/c)]
  [ninth-lens   (lens/c (list*-length-at-least/c 9) any/c)]
  [tenth-lens   (lens/c (list*-length-at-least/c 10) any/c)]))

(require racket/list
         fancy-app
         "../util/improper-list-length.rkt"
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
  (check-equal? (lens-set fifth-lens  '(1 2 3 4 5) 'a) '(1 2 3 4 a)))
