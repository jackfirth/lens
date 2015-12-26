#lang racket/base

(require racket/contract/base)
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
         "../base/rename.rkt"
         "../compound/compose.rkt"
         "car-cdr.rkt")

(module+ test
  (require rackunit "../test-util/test-lens.rkt"))


(define (set-take n lst new-head)
  (append new-head (drop lst n)))

(module+ test
  (check-equal? (set-take 2 '(1 2 3 4 5) '(a b)) '(a b 3 4 5)))


(define (set-drop n lst new-tail)
  (append (take lst n) new-tail))

(module+ test
  (check-equal? (set-drop 2 '(1 2 3 4 5) '(a b c)) '(1 2 a b c)))


(define (take-lens n)
  (lens-rename
   (make-lens (take _ n) (set-take n _ _))
   `(take-lens ,n)))

(module+ test
  (define take2-lens (take-lens 2))
  (check-lens-view take2-lens '(1 2 3 4 5) '(1 2))
  (check-lens-set take2-lens '(1 2 3 4 5) '(a b) '(a b 3 4 5)))


(define (drop-lens n)
  (lens-rename
   (make-lens (drop _ n) (set-drop n _ _))
   `(drop-lens ,n)))

(module+ test
  (define drop2-lens (drop-lens 2))
  (check-lens-view drop2-lens '(1 2 3 4 5) '(3 4 5))
  (check-lens-set drop2-lens '(1 2 3 4 5) '(a b c) '(1 2 a b c)))


(define (list-ref-lens i)
  (lens-rename
   (lens-compose car-lens (drop-lens i))
   `(list-ref-lens ,i)))

(define first-lens   (lens-rename (list-ref-lens 0) 'first-lens))
(define second-lens  (lens-rename (list-ref-lens 1) 'second-lens))
(define third-lens   (lens-rename (list-ref-lens 2) 'third-lens))
(define fourth-lens  (lens-rename (list-ref-lens 3) 'fourth-lens))
(define fifth-lens   (lens-rename (list-ref-lens 4) 'fifth-lens))
(define sixth-lens   (lens-rename (list-ref-lens 5) 'sixth-lens))
(define seventh-lens (lens-rename (list-ref-lens 6) 'seventh-lens))
(define eighth-lens  (lens-rename (list-ref-lens 7) 'eigth-lens))
(define ninth-lens   (lens-rename (list-ref-lens 8) 'ninth-lens))
(define tenth-lens   (lens-rename (list-ref-lens 9) 'tenth-lens))


(module+ test
  (check-lens-view first-lens  '(1 2 3 4 5) 1)
  (check-lens-view second-lens '(1 2 3 4 5) 2)
  (check-lens-view third-lens  '(1 2 3 4 5) 3)
  (check-lens-view fourth-lens '(1 2 3 4 5) 4)
  (check-lens-view fifth-lens  '(1 2 3 4 5) 5)
  (check-lens-set first-lens  '(1 2 3 4 5) 'a '(a 2 3 4 5))
  (check-lens-set second-lens '(1 2 3 4 5) 'a '(1 a 3 4 5))
  (check-lens-set third-lens  '(1 2 3 4 5) 'a '(1 2 a 4 5))
  (check-lens-set fourth-lens '(1 2 3 4 5) 'a '(1 2 3 a 5))
  (check-lens-set fifth-lens  '(1 2 3 4 5) 'a '(1 2 3 4 a)))
