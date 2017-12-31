#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    lens-zoom (-> lens? lens? lens?)
    lens-zoom* (->* [] #:rest (listof2 lens? lens?) lens?)

require fancy-app
        lens/private/base/main
        lens/private/compound/thrush
        lens/private/util/list-pair-contract
        racket/match
        racket/sequence
        lens/private/isomorphism/base
module+ test
  require lens/private/list/main
          rackunit
          lens/private/isomorphism/data
          lens/private/list/map

;; lens-zoom : (Lens (Outer Inner) Inner) (Lens A B) -> (Lens (Outer A) (Outer B))
(define (lens-zoom zoom-lens trans-lens)
  (match trans-lens
    [(make-isomorphism-lens transformer inverse)
     ;; transformer : A -> B
     ;; inverse     : B -> A
     (make-isomorphism-lens
      (lens-transform zoom-lens _ transformer) ; (Outer A) -> (Outer B)
      (lens-transform zoom-lens _ inverse))]   ; (Outer B) -> (Outer A)
    [_
     ;; get : (Outer A) -> (Outer B)
     (define (get x)
       ;; a : A
       (define a (lens-view zoom-lens x))
       ;; inner : B
       (define inner (lens-view trans-lens a))
       (lens-set zoom-lens x inner))
     ;; set : (Outer A) (Outer B) -> (Outer A)
     (define (set x y)
       ;; a : A, b : B
       (define a (lens-view zoom-lens x))
       (define b (lens-view zoom-lens y))
       ;; inner : A
       (define inner (lens-set trans-lens a b))
       (lens-set zoom-lens y inner))
     (make-lens get set)]))


;; For the zoomed lens to follow the lens laws, these properties need to hold
;; for all X : (Outer A) and Y, Y1, Y2 : (Outer B).
;; (get (set X Y)) = Y
;; (set X (get X)) = X
;; (set (set X Y1) Y2) = (set X Y2)

;; Context:
;; C1.   {lens laws for trans-lens}
;;     (lens-view trans-lens (lens-set trans-lens A B)) = B
;; C2.   {lens laws for trans-lens}
;;     (lens-set trans-lens A (lens-view trans-lens A)) = A
;; C3.   {lens laws for zoom-lens}
;;     for all X : (Outer Inner) and A : Inner,
;;     (lens-view zoom-lens (lens-set zoom-lens X A)) = A
;; C4.   {lens laws for zoom-lens}
;;     for all X : (Outer Inner),
;;     (lens-set zoom-lens X (lens-view zoom-lens X)) = X
;; C5.   {lens laws for trans-lens}
;;     (lens-set trans-lens (lens-set trans-lens A B1) B2)
;;     =
;;     (lens-set trans-lens A B2)
;; C6.   {lens laws for zoom-lens}
;;     for all X : (Outer Inner), A1 : Inner, and A2 : Inner,
;;     (lens-set zoom-lens (lens-set zoom-lens X A1) A2)
;;     =
;;     (lens-set zoom-lens X A2)


;; Proof for (get (set X Y)) = Y:
;;   (get (set X Y))
;; =   {Def. set}
;;   let a = (lens-view zoom-lens X)
;;   let b = (lens-view zoom-lens Y)
;;   let inner = (lens-set trans-lens a b)
;;   (get (lens-set zoom-lens Y inner))
;; =   {Def. get}
;;   let a = (lens-view zoom-lens X)
;;   let b = (lens-view zoom-lens Y)
;;   let inner = (lens-set trans-lens a b)
;;   let outer = (lens-set zoom-lens Y inner)
;;   let a* = (lens-view zoom-lens outer)
;;   let inner* = (lens-view trans-lens a*)
;;   (lens-set zoom-lens outer inner*)
;; =   {C3}
;;   let a = (lens-view zoom-lens X)
;;   let b = (lens-view zoom-lens Y)
;;   let inner = (lens-set trans-lens a b)
;;   let outer = (lens-set zoom-lens Y inner)
;;   let inner* = (lens-view trans-lens inner)
;;   (lens-set zoom-lens outer inner*)
;; =   {C1}
;;   let a = (lens-view zoom-lens X)
;;   let b = (lens-view zoom-lens Y)
;;   let inner = (lens-set trans-lens a b)
;;   let outer = (lens-set zoom-lens Y inner)
;;   (lens-set zoom-lens outer b)
;; =   {C6}
;;   let b = (lens-view zoom-lens Y)
;;   (lens-set zoom-lens Y b)
;; =   {C4}
;;   Y


;; Proof for (set X (get X)) = X:
;;   (set X (get X))
;; =   {Def. get}
;;   let a = (lens-view zoom-lens X)
;;   let inner = (lens-view trans-lens a)
;;   (set X (lens-set zoom-lens X inner))
;; =   {Def. set}
;;   let a = (lens-view zoom-lens X)
;;   let inner = (lens-view trans-lens a)
;;   let y = (lens-set zoom-lens X inner)
;;   let b = (lens-view zoom-lens y)
;;   let inner* = (lens-set trans-lens a b)
;;   (lens-set zoom-lens y inner*)
;; =   {C3}
;;   let a = (lens-view zoom-lens X)
;;   let inner = (lens-view trans-lens a)
;;   let y = (lens-set zoom-lens X inner)
;;   let inner* = (lens-set trans-lens a inner)
;;   (lens-set zoom-lens y inner*)
;; =   {C2}
;;   let a = (lens-view zoom-len X)
;;   let inner = (lens-view trans-lens a)
;;   let y = (lens-set zoom-lens X inner)
;;   (lens-set zoom-lens y a)
;; =   {C6}
;;   let a = (lens-view zoom-lens X)
;;   (lens-set zoom-lens X a)
;; =   {C4}
;;   X


;; Proof for (set (set X Y1) Y2) = (set X Y2):
;;   (set (set X Y1) Y2)
;; =   {Def. set}
;;   let a1 = (lens-view zoom-lens X)
;;   let b1 = (lens-view zoom-lens Y1)
;;   let inner1 = (lens-set trans-lens a1 b1)
;;   (set (lens-set zoom-lens Y1 inner1) Y2)
;; =   {Def. set}
;;   let a1 = (lens-view zoom-lens X)
;;   let b1 = (lens-view zoom-lens Y1)
;;   let inner1 = (lens-set trans-lens a1 b1)
;;   let outer1 = (lens-set zoom-lens Y1 inner1)
;;   let a2 = (lens-view zoom-lens outer1)
;;   let b2 = (lens-view zoom-lens Y2)
;;   let inner2 = (lens-set trans-lens a2 b2)
;;   (lens-set zoom-lens Y2 inner2)
;; =   {C3}
;;   let a1 = (lens-view zoom-lens X)
;;   let b1 = (lens-view zoom-lens Y1)
;;   let inner1 = (lens-set trans-lens a1 b1)
;;   let b2 = (lens-view zoom-lens Y2)
;;   let inner2 = (lens-set trans-lens inner1 b2)
;;   (lens-set zoom-lens Y2 inner2)
;; =   {C5}
;;   let a1 = (lens-view zoom-lens X)
;;   let b2 = (lens-view zoom-lens Y2)
;;   let inner2 = (lens-set trans-lens a1 b2)
;;   (lens-set zoom-lens Y2 inner2)
;; =   {Def. set}
;;   (set X Y2)


(define (lens-zoom* . lenses/transformers)
  (apply lens-thrush
         (for/list ([args (in-slice 2 lenses/transformers)])
           (apply lens-zoom args))))

module+ test
  (define first-sym->str
    (lens-zoom first-lens symbol->string-lens))
  (check-equal? (lens-view first-sym->str '(a b c))
                '("a" b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("a" b c))
                '(a b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("z" b c))
                '(z b c))
  (check-equal? (lens-set first-sym->str '(a b c) '("z" bee sea))
                '(z bee sea))
  (check-equal? (lens-view first-sym->str (lens-set first-sym->str '(a b c) '("z" bee sea)))
                '("z" bee sea))
  (define trans-second-first/third-second
    (lens-zoom* second-lens first-lens third-lens second-lens))
  (check-equal? (lens-view trans-second-first/third-second '(1 (2 3) (4 5)))
                '(1 2 5))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(1 2 5))
                '(1 (2 3) (4 5)))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(1 b 5))
                '(1 (b 3) (4 5)))
  (check-equal? (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(a b c))
                '(a (b 3) (4 c)))
  (check-equal? (lens-view trans-second-first/third-second
                           (lens-set trans-second-first/third-second '(1 (2 3) (4 5)) '(a b c)))
                '(a b c))
  (define (rekey-alist-lens key->new-key-lens)
    (map-lens (lens-zoom car-lens key->new-key-lens)))
  (check-equal? (lens-view (rekey-alist-lens symbol->string-lens) '((a . 1) (b . 2) (c . 3)))
                '(("a" . 1) ("b" . 2) ("c" . 3)))
  (check-equal? (lens-set (rekey-alist-lens symbol->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("a" . 10) ("b" . 200) ("c" . 3000)))
                '((a . 10) (b . 200) (c . 3000)))
  (check-equal? (lens-set (rekey-alist-lens symbol->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("one" . 10) ("two" . 200) ("three" . 3000)))
                '((one . 10) (two . 200) (three . 3000)))
  (define (rek+v-alist-lens key->new-key-lens value->new-value-lens)
    (map-lens (lens-zoom* car-lens key->new-key-lens cdr-lens value->new-value-lens)))
  (check-equal? (lens-view (rek+v-alist-lens symbol->string-lens number->string-lens)
                           '((a . 1) (b . 2) (c . 3)))
                '(("a" . "1") ("b" . "2") ("c" . "3")))
  (check-equal? (lens-set (rek+v-alist-lens symbol->string-lens number->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("a" . "10") ("b" . "200") ("c" . "3000")))
                '((a . 10) (b . 200) (c . 3000)))
  (check-equal? (lens-set (rek+v-alist-lens symbol->string-lens number->string-lens)
                          '((a . 1) (b . 2) (c . 3))
                          '(("one" . "10") ("two" . "200") ("three" . "3000")))
                '((one . 10) (two . 200) (three . 3000)))

