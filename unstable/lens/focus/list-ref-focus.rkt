#lang sweet-exp racket/base

;; This module provides a version of list-ref-lens based entirely off
;; of the focus-lens method, instead of the lens-view and lens-set
;; methods.

provide list-ref-lens

require fancy-app
        lens/private/base/gen-lens
        racket/match
        only-in srfi/1 append-reverse
        "make-lens-focus.rkt"
module+ test
  require lens/private/base/transform
          rackunit

(define (list-ref-lens i)
  (lens-thrush (drop-lens i) car-lens))

;; (Lens A B) (Lens B C) -> (Lens A C)
(define (lens-thrush lens1 lens2)
  (lens-lambda (tgt) ; [tgt : A]
    (let-lens [view1 ctxt1] lens1 tgt ; [view1 : B] [ctxt1 : (B -> A)]
      (let-lens [view2 ctxt2] lens2 view1 ; [view2 : C] [ctxt2 : (C -> B)]
        (define (ctxt c) ; [ctxt : (C -> A)]
          (ctxt1 (ctxt2 c)))
        (values view2 ctxt)))))

(define (drop-lens n)
  (lens-lambda (tgt)
    (define-values [rev-fst rst] (split-at-reverse tgt n))
    (values rst
            (append-reverse rev-fst _))))

(define car-lens
  (lens-match-lambda
    [(cons a b)
     (values a (cons _ b))]))

(define (split-at-reverse lst n)
  (let loop ([i n] [rev-fst '()] [rst lst])
    (cond [(eq? i 0)
           (values rev-fst rst)]
          [else
           (match rst
             [(cons fst rst)
              (loop (sub1 i) (cons fst rev-fst) rst)])])))

module+ test
  (let-values ([(fst rst) (split-at-reverse '(a b c d e) 2)])
    (check-equal? fst '(b a))
    (check-equal? rst '(c d e)))
  (check-equal? (lens-transform (list-ref-lens 2) '(1 2 3 4) (* 100 _))
                '(1 2 300 4))
