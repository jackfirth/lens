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
        unstable/sequence
        "isomorphism/base.rkt"
module+ test
  require lens/private/list/main
          rackunit
          "isomorphism/data.rkt"
          "mapper.rkt"

;; lens-zoom : (Lens (Outer Inner) Inner) (Lens A B) -> (Lens (Outer A) (Outer B))
(define (lens-zoom zoom-lens transformer-lens)
  (match transformer-lens
    [(make-isomorphism-lens transformer inverse)
     ;; transformer : A -> B
     ;; inverse     : B -> A
     (make-isomorphism-lens
      (lens-transform zoom-lens _ transformer) ; (Outer A) -> (Outer B)
      (lens-transform zoom-lens _ inverse))]   ; (Outer B) -> (Outer A)
    [transformer-lens
     ;; get : (Outer A) -> (Outer B)
     (define (get tgt)
       ;; transformer : A -> B
       (define (transformer a)
         (lens-view transformer-lens a))
       (lens-transform zoom-lens tgt transformer))
     ;; set : (Outer A) (Outer B) -> (Outer A)
     (define (set tgt nvw)
       ;; a : A
       (define a (lens-view zoom-lens tgt))
       ;; transformer : B -> A
       (define (transformer b)
         (lens-set transformer-lens a b))
       (lens-transform zoom-lens nvw transformer))
     (make-lens get set)]))

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
    (mapper-lens (lens-zoom car-lens key->new-key-lens)))
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
    (mapper-lens (lens-zoom* car-lens key->new-key-lens cdr-lens value->new-value-lens)))
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

