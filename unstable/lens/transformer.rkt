#lang sweet-exp racket/base

provide transformer-lens

require fancy-app
        lens/private/base/main
        racket/match
        "isomorphism/base.rkt"
module+ test
  require lens/private/list/main
          rackunit
          "isomorphism/data.rkt"
          "mapper.rkt"

;; transformer-lens : (Lens (Outer Inner) Inner) (Lens A B) -> (Lens (Outer A) (Outer B))
(define (transformer-lens lens transformer-lens)
  (match transformer-lens
    [(make-isomorphism-lens transformer inverse)
     ;; transformer : A -> B
     ;; inverse     : B -> A
     (make-isomorphism-lens
      (lens-transform lens _ transformer) ; (Outer A) -> (Outer B)
      (lens-transform lens _ inverse))]   ; (Outer B) -> (Outer A)
    [transformer-lens
     ;; get : (Outer A) -> (Outer B)
     (define (get tgt)
       ;; transformer : A -> B
       (define (transformer a)
         (lens-view transformer-lens a))
       (lens-transform lens tgt transformer))
     ;; set : (Outer A) (Outer B) -> (Outer A)
     (define (set tgt nvw)
       ;; a : A
       (define a (lens-view lens tgt))
       ;; transformer : B -> A
       (define (transformer b)
         (lens-set transformer-lens a b))
       (lens-transform lens nvw transformer))
     (make-lens get set)]))

module+ test
  (define first-sym->str
    (transformer-lens first-lens symbol->string-lens))
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
  (define trans-second-first
    (transformer-lens second-lens first-lens))
  (check-equal? (lens-view trans-second-first '(1 (2 3) 4))
                '(1 2 4))
  (check-equal? (lens-set trans-second-first '(1 (2 3) 4) '(1 2 4))
                '(1 (2 3) 4))
  (check-equal? (lens-set trans-second-first '(1 (2 3) 4) '(1 b 4))
                '(1 (b 3) 4))
  (check-equal? (lens-set trans-second-first '(1 (2 3) 4) '(a b c))
                '(a (b 3) c))
  (check-equal? (lens-view trans-second-first
                           (lens-set trans-second-first '(1 (2 3) 4) '(a b c)))
                '(a b c))
  (define (rekey-alist-lens key->new-key-lens)
    (mapper-lens (transformer-lens car-lens key->new-key-lens)))
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

