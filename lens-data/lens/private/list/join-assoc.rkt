#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    lens-join/assoc (->* [] #:rest (listof2 any/c lens?) (lens/c any/c (listof pair?)))

require lens/private/base/main
        lens/private/list/join-list
        lens/private/list/assoc
        lens/private/util/alternating-list
        lens/private/util/list-pair-contract
        racket/match
        unstable/sequence
module+ test
  require rackunit lens/private/list/list-ref-take-drop

(define (lens-join/assoc . ks/lenses)
  (define-values [keys lenses]
    (alternating-list->keys+values ks/lenses))
  (define key-lenses (map assoc-lens keys))
  (define list-lens (apply lens-join/list lenses))
  (make-lens
   (λ (tgt)
     (keys+values->assoc-list keys (lens-view list-lens tgt)))
   (λ (tgt nvw)
     (lens-set list-lens tgt (apply lens-view/list nvw key-lenses)))))

module+ test
  (define a-b-lens (lens-join/assoc 'a first-lens
                                    'b third-lens))
  (check-equal? (lens-view a-b-lens '(1 2 3))
                '((a . 1) (b . 3)))
  (check-equal? (lens-set a-b-lens '(1 2 3) '((a . 100) (b . 200)))
                '(100 2 200))
