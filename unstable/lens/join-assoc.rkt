#lang sweet-exp racket/base

provide lens-join/assoc

require lens/private/base/main
        lens/private/compound/join-list
        lens/private/list/assoc
        lens/private/util/alternating-list
        racket/match
        unstable/sequence
module+ test
  require rackunit lens/private/list/main

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
