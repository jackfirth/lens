#lang sweet-exp racket/base

provide lazy-lens
        rec-lens

require fancy-app lens/private/base/main racket/promise
module+ test
  require rackunit "if.rkt" "isomorphism/data.rkt" "map.rkt"

(define-syntax-rule (lazy-lens expr)
  (let ([p (delay expr)])
    (make-lens (lens-view (force p) _) (lens-set (force p) _ _))))

(define-syntax-rule (rec-lens name expr)
  (letrec ([name (lazy-lens expr)])
    name))

module+ test
  (define (tree-map-lens item-lens)
    (rec-lens the-tree-lens
      (lens-cond [list? (map-lens the-tree-lens)]
                 [else item-lens])))
  (check-equal? (lens-view (tree-map-lens symbol->string-lens) '(a (b (() c)) (d)))
                '("a" ("b" (() "c")) ("d")))
  (check-equal? (lens-set (tree-map-lens symbol->string-lens)
                          '(a (b (() c)) (d))
                          '("hay" ("bee" (() "sea")) ("deep")))
                '(hay (bee (() sea)) (deep)))

