#lang sweet-exp racket/base

module+ test
  require rackunit
          lens/private/base/main
          lens/private/compound/if
          lens/private/compound/lazy
          lens/private/isomorphism/data
          lens/private/list/map
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

