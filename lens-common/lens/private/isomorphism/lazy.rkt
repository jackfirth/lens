#lang sweet-exp racket/base

provide lazy-isomorphism
        rec-isomorphism

require fancy-app
        lens/private/base/main
        racket/match
        racket/promise
        "base.rkt"
module+ test
  require rackunit
          racket/list
          lens/private/compound/identity
          lens/private/isomorphism/data
          "compound.rkt"

(define-syntax-rule (lazy-isomorphism expr)
  (let ([iso (delay expr)])
    (make-isomorphism-lens
     (λ (a) (match (force iso) [(make-isomorphism-lens f _) (f a)]))
     (λ (b) (match (force iso) [(make-isomorphism-lens _ inv) (inv b)])))))

(define-syntax-rule (rec-isomorphism name expr)
  (letrec ([name (lazy-isomorphism expr)])
    name))

module+ test
  (define (map-iso item-iso)
    (rec-isomorphism the-map-iso
      (isomorphism-cond
        [empty?  identity-lens  empty?]
        [cons?
         (isomorphism-join
           cons
           cons
           [first  item-iso     first]
           [rest   the-map-iso  rest])
         cons?])))
  ;
  (define (tree-map-iso a? item-iso b?)
    (rec-isomorphism the-tree-iso
      (isomorphism-cond
        [list? (map-iso the-tree-iso) list?]
        [a? item-iso b?])))
  ;
  (check-equal? (lens-view (tree-map-iso symbol? symbol->string-lens string?)
                           '(a (b (() c)) (d)))
                '("a" ("b" (() "c")) ("d")))
  (check-equal? (lens-set (tree-map-iso symbol? symbol->string-lens string?)
                          '(a (b (() c)) (d))
                          '("hay" ("bee" (() "sea")) ("deep")))
                '(hay (bee (() sea)) (deep)))

