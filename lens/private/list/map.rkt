#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [map-lens
           (-> lens? (lens/c list? list?))]
          [vector-map-lens
           (-> lens? (lens/c immutable-vector? immutable-vector?))]
          ))

(require lens/private/base/main
         lens/private/util/immutable
         racket/vector
         fancy-app
         )
(module+ test
  (require rackunit lens/private/list/main))

(define (map-lens lens)
  (make-lens
   (lens-view/map lens _)
   (lens-set/map lens _ _)))

(define (lens-view/map lens tgts)
  (map (lens-view lens _) tgts))

(define (lens-set/map lens tgts new-views)
  (map (lens-set lens _ _) tgts new-views))

(define (vector-map-lens lens)
  (make-lens
   (lens-view/vector-map lens _)
   (lens-set/vector-map lens _ _)))

(define (lens-view/vector-map lens tgt)
  (vector->immutable-vector (vector-map (lens-view lens _) tgt)))

(define (lens-set/vector-map lens tgt new-view)
  (vector->immutable-vector (vector-map (lens-set lens _ _) tgt new-view)))

(module+ test
  (check-equal? (lens-view (map-lens first-lens) '((a b) (c d) (e f)))
                '(a c e))
  (check-equal? (lens-set (map-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
                '((1 b) (2 d) (3 f)))
  (check-equal? (lens-transform (map-lens first-lens) '((a b) (c d) (e f)) (map symbol->string _))
                '(("a" b) ("c" d) ("e" f)))
  (check-equal? (lens-view (vector-map-lens first-lens) '#((a b) (c d) (e f)))
                '#(a c e))
  (check-equal? (lens-set (vector-map-lens first-lens) '#((a b) (c d) (e f)) '#(1 2 3))
                '#((1 b) (2 d) (3 f)))
  (check-equal? (lens-transform (vector-map-lens first-lens) '#((a b) (c d) (e f))
                                (immutable-vector-map symbol->string _))
                '#(("a" b) ("c" d) ("e" f)))
  )
