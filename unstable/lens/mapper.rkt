#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [mapper-lens
           (-> lens? (lens/c list? list?))]
          [vector-mapper-lens
           (-> lens? (lens/c immutable-vector? immutable-vector?))]
          ))

(require lens/base/main
         lens/util/immutable
         racket/vector
         fancy-app
         )
(module+ test
  (require rackunit lens/list/main))

(define (mapper-lens lens)
  (make-lens
   (lens-view/map lens _)
   (lens-set/map lens _ _)))

(define (lens-view/map lens tgts)
  (map (lens-view lens _) tgts))

(define (lens-set/map lens tgts new-views)
  (map (lens-set lens _ _) tgts new-views))

(define (vector-mapper-lens lens)
  (make-lens
   (lens-view/vector-map lens _)
   (lens-set/vector-map lens _ _)))

(define (lens-view/vector-map lens tgt)
  (vector->immutable-vector (vector-map (lens-view lens _) tgt)))

(define (lens-set/vector-map lens tgt new-view)
  (vector->immutable-vector (vector-map (lens-set lens _ _) tgt new-view)))

(module+ test
  (check-equal? (lens-view (mapper-lens first-lens) '((a b) (c d) (e f)))
                '(a c e))
  (check-equal? (lens-set (mapper-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
                '((1 b) (2 d) (3 f)))
  (check-equal? (lens-transform (mapper-lens first-lens) '((a b) (c d) (e f)) (map symbol->string _))
                '(("a" b) ("c" d) ("e" f)))
  (check-equal? (lens-view (vector-mapper-lens first-lens) '#((a b) (c d) (e f)))
                '#(a c e))
  (check-equal? (lens-set (vector-mapper-lens first-lens) '#((a b) (c d) (e f)) '#(1 2 3))
                '#((1 b) (2 d) (3 f)))
  (check-equal? (lens-transform (vector-mapper-lens first-lens) '#((a b) (c d) (e f))
                                (immutable-vector-map symbol->string _))
                '#(("a" b) ("c" d) ("e" f)))
  )
