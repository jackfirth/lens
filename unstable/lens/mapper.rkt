#lang racket/base

(provide mapper-lens
         )

(require lens/base/main
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

(module+ test
  (check-equal? (lens-view (mapper-lens first-lens) '((a b) (c d) (e f)))
                '(a c e))
  (check-equal? (lens-set (mapper-lens first-lens) '((a b) (c d) (e f)) '(1 2 3))
                '((1 b) (2 d) (3 f)))
  (check-equal? (lens-transform (mapper-lens first-lens) '((a b) (c d) (e f)) (map symbol->string _))
                '(("a" b) ("c" d) ("e" f)))
  )
