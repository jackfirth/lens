#lang racket/base

(require lens/base/main
         racket/contract/base
         racket/list
         fancy-app
         )
(module+ test
  (require rackunit))

(define (filterer-lens pred)
  (make-lens
   (filter pred _)
   (λ (tgt nvw)
     (unless (andmap pred nvw)
       (error 'filterer-lens "expected (listof ~a), given: ~v" (contract-name pred) nvw))
     (append nvw (filter-not pred tgt)))))

(module+ test
  (check-equal? (lens-view (filterer-lens number?) '(1 a 2 b c 3 d e))
                '(1 2 3))
  (check-equal? (lens-set (filterer-lens number?) '(1 a 2 b c 3 d e) '(4 5 6 7))
                '(4 5 6 7 a b c d e))
  )
