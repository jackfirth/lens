#lang racket/base

(provide set-filterer-lens)

(require lens/base/main
         racket/contract/base
         racket/list
         racket/set
         fancy-app
         )
(module+ test
  (require rackunit))

(define (set-filterer-lens pred)
  (make-lens
   (set-filter pred _)
   (λ (tgt nvw)
     (unless (andmap pred (set->list nvw))
       (error 'set-filterer-lens "expected (set/c ~a), given: ~v" (contract-name pred) nvw))
     (set-union (set-filter-not pred tgt) nvw))))

(define (set-filter pred set)
  (for/fold ([set set]) ([elem (in-set set)] #:unless (pred elem))
    (set-remove set elem)))

(define (set-filter-not pred set)
  (for/fold ([set set]) ([elem (in-set set)] #:when (pred elem))
    (set-remove set elem)))

(module+ test
  (check-equal? (lens-view (set-filterer-lens number?) '(1 a 2 b c 3 d e))
                '(1 2 3))
  (check-equal? (lens-set (set-filterer-lens number?) '(1 a 2 b c 3 d e) '(4 5 6 7))
                '(7 6 5 4 a b c d e))
  (check-equal? (lens-view (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e))
                (set 1 2 3))
  (check-equal? (lens-set (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e) (set 4 5 6 7))
                (set 4 5 6 7 'a 'b 'c 'd 'e))
  )
