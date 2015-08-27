#lang sweet-exp racket/base

require racket/contract/base

provide
  contract-out
    set-filterer-lens (-> predicate/c (lens/c functional-set? functional-set?))

require lens/private/base/main
        lens/private/util/functional-set
        racket/set
        racket/function
        fancy-app

module+ test
  require rackunit


(define (set-filter pred set)
  (for/fold ([set set]) ([elem (in-set set)] #:unless (pred elem))
    (set-remove set elem)))

(define (set-filter-not pred set)
  (for/fold ([set set]) ([elem (in-set set)] #:when (pred elem))
    (set-remove set elem)))

(define (andmap-set pred set)
  (andmap pred (set->list set)))


(define (check-set-filterer-lens-view pred new-view-to-check)
  (unless (andmap-set pred new-view-to-check)
    (raise-argument-error 'set-filterer-lens
                          (format "(set/c ~a)" (contract-name pred))
                          new-view-to-check)))

(define (set-filterer-lens pred)
  (define (insert-filtered-items target new-view)
    (check-set-filterer-lens-view pred new-view)
    (set-union (set-filter-not pred target) new-view))
  (make-lens (set-filter pred _)
             insert-filtered-items))

module+ test
  (check-equal? (lens-view (set-filterer-lens number?) '(1 a 2 b c 3 d e))
                '(1 2 3))
  (check-equal? (lens-set (set-filterer-lens number?) '(1 a 2 b c 3 d e) '(4 5 6 7))
                '(7 6 5 4 a b c d e))
  (check-equal? (lens-view (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e))
                (set 1 2 3))
  (check-equal? (lens-set (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e) (set 4 5 6 7))
                (set 4 5 6 7 'a 'b 'c 'd 'e))
  (check-exn exn:fail:contract?
             (thunk (lens-set (set-filterer-lens number?) (set 1) (set 'a))))
