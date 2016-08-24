#lang racket/base

(require racket/contract
         lens/private/base/main
         lens/private/compound/main
         lens/private/util/rest-contract
         "join-list.rkt"
         "list-ref-take-drop.rkt")

(module+ test
  (require rackunit lens/private/test-util/test-lens))

(provide
 (contract-out
  [list-ref-nested-lens (rest-> exact-nonnegative-integer? lens?)]
  [list-refs-lens (rest-> exact-nonnegative-integer? (lens/c list? list?))]))


(define (list-ref-nested-lens . indices)
  (apply lens-thrush (map list-ref-lens indices)))

(module+ test
  (check-equal? (lens-transform/list '(a (b c) (d e f)) (list-ref-nested-lens 2 1) symbol->string)
                '(a (b c) (d "e" f))))


(define (list-refs-lens . indices)
  (apply lens-join/list (map list-ref-lens indices)))

(module+ test
  (define 1-5-6-lens (list-refs-lens 1 5 6))
  (check-lens-view 1-5-6-lens '(a b c d e f g)
                   '(b f g))
  (check-lens-set 1-5-6-lens '(a b c d e f g) '(1 2 3)
                  '(a 1 c d e 2 3)))
