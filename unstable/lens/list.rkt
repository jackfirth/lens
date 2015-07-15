#lang racket

(require lens
         "join.rkt")

(module+ test
  (require rackunit))

(provide
  (contract-out
    [list-ref-nested-lens (->* () #:rest (listof exact-nonnegative-integer?) lens?)]
    [list-refs-lens (->* () #:rest (listof exact-nonnegative-integer?) lens?)]))


(define (list-ref-nested-lens . indices)
  (apply lens-thrush (map list-ref-lens indices)))

(module+ test
  (check-equal? (lens-transform/list '(a (b c) (d e f)) (list-ref-nested-lens 2 1) symbol->string)
                '(a (b c) (d "e" f))))


(define (list-refs-lens . indices)
  (apply lens-join/list (map list-ref-lens indices)))

(module+ test
  (define 1-5-6-lens (list-refs-lens 1 5 6))
  (check-equal? (lens-view 1-5-6-lens '(a b c d e f g))
                '(b f g))
  (check-equal? (lens-set 1-5-6-lens '(a b c d e f g) '(1 2 3))
                '(a 1 c d e 2 3)))
