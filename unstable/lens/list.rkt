#lang racket

(require lens)

(module+ test
  (require rackunit))

(provide
  (contract-out
    [list-ref-nested-lens (->* () #:rest (listof exact-nonnegative-integer?) lens?)]))

(define (list-ref-nested-lens . args)
  (apply lens-thrush (map list-ref-lens args)))

(module+ test
  (check-equal? (lens-transform/list '(a (b c) (d e f)) (list-ref-nested-lens 2 1) symbol->string)
                '(a (b c) (d "e" f))))
