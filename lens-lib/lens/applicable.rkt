#lang racket/base

(require reprovide/reprovide lens/private/common/applicable)
(reprovide lens)

(module+ test
  (require rackunit)
  (check-equal? (first-lens '(a b c)) 'a)
  (check-equal? (lens-view first-lens '(a b c)) 'a)
  (check-equal? (lens-set first-lens '(a b c) 97) '(97 b c)))
