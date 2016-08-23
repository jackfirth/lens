#lang racket/base
(require reprovide/reprovide)
(reprovide (except-in "gen-lens.rkt" gen-lens/c) "make-lens.rkt" "contract.rkt")
(module+ test
  (require rackunit racket/list)
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (let-lens (view-first setter-first) first-lens '(1 2 3 4 5)
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))
