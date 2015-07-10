#lang racket

(require lens)

(module+ test
  (require rackunit))

(provide
 (contract-out
  [compound-list-lens (->* () #:rest (listof lens?) lens?)]))


(define (zip xs ys)
  (append-map list xs ys))

(define (compound-list-lens . lenses)
  (define (get target)
    (apply lens-view/list target lenses))
  (define (set target new-views)
    (apply lens-set/list target (zip lenses new-views)))
  (make-lens get set))


(module+ test
  (define first-third-fifth-lens
    (compound-list-lens first-lens
                        third-lens
                        fifth-lens))
  (check-equal? (lens-view first-third-fifth-lens '(a b c d e f))
                '(a c e))
  (check-equal? (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
                '(1 b 2 d 3 f)))
(define first-first-lens
  (compound-list-lens first-lens
                      first-lens))
