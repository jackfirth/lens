#lang racket

(require lens
         unstable/sequence)

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


(define (compound-hash-lens . keys/lenses)
  (define grouped-keys/lenses
    (for/list ([key/lens (in-slice 2 keys/lenses)])
      key/lens))
  (define keys (map first grouped-keys/lenses))
  (define lenses (map second grouped-keys/lenses))
  (define list-lens (apply compound-list-lens lenses))
  (define (value-list->hash keys xs)
    (make-immutable-hash (map cons keys xs)))
  (define (get target)
    (value-list->hash keys (lens-view list-lens target)))
  (define (set target new-view-hash)
    (lens-set list-lens target (hash-values new-view-hash)))
  (make-lens get set))

(module+ test
  (define a-b-lens (compound-hash-lens 'a first-lens
                                       'b third-lens))
  (check-equal? (lens-view a-b-lens '(1 2 3))
                (hash 'a 1 'b 3))
  (check-equal? (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
                '(100 2 200)))
