#lang racket

(require fancy-app
         lens
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


(define (value-list->hash keys vs)
  (make-immutable-hash (map cons keys vs)))

(define (split-slice n vs)
  (define grouped
    (for/list ([group (in-slice n vs)])
      group))
  (define (get-ith i)
    (map (list-ref _ i) grouped))
  (build-list n get-ith))

(module+ test
  (check-equal? (split-slice 3 '(a 1 FOO b 2 BAR c 3 BAZ))
                '((a b c) (1 2 3) (FOO BAR BAZ))))


(define (compound-hash-lens . keys/lenses)
  (match-define (list keys lenses) (split-slice 2 keys/lenses))
  (define list-lens (apply compound-list-lens lenses))
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
