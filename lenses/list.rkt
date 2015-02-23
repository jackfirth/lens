#lang racket

(provide list-lens
         first-lens
         second-lens
         third-lens
         fourth-lens
         fifth-lens
         assoc-lens
         assv-lens
         assq-lens
         assf-lens)

(define ((list-lens i) lst)
  (define (list-set-first v)
    (cons v (drop lst 1)))
  (define (list-set-i v)
    (append (take lst i)
            (list v)
            (drop lst (add1 i))))
  (values (list-ref lst i)
          (if (zero? i)
              list-set-first
              list-set-i)))

(define first-lens (list-lens 0))
(define second-lens (list-lens 1))
(define third-lens (list-lens 2))
(define fourth-lens (list-lens 3))
(define fifth-lens (list-lens 4))

(define ((assoc-lens assoc-key #:is-equal? [equal? equal?]) assoc-list)
  (define assoc-pair (assoc assoc-key assoc-list equal?))
  (define (assoc-set new-assoc-pair)
    (if assoc-pair
        (assoc-swap assoc-list assoc-pair new-assoc-pair #:is-equal? equal?)
        (append assoc-list new-assoc-pair)))
  (values assoc-pair assoc-set))

(define (assoc-swap assoc-list old-assoc-pair new-assoc-pair #:is-equal? [equal? equal?])
  (define (swap-assoc-pair assoc-pair)
    (if (equal? assoc-pair old-assoc-pair)
        new-assoc-pair
        assoc-pair))
  (map swap-assoc-pair assoc-list))

(define (assv-lens assv-key)
  (assoc-lens assv-key #:is-equal? eqv?))

(define (assq-lens assq-key)
  (assoc-lens assq-key #:is-equal? eq?))

(define ((assf-lens predicate) assoc-list)
  (define assf-pair (assf predicate assoc-list))
  (define (assf-set new-assf-pair)
    (if assf-pair
        (assoc-swap assoc-list assf-pair new-assf-pair #:is-equal? eq?)
        (append assoc-list new-assf-pair)))
  (values assf-pair assf-set))