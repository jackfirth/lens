#lang racket

(require rackunit
         "core.rkt")

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

(module+ test
  (check-eqv? (lens-view first-lens '(1 2 3 4 5)) 1)
  (check-eqv? (lens-view second-lens '(1 2 3 4 5)) 2)
  (check-eqv? (lens-view third-lens '(1 2 3 4 5)) 3)
  (check-eqv? (lens-view fourth-lens '(1 2 3 4 5)) 4)
  (check-eqv? (lens-view fifth-lens '(1 2 3 4 5)) 5)
  (check-equal? (lens-set first-lens '(1 2 3 4 5) 'a) '(a 2 3 4 5))
  (check-equal? (lens-set second-lens '(1 2 3 4 5) 'a) '(1 a 3 4 5))
  (check-equal? (lens-set third-lens '(1 2 3 4 5) 'a) '(1 2 a 4 5))
  (check-equal? (lens-set fourth-lens '(1 2 3 4 5) 'a) '(1 2 3 a 5))
  (check-equal? (lens-set fifth-lens '(1 2 3 4 5) 'a) '(1 2 3 4 a)))

(define (assoc-swap assoc-list old-assoc-pair new-assoc-pair #:is-equal? [equal? equal?])
  (define (swap-assoc-pair assoc-pair)
    (if (equal? assoc-pair old-assoc-pair)
        new-assoc-pair
        assoc-pair))
  (map swap-assoc-pair assoc-list))

(module+ test
  (define assoc-list '((a 1) (b 2) (c 3)))
  (check-equal? (assoc-swap assoc-list '(b 2) '(FOO BAR))
                '((a 1) (FOO BAR) (c 3))))

(define ((assoc-lens assoc-key #:is-equal? [equal? equal?]) assoc-list)
  (define assoc-pair (assoc assoc-key assoc-list equal?))
  (define (assoc-set new-assoc-pair)
    (if assoc-pair
        (assoc-swap assoc-list assoc-pair new-assoc-pair #:is-equal? equal?)
        (append assoc-list new-assoc-pair)))
  (values assoc-pair assoc-set))

(module+ test
  (define assoc-a-lens (assoc-lens 'a))
  (check-equal? (lens-view assoc-a-lens assoc-list) '(a 1))
  (check-equal? (lens-set assoc-a-lens assoc-list '(FOO BAR))
                '((FOO BAR) (b 2) (c 3))))

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

(module+ test
  (define assf>10-lens (assf-lens (λ (v) (> v 10))))
  (define assf-list '((1 a) (10 b) (100 c)))
  (check-equal? (lens-view assf>10-lens assf-list) '(100 c))
  (check-equal? (lens-set assf>10-lens assf-list '(FOO BAR))
                '((1 a) (10 b) (FOO BAR))))