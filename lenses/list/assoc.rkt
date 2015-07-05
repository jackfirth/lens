#lang racket/base

(provide assoc-lens assv-lens assq-lens)

(require racket/list
         "../core.rkt"
         )
(module+ test
  (require rackunit))

(define (assoc-swap assoc-list old-assoc-pair new-assoc-pair #:is-equal? [equal? equal?])
  (define (swap-assoc-pair assoc-pair)
    (if (equal? assoc-pair old-assoc-pair)
        new-assoc-pair
        assoc-pair))
  (map swap-assoc-pair assoc-list))

(define (assoc-set assoc-list key value #:is-equal? [equal? equal?])
  (define (set-assoc-pair assoc-pair)
    (if (equal? (car assoc-pair) key)
        (cons (car assoc-pair) value)
        assoc-pair))
  (map set-assoc-pair assoc-list))

(module+ test
  (define assoc-list '((a . 1) (b . 2) (c . 3)))
  (check-equal? (assoc-swap assoc-list '(b . 2) '(FOO . BAR))
                '((a . 1) (FOO . BAR) (c . 3))))


(define ((assoc-lens key #:is-equal? [equal? equal?]) assoc-list)
  (define assoc-pair (assoc key assoc-list equal?))
  (define (assoc-lens-set v)
    (if assoc-pair
        (assoc-set assoc-list key v #:is-equal? equal?)
        (append assoc-list (list (cons key v)))))
  (values (and assoc-pair (cdr assoc-pair))
          assoc-lens-set))

(module+ test
  (define assoc-a-lens (assoc-lens 'a))
  (define assoc-d-lens (assoc-lens 'd))
  (check-equal? (lens-view assoc-a-lens assoc-list) 1)
  (check-equal? (lens-set assoc-a-lens assoc-list 100)
                '((a . 100) (b . 2) (c . 3)))
  (check-false (lens-view assoc-d-lens assoc-list))
  (check-equal? (lens-set assoc-d-lens assoc-list 4)
                '((a . 1) (b . 2) (c . 3) (d . 4)))
  (define assoc-foo-lens (assoc-lens "foo"))
  (define assoc-str '(("bar" . 1) ("foo" . 2) ("baz" . 3)))
  (check-equal? (lens-view assoc-foo-lens assoc-str) 2)
  (check-equal? (lens-set assoc-foo-lens assoc-str 100)
                '(("bar" . 1) ("foo" . 100) ("baz" . 3))))


(define (assv-lens assv-key)
  (assoc-lens assv-key #:is-equal? eqv?))

(module+ test
  (define assv-2-lens (assv-lens 2))
  (define assv-list '((1 . a) (2 . b) (3 . c)))
  (check-eq? (lens-view assv-2-lens assv-list) 'b)
  (check-equal? (lens-set assv-2-lens assv-list 'FOO)
                '((1 . a) (2 . FOO) (3 . c))))


(define (assq-lens assq-key)
  (assoc-lens assq-key #:is-equal? eq?))

(module+ test
  (define assq-a-lens (assq-lens 'a))
  (define assq-list '((a . 1) (b . 2) (c . 3)))
  (check-eqv? (lens-view assq-a-lens assq-list) 1)
  (check-equal? (lens-set assq-a-lens assq-list 100)
                '((a . 100) (b . 2) (c . 3))))

