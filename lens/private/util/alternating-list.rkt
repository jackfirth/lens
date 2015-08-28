#lang sweet-exp racket/base

provide alternating->assoc-list
        assoc->alternating-list
        keys+values->assoc-list
        assoc-list->keys+values
        keys+values->alternating-list
        alternating-list->keys+values

require racket/list
        racket/match
        unstable/sequence
module+ test
  require rackunit

(define (alternating->assoc-list alternating-list)
  (for/list ([lst (in-slice 2 alternating-list)])
    (match-define (list a b) lst)
    (cons a b)))

(define (assoc->alternating-list alist)
  (append*
   (for/list ([(k v) (in-pairs alist)])
     (list k v))))

(define (keys+values->assoc-list keys values)
  (map cons keys values))

(define (assoc-list->keys+values alist)
  (values (map car alist)
          (map cdr alist)))

(define (keys+values->alternating-list keys values)
  (append-map list keys values))

(define (alternating-list->keys+values alternating-list)
  (for/lists (ks vv) ([lst (in-slice 2 alternating-list)])
    (match-define (list k v) lst)
    (values k v)))

module+ test
  (check-equal? (alternating->assoc-list '(a 1 b 2)) '((a . 1) (b . 2)))
  (check-equal? (alternating->assoc-list '(b 2 a 1)) '((b . 2) (a . 1)))
  (check-equal? (assoc->alternating-list '((a . 1) (b . 2))) '(a 1 b 2))
  (check-equal? (assoc->alternating-list '((b . 2) (a . 1))) '(b 2 a 1))
  (check-equal? (keys+values->assoc-list '(a b) '(1 2)) '((a . 1) (b . 2)))
  (check-equal? (keys+values->assoc-list '(b a) '(2 1)) '((b . 2) (a . 1)))
  (check-equal? (keys+values->alternating-list '(a b) '(1 2)) '(a 1 b 2))
  (check-equal? (keys+values->alternating-list '(b a) '(2 1)) '(b 2 a 1))
  (let-values ([(ks vs) (assoc-list->keys+values '((a . 1) (b . 2)))])
    (check-equal? ks '(a b))
    (check-equal? vs '(1 2)))
  (let-values ([(ks vs) (assoc-list->keys+values '((b . 2) (a . 1)))])
    (check-equal? ks '(b a))
    (check-equal? vs '(2 1)))
  (let-values ([(ks vs) (alternating-list->keys+values '(a 1 b 2))])
    (check-equal? ks '(a b))
    (check-equal? vs '(1 2)))
  (let-values ([(ks vs) (alternating-list->keys+values '(b 2 a 1))])
    (check-equal? ks '(b a))
    (check-equal? vs '(2 1)))
