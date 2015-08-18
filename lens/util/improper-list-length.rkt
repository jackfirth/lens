#lang racket/base

(provide list*-length
         list*-length-at-least/c
         )

(require racket/contract/base)
(module+ test
  (require rackunit))

(define (list*-length lst)
  (let loop ([len 0] [lst lst])
    (cond [(pair? lst)
           (loop (add1 len) (cdr lst))]
          [else len])))

(define (list*-length-at-least/c i)
  (define (pred lst)
    (let loop ([i i] [lst lst])
      (cond [(<= i 0) #t]
            [(pair? lst) (loop (sub1 i) (cdr lst))]
            [else #f])))
  (flat-named-contract
   `(list*-length-at-least/c ,i)
   pred))

(module+ test
  (check-equal? (list*-length '()) 0)
  (check-equal? (list*-length '(a)) 1)
  (check-equal? (list*-length '(a b)) 2)
  (check-equal? (list*-length '(a b c)) 3)
  (check-equal? (list*-length "whatever") 0)
  (check-equal? (list*-length 'a) 0)
  (check-equal? (list*-length '(a . b)) 1)
  (check-equal? (list*-length '(a b . c)) 2)
  (check-equal? (list*-length '(a b c . d)) 3)
  (check-true  ((list*-length-at-least/c 0) 'a))
  (check-false ((list*-length-at-least/c 1) 'a))
  (check-true  ((list*-length-at-least/c 1) '(a . b)))
  (check-false ((list*-length-at-least/c 2) '(a . b)))
  (check-true  ((list*-length-at-least/c 2) '(a b . c)))
  (check-false ((list*-length-at-least/c 3) '(a b . c)))
  (check-true  ((list*-length-at-least/c 3) '(a b c . d)))
  (check-false ((list*-length-at-least/c 4) '(a b c . d)))
  )
