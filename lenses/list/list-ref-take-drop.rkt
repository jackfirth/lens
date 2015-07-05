#lang racket/base

(provide list-ref-lens
         take-lens
         drop-lens
         first-lens
         second-lens
         third-lens
         fourth-lens
         fifth-lens
         sixth-lens
         seventh-lens
         eighth-lens
         nineth-lens
         tenth-lens
         )

(require racket/list
         (only-in srfi/1 append-reverse)
         fancy-app
         "../core.rkt"
         "car-cdr.rkt"
         )
(module+ test
  (require rackunit))

(define ((take-lens n) lst)
  (define-values [fst-lst rst-lst] (split-at lst n))
  (values fst-lst (append _ rst-lst)))

(define ((drop-lens n) lst)
  (define-values [fst-lst rst-lst] (split-at-reverse lst n))
  (values rst-lst (append-reverse fst-lst _)))

(define (list-ref-lens i)
  (lens-compose car-lens (drop-lens i)))

(define first-lens (list-ref-lens 0))
(define second-lens (list-ref-lens 1))
(define third-lens (list-ref-lens 2))
(define fourth-lens (list-ref-lens 3))
(define fifth-lens (list-ref-lens 4))
(define sixth-lens (list-ref-lens 5))
(define seventh-lens (list-ref-lens 6))
(define eighth-lens (list-ref-lens 7))
(define nineth-lens (list-ref-lens 8))
(define tenth-lens (list-ref-lens 9))

(module+ test
  (check-eqv? (lens-view first-lens  '(1 2 3 4 5)) 1)
  (check-eqv? (lens-view second-lens '(1 2 3 4 5)) 2)
  (check-eqv? (lens-view third-lens  '(1 2 3 4 5)) 3)
  (check-eqv? (lens-view fourth-lens '(1 2 3 4 5)) 4)
  (check-eqv? (lens-view fifth-lens  '(1 2 3 4 5)) 5)
  (check-equal? (lens-set first-lens  '(1 2 3 4 5) 'a) '(a 2 3 4 5))
  (check-equal? (lens-set second-lens '(1 2 3 4 5) 'a) '(1 a 3 4 5))
  (check-equal? (lens-set third-lens  '(1 2 3 4 5) 'a) '(1 2 a 4 5))
  (check-equal? (lens-set fourth-lens '(1 2 3 4 5) 'a) '(1 2 3 a 5))
  (check-equal? (lens-set fifth-lens  '(1 2 3 4 5) 'a) '(1 2 3 4 a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modified from split-at in racket/list
(define (split-at-reverse list0 n0)
  (let loop ([list list0] [n n0] [rev-pfx '()])
    (cond [(zero? n) (values rev-pfx list)]
          [(pair? list) (loop (cdr list) (sub1 n) (cons (car list) rev-pfx))]
          [else (raise-arguments-error
                 'split-at-reverse
                 (if (list? list0) "index is too large for list" "index reaches a non-pair")
                 "index" n0
                 (if (list? list0) "list" "in")
                 list0)])))

