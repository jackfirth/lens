#lang racket

(require racket/match
         syntax/parse/define
         fancy-app
         srfi/1
         "core.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))

(provide car-lens
         cdr-lens
         take-lens
         drop-lens
         list-ref-lens
         list-lens
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
         ;; cadr-lens etc. provided by macro
         assoc-lens
         assv-lens
         assq-lens
         )

(module+ test
  (require rackunit))

(define (car-lens v)
  (match-define (cons car cdr) v)
  (values car (cons _ cdr))) ; fancy-app

(define (cdr-lens v)
  (match-define (cons car cdr) v)
  (values cdr (cons car _)))

(define ((take-lens n) lst)
  (define-values [fst-lst rst-lst] (split-at lst n))
  (values fst-lst (append _ rst-lst)))

(define ((drop-lens n) lst)
  (define-values [fst-lst rst-lst] (split-at-reverse lst n))
  (values rst-lst (append-reverse fst-lst _)))

(define (list-ref-lens i)
  (lens-compose car-lens (drop-lens i)))

(define (list-lens i)
  (list-ref-lens i))

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

(define (c_r->lens sym)
  (apply lens-compose
         (for/list ([char (in-string (symbol->string sym))])
           (case char [(#\a) car-lens] [(#\d) cdr-lens]))))
(define-simple-macro (define-c_r-lens id:id)
  #:with c_r-lens (format-id #'id "c~ar-lens" #'id #:source #'id #:props #'id)
  (begin (provide c_r-lens) (define c_r-lens (c_r->lens 'id))))
(define-simple-macro (define-c_r-lenses id:id ...)
  (begin (define-c_r-lens id) ...))

(define-c_r-lenses
  aa ad da dd
  aaa aad ada add
  daa dad dda ddd
  aaaa aaad aada aadd
  adaa adad adda addd
  daaa daad dada dadd
  ddaa ddad ddda dddd
  )

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
  (check-equal? (lens-set fifth-lens '(1 2 3 4 5) 'a) '(1 2 3 4 a))
  (check-equal? (lens-transform cdaddr-lens list->vector '(9 8 (6 5 4 3 2 1) 7))
                '(9 8 (6 . #(5 4 3 2 1)) 7))
  )


(define (assoc-swap assoc-list old-assoc-pair new-assoc-pair #:is-equal? [equal? equal?])
  (define (swap-assoc-pair assoc-pair)
    (if (equal? assoc-pair old-assoc-pair)
        new-assoc-pair
        assoc-pair))
  (map swap-assoc-pair assoc-list))

(define (assoc-set assoc-list key value #:is-equal? [equal? equal?])
  (define (set-assoc-pair assoc-pair)
    (if (equal? (first assoc-pair) key)
        (list (first assoc-pair) value)
        assoc-pair))
  (map set-assoc-pair assoc-list))

(module+ test
  (define assoc-list '((a 1) (b 2) (c 3)))
  (check-equal? (assoc-swap assoc-list '(b 2) '(FOO BAR))
                '((a 1) (FOO BAR) (c 3))))


(define ((assoc-lens key #:is-equal? [equal? equal?]) assoc-list)
  (define assoc-pair (assoc key assoc-list equal?))
  (define (assoc-lens-set v)
    (if assoc-pair
        (assoc-set assoc-list key v #:is-equal? equal?)
        (append assoc-list (list (list key v)))))
  (values (and assoc-pair (second assoc-pair))
          assoc-lens-set))

(module+ test
  (define assoc-a-lens (assoc-lens 'a))
  (define assoc-d-lens (assoc-lens 'd))
  (check-equal? (lens-view assoc-a-lens assoc-list) 1)
  (check-equal? (lens-set assoc-a-lens assoc-list 100)
                '((a 100) (b 2) (c 3)))
  (check-false (lens-view assoc-d-lens assoc-list))
  (check-equal? (lens-set assoc-d-lens assoc-list 4)
                '((a 1) (b 2) (c 3) (d 4)))
  (define assoc-foo-lens (assoc-lens "foo"))
  (define assoc-str '(("bar" 1) ("foo" 2) ("baz" 3)))
  (check-equal? (lens-view assoc-foo-lens assoc-str) 2)
  (check-equal? (lens-set assoc-foo-lens assoc-str 100)
                '(("bar" 1) ("foo" 100) ("baz" 3))))


(define (assv-lens assv-key)
  (assoc-lens assv-key #:is-equal? eqv?))

(module+ test
  (define assv-2-lens (assv-lens 2))
  (define assv-list '((1 a) (2 b) (3 c)))
  (check-eq? (lens-view assv-2-lens assv-list) 'b)
  (check-equal? (lens-set assv-2-lens assv-list 'FOO)
                '((1 a) (2 FOO) (3 c))))


(define (assq-lens assq-key)
  (assoc-lens assq-key #:is-equal? eq?))

(module+ test
  (define assq-a-lens (assq-lens 'a))
  (define assq-list '((a 1) (b 2) (c 3)))
  (check-eqv? (lens-view assq-a-lens assq-list) 1)
  (check-equal? (lens-set assq-a-lens assq-list 100)
                '((a 100) (b 2) (c 3))))
