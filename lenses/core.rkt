#lang racket

(require rackunit)

(provide lens/c
         let-lens
         lens-view
         lens-set
         lens-transform
         lens-compose)

(module+ test
  (define (first-lens lst)
    (values (first lst)
            (λ (v) (cons v (drop lst 1)))))
  (define (second-lens lst)
    (values (second lst)
            (λ (v)
              (append (take lst 1)
                      (list v)
                      (drop lst 2))))))

;; Lens contract

(define (lens/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(module+ test
  (define list-lens (lens/c list? any/c))
  (check-pred chaperone-contract? list-lens))

;; Lens result local bindings syntax

(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) lens-call-expr])
    body ...))

(module+ test
  (let-lens (view-first setter-first) (first-lens '(1 2 3 4 5))
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))

;; Helpers for only working with one half of a lens

(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
    (setter x)))

(module+ test
  (check-eqv? (lens-view second-lens '(1 2 3)) 2)
  (check-equal? (lens-set second-lens '(1 2 3) 'a) '(1 a 3)))

;; Composing a lens with a function to make a value-sensitive setter

(define (lens-transform lens f v)
  (let-lens (view setter) (lens v)
    (setter (f view))))

(module+ test
  (check-equal? (lens-transform second-lens number->string '(1 2 3)) '(1 "2" 3)))

;; Lens composition

(define ((lens-compose2 sub-lens super-lens) v)
  (let-lens (super-view super-setter) (super-lens v)
    (let-lens (sub-view sub-setter) (sub-lens super-view)
      (values sub-view
              (compose super-setter sub-setter)))))

(module+ test
  (define first-of-second-lens (lens-compose2 first-lens second-lens))
  (define test-alist '((a 1) (b 2) (c 3)))
  (check-eq? (lens-view first-of-second-lens test-alist) 'b)
  (check-equal? (lens-set first-of-second-lens test-alist 'B)
                '((a 1) (B 2) (c 3))))

(define ((generalize-operator op) v . vs)
  (if (empty? vs)
      v
      (foldl (λ (next-v previous) (op previous next-v)) v vs)))

(module+ test
  (define (num-append2 n m)
    (+ (* 10 n) m))
  (define num-append (generalize-operator num-append2))
  (check-eqv? (num-append 1 2 3 4 5) 12345)
  (check-eqv? (num-append 1) 1))

(define lens-compose (generalize-operator lens-compose2))