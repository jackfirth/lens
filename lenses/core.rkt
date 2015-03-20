#lang racket

(require fancy-app)

(provide lens/c
         make-lens
         let-lens
         lens-view
         lens-set
         lens-transform
         lens-compose)

(module+ test
  (require rackunit))


(define (lens/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(module+ test
  (define list-lens (lens/c list? any/c))
  (check-pred chaperone-contract? list-lens))


(define ((make-lens getter setter) v)
  (values (getter v)
          (setter v _)))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define test-list '(1 2 3))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-view first-lens test-list) 1)
  (check-equal? (lens-set first-lens test-list 'a) '(a 2 3)))


(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) lens-call-expr])
    body ...))

(module+ test
  (let-lens (view-first setter-first) (first-lens '(1 2 3 4 5))
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))


(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
    (setter x)))

(module+ test
  (check-eqv? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3)))


(define (lens-transform lens f v)
  (let-lens (view setter) (lens v)
    (setter (f view))))

(module+ test
  (check-equal? (lens-transform first-lens number->string '(1 2 3)) '("1" 2 3)))


(define ((lens-compose2 sub-lens super-lens) v)
  (let-lens (super-view super-setter) (super-lens v)
    (let-lens (sub-view sub-setter) (sub-lens super-view)
      (values sub-view
              (compose super-setter sub-setter)))))

(module+ test
  (define (second-set l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second second-set))
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
