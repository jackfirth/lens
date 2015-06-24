#lang racket

(require fancy-app
         unstable/contract)

(provide lens/c
         make-lens
         let-lens
         lens-view
         lens-set
         lens-transform
         lens-compose
         lens-struct
         lens-proc
         lens-application-context?
         identity-lens)

(module+ test
  (require rackunit))


(define (lens-proc/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(define lens-application-context? (make-parameter #f))

(define-syntax-rule (with-both-lens-values body ...)
  (parameterize ([lens-application-context? #f]) body ...))

(struct lens-struct (proc)
  #:property prop:procedure
  (lambda (this target)
    (if (lens-application-context?)
        (lens-view (lens-struct-proc this) target)
        ((lens-struct-proc this) target))))

(define (lens-proc lns)
  (match lns
    [(lens-struct proc) proc]
    [(? procedure?  proc) proc]))

(define (lens/c target/c view/c)
  (define proc/c (lens-proc/c target/c view/c))
  (if/c lens-struct?
        (struct/c lens-struct proc/c)
        proc/c))

(module+ test
  (define (non-flat-contract? v)
    (and (contract? v) (not (flat-contract? v))))
  (define list-lens (lens/c list? any/c))
  (check-pred non-flat-contract? list-lens))


(define ((make-lens-proc getter setter) v)
  (values (getter v)
          (setter v _)))

(define (make-lens getter setter)
  (lens-struct (make-lens-proc getter setter)))

(define identity-lens
  (lens-struct (values _ identity)))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define test-list '(1 2 3))
  (define first-lens (make-lens first set-first))
  (check-equal? (lens-view first-lens test-list) 1)
  (check-equal? (lens-set first-lens test-list 'a) '(a 2 3))
  (check-equal? (lens-view identity-lens 3) 3)
  (check-equal? (lens-set identity-lens 3 4) 4)
  (check-equal? (lens-compose) identity-lens)
  (define first-lens-proc (lens-proc first-lens))
  (check-equal? (lens-proc first-lens-proc) first-lens-proc)
  (let-values ([(a b) (first-lens-proc '(1 2 3))])
    (check-equal? a 1)
    (check-equal? (b 'a) '(a 2 3))))


(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) (with-both-lens-values lens-call-expr)])
    body ...))

(module+ test
  (let-lens (view-first setter-first) (first-lens '(1 2 3 4 5))
    (check-equal? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5)))
  (let-lens (view-first setter-first) (if #t (first-lens '(1 2 3 4 5)) (values 1 2))
    (check-equal? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))


(define (lens-view lens v)
  (let-lens (view _) (lens v)
    view))

(define (lens-set lens v x)
  (let-lens (_ setter) (lens v)
    (setter x)))

(module+ test
  (check-equal? (lens-view first-lens '(1 2 3)) 1)
  (check-equal? (lens-set first-lens '(1 2 3) 'a) '(a 2 3)))


(define (lens-transform lens f v)
  (let-lens (view setter) (lens v)
    (setter (f view))))

(module+ test
  (check-equal? (lens-transform first-lens number->string '(1 2 3)) '("1" 2 3)))


(define (lens-compose2 sub-lens super-lens)
  (lens-struct
   (lambda (v)
     (let-lens (super-view super-setter) (super-lens v)
       (let-lens (sub-view sub-setter) (sub-lens super-view)
         (values sub-view
                 (compose super-setter sub-setter)))))))

(module+ test
  (define (second-set l v)
    (list* (first l) v (rest (rest l))))
  (define second-lens (make-lens second second-set))
  (define first-of-second-lens (lens-compose first-lens second-lens))
  (define test-alist '((a 1) (b 2) (c 3)))
  (check-equal? (lens-view first-of-second-lens test-alist) 'b)
  (check-equal? (lens-set first-of-second-lens test-alist 'B)
                '((a 1) (B 2) (c 3))))

(define lens-compose
  (compose (foldr lens-compose2 identity-lens _) list))
