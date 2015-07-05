#lang racket

(require fancy-app)

(provide make-lens
         let-lens
         lens-struct
         lens-struct?
         lens-proc
         use-applicable-lenses!)

(module+ test
  (require rackunit))


(define lens-application-context? (make-parameter #f))
(define (use-applicable-lenses!) (lens-application-context? #t))

(define-syntax-rule (with-both-lens-values body ...)
  (parameterize ([lens-application-context? #f]) body ...))


(define (first-value v _) v)

(struct lens-struct (proc)
  #:property prop:procedure
  (lambda (this target)
    (if (lens-application-context?)
        (call-with-values (thunk ((lens-struct-proc this) target)) first-value)
        ((lens-struct-proc this) target))))

(define (lens-proc lns)
  (match lns
    [(lens-struct proc) proc]
    [(? procedure?  proc) proc]))


(define ((make-lens-proc getter setter) v)
  (values (getter v)
          (setter v _)))


(define (make-lens getter setter)
  (lens-struct (make-lens-proc getter setter)))

(define-syntax-rule (let-lens (view setter) lens-expr target-expr body ...)
  (let ([lens lens-expr]
        [target target-expr])
    (let-values ([(view setter) (with-both-lens-values (lens target))])
      body ...)))


(module+ test
  (define (set-first lst v)
    (list* v (rest lst)))
  (define first-lens (make-lens first set-first))
  (check-equal? (let-lens (view _) first-lens '(1 2 3) view) 1)
  (check-equal? (let-lens (_ context) first-lens '(1 2 3) (context 'a)) '(a 2 3)))
