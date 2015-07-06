#lang racket

(require fancy-app)

(module+ test
  (require rackunit))

(provide let-lens
         make-lens
         apply-lens
         use-applicable-lenses!
         (struct-out lens))


(define lenses-applicable? (make-parameter #f))

(define (use-applicable-lenses!)
  (lenses-applicable? #t))

(define (first-value v _) v)


(struct lens (proc)
  #:property prop:procedure
  (lambda (this target)
    (if (lenses-applicable?)
        (let-lens (view _) this target view)
        (error "cannot apply a non-applicable lens as a function"))))

(define ((make-lens-proc getter setter) v)
  (values (getter v)
          (setter v _))) ; fancy-app

(define (make-lens getter setter)
  (lens (make-lens-proc getter setter)))

(define (apply-lens lens target)
  ((lens-proc lens) target))


(define-syntax-rule (let-lens (view setter) lens target body ...)
  (let-values ([(view setter) (apply-lens lens target)])
    body ...))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (let-lens (view-first setter-first) first-lens '(1 2 3 4 5)
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))

