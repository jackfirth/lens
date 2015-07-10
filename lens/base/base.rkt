#lang racket

(require fancy-app)

(module+ test
  (require rackunit))

(provide let-lens
         (contract-out [make-lens (-> (-> any/c any/c)
                                      (-> any/c any/c any/c)
                                      lens?)]
                       [focus-lens (-> lens? any/c
                                       (values any/c (-> any/c any/c)))]
                       [use-applicable-lenses! (-> void?)]
                       [lens? predicate/c]))


(define lenses-applicable? (make-parameter #f))

(define (use-applicable-lenses!)
  (lenses-applicable? #t))

(struct lens (proc)
  #:property prop:procedure
  (lambda (this target)
    (if (lenses-applicable?)
        (let-lens (view _) this target view)
        (error "cannot apply a non-applicable lens as a function"))))

(module+ test
  (require rackunit)
  (check-exn exn:fail? (thunk (first-lens '(a b c)))))

(define (make-lens getter setter)
  (lens (λ (target) (values (getter target) (setter target _)))))

(define (focus-lens lens target)
  ((lens-proc lens) target))


(define-syntax-rule (let-lens (view setter) lens-expr target-expr body ...)
  (let-values ([(view setter) (focus-lens lens-expr target-expr)])
    body ...))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (let-lens (view-first setter-first) first-lens '(1 2 3 4 5)
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))
