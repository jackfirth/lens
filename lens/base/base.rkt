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

(struct lens-struct (get set)
  #:property prop:procedure
  (lambda (this target)
    (if (lenses-applicable?)
        ((lens-struct-get this) target)
        (error "cannot apply a non-applicable lens as a function"))))

(module+ test
  (require rackunit)
  (check-exn exn:fail? (thunk (first-lens '(a b c)))))

(define lens? lens-struct?)

(define (make-lens getter setter)
  (lens-struct getter setter))

(define (focus-lens lens target)
  (match-define (lens-struct get set) lens)
  (values (get target)
          (set target _)))


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
