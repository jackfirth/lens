#lang racket/base

(require racket/contract/base)
(provide gen:lens
         let-lens
         (rename-out [lens/c gen-lens/c])
         (contract-out 
                       [lens? predicate/c]
                       [lens-view (-> lens? any/c any/c)]
                       [lens-set (-> lens? any/c any/c any/c)]
                       [focus-lens (-> lens? any/c
                                       (values any/c (-> any/c any/c)))]
                       [use-applicable-lenses! (-> void?)]
                       ))

(require racket/generic fancy-app)

(define-generics lens
  (lens-view lens target)
  (lens-set lens target x)
  (focus-lens lens target)
  #:fallbacks
  [(define/generic gen-lens-view lens-view)
   (define/generic gen-lens-set lens-set)
   (define (lens-view lens target)
     (let-lens (view _) lens target view))
   (define (lens-set lens target x)
     (let-lens (_ setter) lens target
       (setter x)))
   (define (focus-lens lens target)
     (values (gen-lens-view lens target)
             (gen-lens-set lens target _)))]
  #:derive-property prop:procedure
  (lambda (this target)
    (if (lenses-applicable?)
        (lens-view this target)
        (error "cannot apply a non-applicable lens as a function"))))

(define lenses-applicable? (make-parameter #f))

(define (use-applicable-lenses!)
  (lenses-applicable? #t))

(define-syntax-rule (let-lens (view context) lens-expr target-expr body ...)
  (let-values ([(view context) (focus-lens lens-expr target-expr)])
    body ...))

