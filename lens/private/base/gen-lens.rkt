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
  #:defined-predicate lens-implements?
  #:fallbacks
  [(define/generic gen-lens-view lens-view)
   (define/generic gen-lens-set lens-set)
   (define/generic gen-focus-lens focus-lens)
   (define (lens-view lens target)
     (unless (lens-implements? lens 'focus-lens)
       (error 'lens-view "not implemented for ~v" lens))
     (let-values ([(view _) (gen-focus-lens lens target)])
       view))
   (define (lens-set lens target x)
     (unless (lens-implements? lens 'focus-lens)
       (error 'lens-set "not implemented for ~v" lens))
     (let-values ([(_ setter) (gen-focus-lens lens target)])
       (setter x)))
   (define (focus-lens lens target)
     (unless (lens-implements? lens 'lens-view 'lens-set)
       (error 'focus-lens "not implemented for ~v" lens))
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

