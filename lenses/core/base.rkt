#lang racket

(require fancy-app)

(module+ test
  (require rackunit))

(provide let-lens
         make-lens
         (struct-out lens-struct))


(define lens-2-val-context-key
  (make-continuation-mark-key 'lens-2-val-context-key))

(define-syntax-rule (let/immediate-mark [val-id key-expr] body-expr ...)
  (call-with-immediate-continuation-mark key-expr (lambda (val-id) body-expr ...)))


(define (first-value v _) v)


(struct lens-struct (proc)
  #:property prop:procedure
  (lambda (this target)
    (let/immediate-mark [lens-2-val-context? lens-2-val-context-key]
      (if lens-2-val-context?
          ((lens-struct-proc this) target)
          (call-with-values (thunk ((lens-struct-proc this) target))
                            first-value)))))

(define (lens-proc lns)
  (match lns
    [(lens-struct proc) proc]
    [(? procedure?  proc) proc]))

(define ((make-lens getter setter) v)
  (values (getter v)
          (setter v _))) ; fancy-app


(define-syntax-rule (let-lens (view setter) lens-call-expr body ...)
  (let-values ([(view setter) (with-continuation-mark lens-2-val-context-key #t
                                lens-call-expr)])
    body ...))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (let-lens (view-first setter-first) (first-lens '(1 2 3 4 5))
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5))))

