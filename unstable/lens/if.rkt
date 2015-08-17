#lang racket/base

(provide lens-if
         lens-cond
         )

(require lens/base/main
         racket/function
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit lens/list/main lens/vector/main lens/string))

(define (lens-if pred lens1 lens2)
  (make-lens
   (λ (tgt)
     (if (pred tgt)
         (lens-view lens1 tgt)
         (lens-view lens2 tgt)))
   (λ (tgt nvw)
     (if (pred tgt)
         (lens-set lens1 tgt nvw)
         (lens-set lens2 tgt nvw)))))

(define (any? x) #t)

(define-syntax lens-cond
  (syntax-parser #:literals (else)
    [(lens-cond [pred-expr:expr lens-expr:expr] ... [else else-lens-expr:expr])
     #'(lens-cond [pred-expr lens-expr] ... [any? else-lens-expr])]
    [(lens-cond [pred-expr:expr lens-expr:expr] ...)
     #:with [pred ...] (generate-temporaries #'[pred-expr ...])
     #:with [lens ...] (generate-temporaries #'[lens-expr ...])
     #'(let ([pred pred-expr] ... [lens lens-expr] ...)
         (make-lens
          (λ (tgt)
            (cond [(pred tgt) (lens-view lens tgt)]
                  ...
                  [else (error 'lens-cond "expected ~a, given: ~v" '(or/c pred-expr ...) tgt)]))
          (λ (tgt nvw)
            (cond [(pred tgt) (lens-set lens tgt nvw)]
                  ...
                  [else (error 'lens-cond "expected ~a, given: ~v" '(or/c pred-expr ...) tgt)]))))]))

(module+ test
  (define if-lens (lens-if list? first-lens (vector-ref-lens 0)))
  (check-equal? (lens-view if-lens '(1 2 3)) 1)
  (check-equal? (lens-view if-lens '#(1 2 3)) 1)
  (check-equal? (lens-set if-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set if-lens '#(1 2 3) 'a) '#(a 2 3))
  (define cond-lens (lens-cond [list? first-lens]
                               [vector? (vector-ref-lens 0)]
                               [string? (string-ref-lens 0)]))
  (check-equal? (lens-view cond-lens '(1 2 3)) 1)
  (check-equal? (lens-view cond-lens '#(1 2 3)) 1)
  (check-equal? (lens-view cond-lens "123") #\1)
  (check-equal? (lens-set cond-lens '(1 2 3) 'a) '(a 2 3))
  (check-equal? (lens-set cond-lens '#(1 2 3) 'a) '#(a 2 3))
  (check-equal? (lens-set cond-lens "123" #\a) "a23")
  )
