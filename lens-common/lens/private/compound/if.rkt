#lang racket/base

(provide lens-if
         lens-cond
         lens-match
         )

(require lens/private/base/main
         racket/match
         (for-syntax racket/base
                     syntax/parse
                     ))

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
                  [else (raise-lens-cond-error tgt 'pred-expr ...)]))
          (λ (tgt nvw)
            (cond [(pred tgt) (lens-set lens tgt nvw)]
                  ...
                  [else (raise-lens-cond-error tgt 'pred-expr ...)]))))]))

(define (raise-lens-cond-error tgt . pred-expr-syms)
  (raise-arguments-error 'lens-cond "no matching clause for target"
                         "target" tgt
                         "expected" `(or/c ,@pred-expr-syms)))

(define-syntax lens-match
  (syntax-parser
    [(lens-match [pat:expr lens-expr:expr] ...)
     #'(make-lens
        (λ (tgt)
          (match tgt
            [pat (lens-view lens-expr tgt)]
            ...))
        (λ (tgt nvw)
          (match tgt
            [pat (lens-set lens-expr tgt nvw)]
            ...)))]))
