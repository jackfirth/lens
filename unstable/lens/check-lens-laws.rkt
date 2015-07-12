#lang racket/base

(require racket/match
         syntax/parse/define
         lens/base/main
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require lens/list/main racket/stream racket/contract))

(define (make-equation-checker name lhs-s-exp rhs-s-exp lhs-f/LTV rhs-f/LTV)
  (define (check L Ts Vs n [equal? equal?])
    (define fail
      (for*/first ([(T i1) (in-indexed Ts)]
                   #:break (<= n i1)
                   [(V i2) (in-indexed Vs)]
                   #:break (<= n i2)
                   [lhs (in-value (lhs-f/LTV L T V))] [rhs (in-value (rhs-f/LTV L T V))]
                   #:when (not (equal? lhs rhs)))
        (list T V lhs rhs)))
    (if fail
        (match-let ([(list T V lhs rhs) fail])
          (error name
                 (string-append "failure: ~s = ~s does not hold\n"
                                "  L: ~v\n"
                                "  T: ~v\n"
                                "  V: ~v\n"
                                "  lhs: ~v\n"
                                "  rhs: ~v\n")
                 lhs-s-exp rhs-s-exp L T V lhs rhs))
        #t))
  (procedure-rename check name))

(define-simple-macro
  (define-equation-checker name:id lhs:expr rhs:expr)
  #:with [-L -T -V] (syntax-local-introduce #'[L T V])
  (define name
    (make-equation-checker 'name 'lhs 'rhs (λ (-L -T -V) lhs) (λ (-L -T -V) rhs))))

(define-equation-checker check-set-get
  (lens-view L (lens-set L T V))
  V)

(define-equation-checker check-get-set
  (lens-set L T (lens-view L T))
  T)

(define (check L Ts Vs n [equal? equal?])
  (and
   (check-set-get L Ts Vs n equal?)
   (check-get-set L Ts Vs n equal?)))

(module+ test
  (check car-lens
         (in-producer contract-random-generate #f pair?)
         (in-producer gensym)
         10000)
  (check cdr-lens
         (in-producer contract-random-generate #f pair?)
         (in-producer gensym)
         10000)
  )
