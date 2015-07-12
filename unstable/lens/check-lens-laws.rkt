#lang racket/base

(require racket/match
         syntax/parse/define
         lens/base/main
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require lens/list/main racket/stream racket/contract))

(define-simple-macro
  (define-equation-checker name:id [arg:id ...] lhs-expr:expr rhs-expr:expr)
  #:with [i ...] (generate-temporaries #'[arg ...])
  #:with {{clause ...} ...}
  #'{{[(arg i) (in-indexed arg)] #:break (<= n i)} ...}
  (define (name n equal? arg ...)
    (define fail
      (for*/first (clause ... ...
                   [lhs (in-value lhs-expr)] [rhs (in-value rhs-expr)]
                   #:when (not (equal? lhs rhs)))
        (list lhs rhs arg ...)))
    (if fail
        (match-let ([(list lhs rhs arg ...) fail])
          (error name
                 (string-append "failure: ~s = ~s does not hold\n"
                                "  lhs: ~v\n"
                                "  rhs: ~v\n"
                                (format "  ~a: ~~v\n" 'arg) ...)
                 'lhs-expr 'rhs-expr lhs rhs arg ...))
        #t)))

(define-equation-checker check-set-get [L T V]
  (lens-view L (lens-set L T V))
  V)

(define-equation-checker check-get-set [L T]
  (lens-set L T (lens-view L T))
  T)

(define-equation-checker check-last-set [L T V1 V2]
  (lens-view L (lens-set L (lens-set L T V1) V2))
  V2)

(define (check n equal? Ls Ts Vs)
  (and
   (check-set-get n equal? Ls Ts Vs)
   (check-get-set n equal? Ls Ts)
   (check-last-set n equal? Ls Ts Vs Vs)))

(module+ test
  (check 10000 equal?
         (in-value car-lens)
         (in-producer contract-random-generate #f pair?)
         (in-producer gensym))
  (check 10000 equal?
         (in-value cdr-lens)
         (in-producer contract-random-generate #f pair?)
         (in-producer gensym))
  )
