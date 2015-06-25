#lang racket

(require unstable/contract
         "core.rkt")

(module+ test
  (require rackunit))

(provide lens/c)



(define (lens-proc/c input subcomponent)
  (-> input
      (values subcomponent
              (-> subcomponent
                  input))))

(define (lens/c target/c view/c)
  (define proc/c (lens-proc/c target/c view/c))
  (if/c lens-struct?
        (struct/c lens-struct proc/c)
        proc/c))

(module+ test
  (define (non-flat-contract? v)
    (and (contract? v) (not (flat-contract? v))))
  (define list-lens/c (lens/c list? any/c))
  (check-pred non-flat-contract? list-lens/c))
