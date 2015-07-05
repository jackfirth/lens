#lang racket

(require unstable/contract
         "base.rkt")

(module+ test
  (require rackunit))

(provide lens/c
         lens-proc/c)


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
  (define list-lens/c (lens/c list? any/c))
  (check-true (contract? list-lens/c))
  (check-false (flat-contract? list-lens/c)))


