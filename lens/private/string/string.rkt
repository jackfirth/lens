#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [string-ref-lens
           (-> exact-nonnegative-integer?
               (lens/c immutable-string? char?))]
          [string-pick-lens
           (->* [] #:rest (listof exact-nonnegative-integer?)
                (lens/c immutable-string? immutable-string?))]
          ))

(require fancy-app
         "../base/main.rkt"
         "../util/immutable.rkt"
         "../string/join-string.rkt"
         "../compound/main.rkt")

(module+ test
  (require rackunit "../test-util/test-lens.rkt"))


(define (string-ref-lens i)
  (make-lens
   (string-ref _ i)
   (string-set _ i _)))

(define (string-set s i c)
  (build-immutable-string
   (string-length s)
   (λ (j)
     (if (= i j)
         c
         (string-ref s j)))))

(module+ test
  (check-lens-view (string-ref-lens 2) "abc" #\c)
  (check-lens-set (string-ref-lens 0) "abc" #\A "Abc"))


(define (string-pick-lens . is)
  (apply lens-join/string (map string-ref-lens is)))

(module+ test
  (define 1-5-6-lens (string-pick-lens 1 5 6))
  (check-lens-view 1-5-6-lens "abcdefg"
                   "bfg")
  (check-lens-set 1-5-6-lens "abcdefg" "BFG"
                  "aBcdeFG"))
