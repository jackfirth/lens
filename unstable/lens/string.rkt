#lang racket/base

(provide string-ref-lens
         string-pluck-lens
         )

(require fancy-app
         lens/base/main
         "join.rkt"
         )
(module+ test
  (require rackunit))

(define (string-ref-lens i)
  (make-lens
   (string-ref _ i)
   (string-set _ i _)))

(define (string-set s i c)
  (string->immutable-string
   (build-string (string-length s)
                 (λ (j)
                   (if (= i j)
                       c
                       (string-ref s j))))))

(define (string-pluck-lens . is)
  (apply lens-join/string (map string-ref-lens is)))


(module+ test
  (check-equal? (lens-view (string-ref-lens 0) "abc") #\a)
  (check-equal? (lens-view (string-ref-lens 1) "abc") #\b)
  (check-equal? (lens-view (string-ref-lens 2) "abc") #\c)
  (check-equal? (lens-set (string-ref-lens 0) "abc" #\A) "Abc")
  (check-equal? (lens-set (string-ref-lens 1) "abc" #\B) "aBc")
  (check-equal? (lens-set (string-ref-lens 2) "abc" #\C) "abC")
  (define 1-5-6-lens (string-pluck-lens 1 5 6))
  (check-equal? (lens-view 1-5-6-lens "abcdefg")
                "bfg")
  (check-equal? (lens-set 1-5-6-lens "abcdefg" "BFG")
                "aBcdeFG")
  )
