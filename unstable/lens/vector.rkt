#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [vector-ref-lens
           (-> exact-nonnegative-integer?
               (lens/c immutable-vector? any/c))]
          [vector-ref-nested-lens
           (->* [] #:rest (listof exact-nonnegative-integer?)
                (lens/c immutable-vector? any/c))]
          [vector-pick-lens
           (->* [] #:rest (listof exact-nonnegative-integer?)
                (lens/c immutable-vector? immutable-vector?))]
          ))

(require fancy-app
         lens/base/main
         lens/util/immutable
         "arrow.rkt"
         "join.rkt")

(module+ test
  (require rackunit))


(define (vector-ref-lens i)
  (make-lens
   (vector-ref _ i)
   (vector-set _ i _)))

(define (vector-set v i x)
  (build-immutable-vector
   (vector-length v)
   (λ (j)
     (if (= i j)
         x
         (vector-ref v j)))))

(module+ test
  (check-equal? (lens-view (vector-ref-lens 0) #(a b c)) 'a)
  (check-equal? (lens-set (vector-ref-lens 2) #(a b c) "C") #(a b "C")))


(define (vector-ref-nested-lens . is)
  (apply lens-thrush (map vector-ref-lens is)))

(module+ test
  (check-equal? (lens-transform (vector-ref-nested-lens 2 1)
                                #(a #(b c) #(d e f))
                                symbol->string)
                #(a #(b c) #(d "e" f))))


(define (vector-pick-lens . is)
  (apply lens-join/vector (map vector-ref-lens is)))

(module+ test
  (define 1-5-6-lens (vector-pick-lens 1 5 6))
  (check-equal? (lens-view 1-5-6-lens #(a b c d e f g))
                #(b f g))
  (check-equal? (lens-set 1-5-6-lens #(a b c d e f g) #(1 2 3))
                #(a 1 c d e 2 3)))
