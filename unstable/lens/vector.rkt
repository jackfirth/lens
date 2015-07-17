#lang racket/base

(provide vector-ref-lens
         vector-ref-nested-lens
         vector-pluck-lens
         )

(require fancy-app
         lens/base/main
         "arrow.rkt"
         "join.rkt"
         )
(module+ test
  (require rackunit))

(define (vector-ref-lens i)
  (make-lens
   (vector-ref _ i)
   (vector-set _ i _)))

(define (vector-set v i x)
  (vector->immutable-vector
   (build-vector (vector-length v)
                 (λ (j)
                   (if (= i j)
                       x
                       (vector-ref v j))))))

(define (vector-ref-nested-lens . is)
  (apply lens-thrush (map vector-ref-lens is)))

(define (vector-pluck-lens . is)
  (apply lens-join/vector (map vector-ref-lens is)))


(module+ test
  (check-equal? (lens-view (vector-ref-lens 0) #(a b c)) 'a)
  (check-equal? (lens-view (vector-ref-lens 1) #(a b c)) 'b)
  (check-equal? (lens-view (vector-ref-lens 2) #(a b c)) 'c)
  (check-equal? (lens-set (vector-ref-lens 0) #(a b c) "A") #("A" b c))
  (check-equal? (lens-set (vector-ref-lens 1) #(a b c) "B") #(a "B" c))
  (check-equal? (lens-set (vector-ref-lens 2) #(a b c) "C") #(a b "C"))
  (check-equal? (lens-transform (vector-ref-nested-lens 2 1) #(a #(b c) #(d e f)) symbol->string)
                #(a #(b c) #(d "e" f)))
  (define 1-5-6-lens (vector-pluck-lens 1 5 6))
  (check-equal? (lens-view 1-5-6-lens #(a b c d e f g))
                #(b f g))
  (check-equal? (lens-set 1-5-6-lens #(a b c d e f g) #(1 2 3))
                #(a 1 c d e 2 3))
  )
