#lang racket/base

(provide stream-first-lens
         stream-rest-lens
         stream-ref-lens)

(require racket/stream
         fancy-app
         "base/main")

(module+ test
  (require rackunit "test-util/test-lens"))

(define (stream-ref-lens i)
  (lens-compose stream-first-lens (stream-tail-lens i)))

(define (stream-set-first s v)
  (stream-cons v (stream-rest s)))

(define (stream-set-rest s rst)
  (stream-cons (stream-first s) rst))

(define stream-first-lens
  (make-lens
   stream-first
   stream-set-first))

(define stream-rest-lens
  (make-lens
   stream-rest
   stream-set-rest))

(define (stream-tail-lens i)
  (make-lens
   (stream-tail _ i)
   (stream-set-tail _ i _)))

(define (stream-set-tail s i rst)
  (define rev-fst
    (for/fold ([rev-fst '()]) ([v (in-stream s)] [_ (in-range i)])
      (cons v rev-fst)))
  (for/fold ([rst rst]) ([v (in-list rev-fst)])
    (stream-cons v rst)))

(define (stream-ref-nested-lens . is)
  (apply lens-thrush (map stream-ref-lens is)))

(module+ test
  (check-view     stream-first-lens   (stream 'a 'b 'c) 'a)
  (check-view     (stream-ref-lens 2) (stream 'a 'b 'c) 'c)
  (check-set-view stream-first-lens   (stream 'a 'b 'c) (gensym))
  (check-set-view (stream-ref-lens 2) (stream 'a 'b 'c) (gensym))
  (check-set-set  stream-first-lens   (stream 'a 'b 'c) (gensym) (gensym))
  (check-set-set  (stream-ref-lens 2) (stream 'a 'b 'c) (gensym) (gensym))
  )
