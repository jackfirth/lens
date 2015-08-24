#lang sweet-exp racket/base

require racket/contract/base

provide
  contract-out
    stream-first-lens (lens/c stream? any/c)
    stream-rest-lens (lens/c stream? stream?)
    stream-ref-lens (-> exact-nonnegative-integer? (lens/c stream? any/c))

require racket/stream
        fancy-app
        "base/main.rkt"
        "compound/main.rkt"

module+ test
  require rackunit
          "test-util/test-lens.rkt"


module+ test
  (define-check (check-stream-equal? stream1 stream2)
    (equal? (stream->list stream1) (stream->list stream2)))


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

module+ test
  (check-equal? (lens-view stream-first-lens (stream 'a 'b 'c)) 'a)
  (check-equal? (lens-view (stream-ref-lens 2) (stream 'a 'b 'c)) 'c)
  (check-stream-equal? (lens-set stream-first-lens (stream 'a 'b 'c) 1)
                       (stream 1 'b 'c))
  (check-stream-equal? (lens-set (stream-ref-lens 2) (stream 'a 'b 'c) 1)
                       (stream 'a 'b 1))

(define (stream-ref-nested-lens . is)
  (apply lens-thrush (map stream-ref-lens is)))

module+ test
  (check-equal? (lens-view (stream-ref-nested-lens 1 2 0)
                           (stream 'a (stream 1 2 (stream 'foo 'bar 'baz) 3 4) 'b 'c 'd))
                'foo)
  (check-stream-equal? (lens-set (stream-ref-nested-lens 1 2 0)
                                 (stream 'a (stream 1 2 (stream 'foo 'bar 'baz) 3 4) 'b 'c 'd)
                                 'FOO)
                       (stream 'a (stream 1 2 (stream 'FOO 'bar 'baz) 3 4) 'b 'c 'd))
