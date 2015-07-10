#lang racket

(provide
 (contract-out
  [hash-ref-lens (-> any/c lens?)]
  [hash-ref-nested-lens (->* () #:rest list? lens?)]))

(require fancy-app
         lens)

(module+ test
  (require rackunit))


(define (hash-ref-lens key)
  (make-lens (hash-ref _ key)
             (hash-set _ key _)))

(define (hash-ref-nested-lens . keys)
  (apply lens-thrush (map hash-ref-lens keys)))

(module+ test
  (define a (hash-ref-lens 'a))
  (define a-x (hash-ref-nested-lens 'a 'x))
  (let-lens [val ctxt] a (hash 'a 1 'b 2 'c 3)
    (check-equal? val 1)
    (check-equal? (ctxt 100) (hash 'a 100 'b 2 'c 3)))
  (check-equal? (lens-transform/list (hash 'a 1 'b 2 'c 3) a (* 10 _))
                (hash 'a 10 'b 2 'c 3))
  (let-lens [val ctxt] a-x (hash 'a (hash 'x 1 'y 2) 'b (hash 'z 3))
    (check-equal? val 1)
    (check-equal? (ctxt 100) (hash 'a (hash 'x 100 'y 2) 'b (hash 'z 3))))
  (check-equal? (lens-transform/list (hash 'a (hash 'x 1 'y 2) 'b (hash 'z 3)) a-x (* 10 _))
                (hash 'a (hash 'x 10 'y 2) 'b (hash 'z 3))))
