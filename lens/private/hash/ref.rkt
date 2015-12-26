#lang racket/base

(require racket/contract
         fancy-app
         "../base/main.rkt"
         "../base/rename.rkt"
         "../util/immutable.rkt")

(module+ test
  (require rackunit))

(provide
 (contract-out
  [hash-ref-lens (-> any/c (lens/c immutable-hash? any/c))]))


(define (hash-ref-lens key)
  (lens-rename
   (make-lens (hash-ref _ key)
              (hash-set _ key _))
   `(hash-ref-lens ,(format "~v" key))))

(module+ test
  (define a (hash-ref-lens 'a))
  (let-lens [val ctxt] a (hash 'a 1 'b 2 'c 3)
    (check-equal? val 1)
    (check-equal? (ctxt 100) (hash 'a 100 'b 2 'c 3)))
  (check-equal? (lens-transform/list (hash 'a 1 'b 2 'c 3) a (* 10 _))
                (hash 'a 10 'b 2 'c 3)))
