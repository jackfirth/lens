#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    dict-ref-nested-lens (->* [] #:rest (listof any/c) (lens/c functional-dict? any/c))

require lens/private/base/main
        lens/private/compound/thrush
        lens/private/dict
        lens/private/util/functional-dict
module+ test
  require rackunit fancy-app

(define (dict-ref-nested-lens . ks)
  (apply lens-thrush (map dict-ref-lens ks)))

module+ test
  (define a-x (dict-ref-nested-lens 'a 'x))
  (let-lens [val ctxt] a-x '([a . ([x . 1] [y . 2])] '[b . ([z . 3])])
    (check-equal? val 1)
    (check-equal? (ctxt 100) '([a . ([x . 100] [y . 2])] '[b . ([z . 3])])))
  (check-equal? (lens-transform/list '([a . ([x . 1] [y . 2])] '[b . ([z . 3])]) a-x (* 10 _))
                '([a . ([x . 10] [y . 2])] '[b . ([z . 3])]))
