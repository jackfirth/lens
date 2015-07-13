#lang racket/base

(provide lens-view->
         lens-set->
         lens-transform->
         lens-view/thrush
         lens-set/thrush
         lens-transform/thrush
         )

(require lens/base/main)
(module+ test
  (require rackunit racket/list fancy-app))

(define (lens-view-> target . lenses)
  (for/fold ([target target]) ([lens (in-list lenses)])
    (lens-view lens target)))

(define (lens-set-> target #:-> new-val . lenses)
  (lens-set (apply lens-thrush lenses) target new-val))

(define (lens-transform-> target #:-> transformer . lenses)
  (lens-transform (apply lens-thrush lenses) target transformer))

(define lens-view/thrush lens-view->)
(define lens-set/thrush lens-set->)
(define lens-transform/thrush lens-transform->)

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define (set-second l v)
    (list* (first l) v (rest (rest l))))
  (define first-lens (make-lens first set-first))
  (define second-lens (make-lens second set-second))
  (check-equal? (lens-view-> '((1 2) 3) first-lens second-lens)
                2)
  (check-equal? (lens-set-> '((1 2) 3) first-lens second-lens #:-> 'two)
                '((1 two) 3))
  (check-equal? (lens-transform-> '((1 2) 3) first-lens second-lens #:-> (* 100 _))
                '((1 200) 3))
  )
