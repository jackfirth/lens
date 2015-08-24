#lang racket/base

(provide match-lens)

(require racket/match
         racket/local
         syntax/parse/define
         lens/private/base/main
         )
(module+ test
  (require rackunit lens/private/test-util/test-lens))

(define-simple-macro (match-lens a:id pat:expr replacement:expr)
  (local [(define (get target)
            (match target
              [pat
               a]))
          (define (set target new-view)
            (match target
              [pat
               (let ([a new-view])
                 replacement)]))]
    (make-lens get set)))

(module+ test
  (define car-lens (match-lens a (cons a b) (cons a b)))
  (define cdr-lens (match-lens b (cons a b) (cons a b)))
  (check-view car-lens (cons 1 2) 1)
  (check-view cdr-lens (cons 1 2) 2)
  (check-set car-lens (cons 1 2) 'a (cons 'a 2))
  (check-set cdr-lens (cons 1 2) 'a (cons 1 'a))
  (test-lens-laws car-lens (cons 1 2) 'a 'b)
  (test-lens-laws cdr-lens (cons 1 2) 'a 'b)
  )
