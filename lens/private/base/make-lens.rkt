#lang racket/base

(require racket/contract/base)
(provide (contract-out [make-lens (-> (-> any/c any/c)
                                      (-> any/c any/c any/c)
                                      lens?)]))

(require "gen-lens.rkt")

(module+ test
  (require rackunit racket/list racket/function))

(struct lens-struct (get set)
  #:methods gen:lens
  [(define (lens-view this target)
     ((lens-struct-get this) target))
   (define (lens-set this target x)
     ((lens-struct-set this) target x))]
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (write-string "#<lens>" out))])

(define (make-lens getter setter)
  (lens-struct getter setter))

(module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (make-lens first set-first))
  (check-exn exn:fail? (thunk (first-lens '(a b c))))
  (let-lens (view-first setter-first) first-lens '(1 2 3 4 5)
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5)))
  (check-equal? (format "~v" first-lens) "#<lens>"))
