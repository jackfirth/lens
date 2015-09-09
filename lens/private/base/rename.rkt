#lang sweet-exp racket/base

require racket/contract/base
provide
  contract-out
    lens-rename (-> lens? symbol? lens?)

require racket/generic
        racket/match
        "gen-lens.rkt"
module+ test
  require rackunit racket/list racket/function "make-lens.rkt"

(struct renamed-lens (lens name)
  #:methods gen:lens
  [(define/generic view lens-view)
   (define/generic set lens-set)
   (define/generic focus focus-lens)
   (define (lens-view this target)
     (view (renamed-lens-lens this) target))
   (define (lens-set this target x)
     (set (renamed-lens-lens this) target x))
   (define (focus-lens this target)
     (focus (renamed-lens-lens this) target))]
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (fprintf out "#<lens:~a>" (renamed-lens-name this)))])

(define (lens-rename lens name)
  (match lens
    [(renamed-lens lens _)
     (renamed-lens lens name)]
    [lens
     (renamed-lens lens name)]))

module+ test
  (define (set-first l v)
    (list* v (rest l)))
  (define first-lens (lens-rename (make-lens first set-first) 'first-lens))
  (let-lens (view-first setter-first) first-lens '(1 2 3 4 5)
    (check-eqv? view-first 1)
    (check-equal? (setter-first 'a) '(a 2 3 4 5)))
  (check-equal? (format "~v" first-lens) "#<lens:first-lens>")
