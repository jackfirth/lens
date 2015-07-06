#lang racket/base

(provide struct-lens)

(require racket/local
         syntax/parse/define
         alexis/util/struct
         "main.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     ))
(module+ test
  (require rackunit fancy-app (only-in lenses lens-transform*)))

(define-simple-macro (struct-lens s:id fld:id)
  #:with s-fld      (format-id #'s "~a-~a" #'s #'fld #:source #'fld)
  #:with s-fld-set  (format-id #'s "~a-~a-set" #'s #'fld #:source #'fld)
  (local [(define-struct-updaters s)]
    (make-lens s-fld s-fld-set)))

(module+ test
  (struct foo (a b c) #:transparent)
  (define foo-a-lens (struct-lens foo a))
  (define foo-b-lens (struct-lens foo b))
  (define foo-c-lens (struct-lens foo c))
  (define f (foo 1 2 3))
  (check-equal? (lens-transform* f foo-a-lens (* 100 _)) (foo 100 2 3))
  (check-equal? (lens-transform* f foo-b-lens (* 100 _)) (foo 1 200 3))
  (check-equal? (lens-transform* f foo-c-lens (* 100 _)) (foo 1 2 300))
  )
