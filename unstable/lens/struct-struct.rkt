#lang sweet-exp racket/base

provide struct->struct-lens

require racket/local
        unstable/lens/isomorphism/base
        for-syntax racket/base
                   syntax/parse
                   "private/struct-id.rkt"
module+ test
  require lens/private/base/base
          rackunit

(define-syntax struct->struct-lens
  (lambda (stx)
    (syntax-parse stx
      [(struct->struct-lens s1:struct-id s2:struct-id)
       (unless (= (length (syntax->list #'[s1.accessor-id ...]))
                  (length (syntax->list #'[s2.accessor-id ...])))
         (raise-syntax-error #f (format "~a and ~a have different numbers of fields"
                                        (syntax-e #'s1) (syntax-e #'s2))
                             stx))
       #'(local [(define (s1->s2 struct1)
                   (#%app s2.constructor-id (s1.accessor-id struct1) ...))
                 (define (s2->s1 struct2)
                   (#%app s1.constructor-id (s2.accessor-id struct2) ...))]
           (make-isomorphism-lens s1->s2 s2->s1))])))

module+ test
  (struct foo (a b c))
  (struct bar (x y z))
  (test-case "without inheritance"
    (check-match (lens-view (struct->struct-lens foo bar) (foo 1 2 3)) (bar 1 2 3))
    (check-match (lens-set (struct->struct-lens foo bar) (foo 1 2 3) (bar 4 5 6)) (foo 4 5 6))
    (check-match (lens-view (struct->struct-lens bar foo) (bar 1 2 3)) (foo 1 2 3))
    (check-match (lens-set (struct->struct-lens bar foo) (bar 1 2 3) (foo 4 5 6)) (bar 4 5 6)))
  (struct a foo (d e))
  (struct b- bar (u))
  (struct b b- (v))
  (test-case "inheriting from foo and bar"
    (check-match (lens-view (struct->struct-lens a b) (a 1 2 3 4 5)) (b 1 2 3 4 5))
    (check-match (lens-set (struct->struct-lens a b) (a 1 2 3 4 5) (b 6 7 8 9 10)) (a 6 7 8 9 10))
    (check-match (lens-view (struct->struct-lens b a) (b 1 2 3 4 5)) (a 1 2 3 4 5))
    (check-match (lens-set (struct->struct-lens b a) (b 1 2 3 4 4) (a 6 7 8 9 10)) (b 6 7 8 9 10)))

