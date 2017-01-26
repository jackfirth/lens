#lang sweet-exp racket/base

module+ test
  require lens/private/base/base
          lens/private/list/main
          lens/private/compound/define-nested
          rackunit
  (define-nested-lenses [first first-lens]
    [first first-lens]
    [second second-lens]
    [third third-lens
      [first first-lens]
      [second second-lens]])
  (check-equal? (lens-view first-first-lens '((a b c d) e)) 'a)
  (check-equal? (lens-view first-second-lens '((a b c d) e)) 'b)
  (check-equal? (lens-view first-third-lens '((a b c d) e)) 'c)
  (check-equal? (lens-view first-third-first-lens '((a b (c d) e) f)) 'c)
  (check-equal? (lens-view first-third-second-lens '((a b (c d) e) f)) 'd)

