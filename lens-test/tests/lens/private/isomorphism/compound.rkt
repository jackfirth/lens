#lang sweet-exp racket/base

module+ test
  require lens/private/base/main
          lens/private/compound/identity
          lens/private/isomorphism/data
          lens/private/isomorphism/compound
          rackunit
  (define string->vector-lens (isomorphism-thrush string->list-lens list->vector-lens))
  (check-equal? (lens-view string->vector-lens "abc") #(#\a #\b #\c))
  (check-equal? (lens-set string->vector-lens "abc" #(#\1 #\2 #\3)) "123")
