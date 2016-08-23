#lang sweet-exp racket/base

require racket/contract
        lens/private/base/main
        lens/private/isomorphism/base
        lens/private/compound/compose
        lens/private/util/rest-contract
        "../util/immutable.rkt"
        "../list/join-list.rkt"

module+ test
  require rackunit
          lens/private/test-util/test-lens
          "../list/list-ref-take-drop.rkt"

provide
  contract-out
    lens-join/string (rest-> (lens/c any/c char?) (lens/c any/c immutable-string?))


(define (lens-join/string . lenses)
  (lens-compose list->string-lens (apply lens-join/list lenses)))

(define list->string-lens
  (make-isomorphism-lens list->immutable-string string->list))

(module+ test
  (define string-first-third-fifth-lens
    (lens-join/string first-lens
                      third-lens
                      fifth-lens))
  (check-lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f)
                   "ace")
  (check-pred immutable? (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f)))
  (check-lens-set string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f) "ACE"
                  '(#\A #\b #\C #\d #\E #\f)))
