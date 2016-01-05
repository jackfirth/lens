#lang sweet-exp racket/base

require racket/contract
        unstable/lens/isomorphism/base
        "../base/main.rkt"
        "../util/immutable.rkt"
        "../util/rest-contract.rkt"
        "../compound/compose.rkt"
        "../list/join-list.rkt"

module+ test
  require rackunit
          "../list/list-ref-take-drop.rkt"
          "../test-util/test-lens.rkt"

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
