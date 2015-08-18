#lang racket/base

(require racket/contract
         "../base/main.rkt"
         "../util/immutable.rkt"
         "compose.rkt"
         "inverse-function-lens.rkt"
         "join-list.rkt")

(module+ test
  (require rackunit
           "../list/list-ref-take-drop.rkt"))

(provide
 (contract-out
  [lens-join/string (->* () #:rest (listof lens?) (lens/c any/c immutable-string?))]))


(define (lens-join/string . lenses)
  (lens-compose list->string-lens (apply lens-join/list lenses)))

(define list->string-lens
  (inverse-function-lens list->immutable-string string->list))

(module+ test
  (define string-first-third-fifth-lens
    (lens-join/string first-lens
                      third-lens
                      fifth-lens))
  (check-equal? (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f))
                "ace")
  (check-pred immutable? (lens-view string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f)))
  (check-equal? (lens-set string-first-third-fifth-lens '(#\a #\b #\c #\d #\e #\f) "ACE")
                '(#\A #\b #\C #\d #\E #\f)))
