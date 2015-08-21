#lang sweet-exp racket/base

provide string->symbol-lens
        symbol->string-lens
        number->string-lens
        string->number-lens
        list->vector-lens
        vector->list-lens
        list->string-lens
        string->list-lens

require lens/base/main
        "base.rkt"

module+ test
  require rackunit


(define-values [string->symbol-lens symbol->string-lens]
  (make-isomorphism-lenses string->symbol symbol->string))
(define-values [number->string-lens string->number-lens]
  (make-isomorphism-lenses number->string string->number))
(define-values [list->vector-lens vector->list-lens]
  (make-isomorphism-lenses list->vector vector->list))
(define-values [list->string-lens string->list-lens]
  (make-isomorphism-lenses list->string string->list))


(module+ test
  (test-case "string-symbol"
    (check-equal? (lens-view string->symbol-lens "a") 'a)
    (check-equal? (lens-set string->symbol-lens "a" 'b) "b")
    (check-equal? (lens-view symbol->string-lens 'a) "a")
    (check-equal? (lens-set symbol->string-lens 'a "b") 'b))
  (test-case "number-string"
    (check-equal? (lens-view number->string-lens 5) "5")
    (check-equal? (lens-set number->string-lens 5 "6") 6)
    (check-equal? (lens-view string->number-lens "5") 5)
    (check-equal? (lens-set string->number-lens "5" 6) "6"))
  (test-case "inverses"
    (check-equal? (isomorphism-lens-inverse string->symbol-lens) symbol->string-lens)
    (check-equal? (isomorphism-lens-inverse symbol->string-lens) string->symbol-lens)
    (check-equal? (isomorphism-lens-inverse number->string-lens) string->number-lens)
    (check-equal? (isomorphism-lens-inverse string->number-lens) number->string-lens)))
