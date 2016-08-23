#lang sweet-exp racket/base

provide test-multi*

require racket/match
        racket/string
        racket/format
        syntax/parse/define
        rackunit
        for-syntax racket/base
                   syntax/parse

(define-simple-macro
  (test-multi* ([test-id:id #:in [test-variant:expr ...]] ...)
    body ...)
  #:with [pair-id ...] (generate-temporaries #'[test-id ...])
  #:with [which-test ...] (generate-temporaries #'[test-id ...])
  (for* ([pair-id (in-list (list (cons 'test-variant test-variant) ...))] ...)
    (match-define (cons which-test test-id) pair-id) ...
    (test-case (string-join (list (format "~a = ~a" 'test-id which-test) ...) ", ")
      body ...)))
