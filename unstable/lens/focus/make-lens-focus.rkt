#lang sweet-exp racket/base

provide make-lens/focus lens-lambda lens-match-lambda

require lens/private/base/gen-lens
        racket/match

(struct lens/focus (focuser)
  #:methods gen:lens
  [(define (focus-lens this target)
     ((lens/focus-focuser this) target))])

(define (make-lens/focus focuser)
  (lens/focus focuser))

(define-syntax-rule (lens-lambda (tgt) body ...)
  (make-lens/focus (lambda (tgt) body ...)))

(define-syntax-rule (lens-match-lambda [pat body ...] ...)
  (make-lens/focus (match-lambda [pat body ...] ...)))

