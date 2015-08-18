#lang racket/base

(require "../base/main.rkt")

(provide inverse-function-lens)


(define (inverse-function-lens f f-inv)
  (make-lens
   (λ (tgt) (f tgt))
   (λ (tgt v) (f-inv v))))
