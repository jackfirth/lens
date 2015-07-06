#lang racket

(require scribble/manual
         "main.rkt")

(provide deflens
         deflenses)


(define-syntax-rule (deflens lens-id pre-flow ...)
  (defthing lens-id lens? pre-flow ...))

(define-syntax-rule (deflenses (lens-id ...) pre-flow ...)
  (deftogether [(defthing lens-id lens?) ...] pre-flow ...))
