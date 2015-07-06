#lang racket/base

(provide car-lens cdr-lens)

(require racket/match
         fancy-app
         "../core/main.rkt"
         "../core/lens-lambda.rkt"
         )

(define car-lens
  (lens-lambda (v)
    (match-define (cons car cdr) v)
    (values car (cons _ cdr)))) ; fancy-app

(define cdr-lens
  (lens-lambda (v)
    (match-define (cons car cdr) v)
    (values cdr (cons car _))))

