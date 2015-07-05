#lang racket/base

(provide car-lens cdr-lens)

(require racket/match
         fancy-app
         "../core/main.rkt"
         )

(define (car-lens v)
  (match-define (cons car cdr) v)
  (values car (cons _ cdr))) ; fancy-app

(define (cdr-lens v)
  (match-define (cons car cdr) v)
  (values cdr (cons car _)))

