#lang scribble/manual

@(require scribble/eval
          "../../doc-util/main.rkt")

@(define make-lens-eval
   (make-eval-factory '(racket/base lens)))

@(define-syntax-rule (lens-interaction expr ...)
   (interaction #:eval (make-lens-eval) expr ...))

@title[#:tag "builtin-lenses"]{Lenses on Built-In Datatypes}


