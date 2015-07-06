#lang scribble/manual

@(require scribble/eval
          "lenses-examples.rkt"
          (for-label lenses
                     racket/base
                     racket/contract))


@title{Applicable lenses}


@defmodule[lenses/applicable]

This module provides the same functions as @racketmodname[lenses],
but enables the use of @italic{applicable lenses}. Applicable lenses
may be used directly as getter functions, removing the need to use
@racket[lens-view].

@lenses-applicable-examples[
  (require lenses/applicable)
  (first-lens '(a b c))
  (map first-lens '((1 2 3) (a b c) (100 200 300)))
]

Attempting to use non-applicable lenses as functions is an error.

@lenses-examples[
  (require lenses)
  (first-lens '(a b c))
]
