#lang scribble/manual

@(require lens/private/doc-util/main)


@title{Applicable lenses}


@defmodule[lens/applicable]

This module provides the same functions as @racketmodname[lens],
but enables the use of @italic{applicable lenses}. Applicable lenses
may be used directly as getter functions, removing the need to use
@racket[lens-view].

@lens-applicable-examples[
  (require lens/applicable)
  (first-lens '(a b c))
  (map first-lens '((1 2 3) (a b c) (100 200 300)))
]

Attempting to use non-applicable lenses as functions is an error.

@lens-examples[
  (require lens)
  (first-lens '(a b c))
]
