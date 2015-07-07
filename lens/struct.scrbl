#lang scribble/manual

@(require scribble/eval
          "lenses-examples.rkt"
          (for-label lens
                     lens/struct
                     racket/base
                     racket/contract))


@title{Struct lenses}

@defmodule[lens/struct]

@defform[(struct-lens struct-id field-id)]{
  Returns a lens for viewing the @racket[field-id] field of
  a @racket[struct-id] instance.
  @lenses-examples[
    (require lens/struct)
    (struct foo (a b c) #:transparent)
    (lens-view (struct-lens foo a) (foo 1 2 3))
    (lens-set (struct-lens foo a) (foo 1 2 3) 100)
]}
