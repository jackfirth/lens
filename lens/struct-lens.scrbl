#lang scribble/manual

@(require scribble/eval
          "lenses-examples.rkt"
          (for-label lenses
                     lenses/struct-lens
                     racket/base
                     racket/contract))


@title{Struct lenses}

@defmodule[lenses/struct-lens]

@defform[(struct-lens struct-id field-id)]{
  Returns a lens for viewing the @racket[field-id] field of
  a @racket[struct-id] instance.
  @lenses-examples[
    (require lenses/struct-lens)
    (struct foo (a b c) #:transparent)
    (lens-view (struct-lens foo a) (foo 1 2 3))
    (lens-set (struct-lens foo a) (foo 1 2 3) 100)
]}

