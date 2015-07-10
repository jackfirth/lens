#lang scribble/manual

@(require scribble/eval
          lens/lenses-examples
          (for-label lens
                     unstable/lens
                     racket/base
                     racket/contract))


@title{Compound Lenses}

@defmodule[unstable/lens/compound]

@defproc[(compound-list-lens [lens lens?] ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a list of views. This lens can
  be used to view and set a list of values in a single
  target. If any of the lenses share views, then when
  setting the later lenses override the earlier ones.
  @lenses-unstable-examples[
    (define first-third-fifth-lens
      (compound-list-lens first-lens
                          third-lens
                          fifth-lens))
    (lens-view first-third-fifth-lens '(a b c d e f))
    (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
]}
