#lang scribble/manual

@(require lens/doc-util/main)


@title{Joining Lenses}

@defmodule[unstable/lens/join]

@defproc[(lens-join/list [lens lens?] ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a list of views. This lens can
  be used to view and set a list of values in a single
  target. If any of the lenses share views, then when
  setting the later lenses override the earlier ones.
  @lenses-unstable-examples[
    (define first-third-fifth-lens
      (lens-join/list first-lens
                      third-lens
                      fifth-lens))
    (lens-view first-third-fifth-lens '(a b c d e f))
    (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
]}

@defproc[(lens-join/hash [key any/c] [lens lens?] ... ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a hash of views with @racket[key]s
  as the hash keys. In the same manner as @racket[lens-join/list],
  if lenses share views later lenses take precedence when
  setting.
  @lenses-unstable-examples[
    (define a-b-lens (lens-join/hash 'a first-lens
                                     'b third-lens))
    (lens-view a-b-lens '(1 2 3))
    (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
]}
