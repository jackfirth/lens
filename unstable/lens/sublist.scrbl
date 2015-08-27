#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Sublist lenses}

@defmodule[unstable/lens/sublist]

@defproc[(sublist-lens [i exact-nonnegative-integer?] [j exact-nonnegative-integer?]) lens?]{
@lens-unstable-examples[
  (lens-view (sublist-lens 1 4) '(0 1 2 3 4 5))
  (lens-set (sublist-lens 1 4) '(0 1 2 3 4 5) '(a b c))
]}
