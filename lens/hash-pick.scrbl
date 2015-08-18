#lang scribble/manual

@(require lens/doc-util/main)

@title{Viewing a subset of a hash table by key}

@defmodule[unstable/lens/hash-pick]

@defproc[(hash-pick-lens [key any/c] ...) lens?]{
Creates a lens that views a subset of the target hash-table with the given
@racket[key]s. The view, is another hash-table with only the given keys and
their corrosponding values in the target hash-table.
@lenses-unstable-examples[
  (lens-view (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3))
  (lens-set (hash-pick-lens 'a 'c) (hash 'a 1 'b 2 'c 3) (hash 'a 4 'c 5))
]}
