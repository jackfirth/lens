#lang scribble/manual

@(require lens/doc-util/main)

@title{Filtering sets}

@defmodule[unstable/lens/set-filterer]

@defproc[(set-filterer-lens [pred (-> any/c any/c)]) lens?]{
Creates a lens that filters a set by the predicate @racket[pred].
@lenses-unstable-examples[
  (lens-view (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e))
  (lens-set (set-filterer-lens number?) (set 1 'a 2 'b 'c 3 'd 'e) (set 4 5 6 7))
]}
