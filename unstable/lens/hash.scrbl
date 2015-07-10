#lang scribble/manual

@(require lens/doc-util/main)


@title{Hash Lenses}

@defmodule[unstable/lens/hash]

@defproc[(hash-ref-lens [key any/c]) lens?]{
  Constructs a lens that targets hashes and views the value
  of @racket[key].
  @lenses-unstable-examples[
    (define foo-lens (hash-ref-lens 'foo))
    (lens-view foo-lens (hash 'foo 10 'bar 20))
    (lens-set foo-lens (hash 'foo 10 'bar 20) 1000)
]}

@defproc[(hash-ref-nested-lens [key any/c] ...) lens?]{
  Contructs a lens that targets hashes with nested hashes
  as values and views the value obtained by using each
  @racket[key] in order.
  @lenses-unstable-examples[
    (define foo-bar-lens (hash-ref-nested-lens 'foo 'bar))
    (lens-view foo-bar-lens (hash 'foo (hash 'bar 1)))
    (lens-set foo-bar-lens (hash 'foo (hash 'bar 1)) 1000)
]}
