#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(hash-ref-nested-lens [key any/c] ...) lens?]{
  Contructs a lens that targets hashes with nested hashes
  as values and views the value obtained by using each
  @racket[key] in order.
  @lenses-examples[
    (define foo-bar-lens (hash-ref-nested-lens 'foo 'bar))
    (lens-view foo-bar-lens (hash 'foo (hash 'bar 1)))
    (lens-set foo-bar-lens (hash 'foo (hash 'bar 1)) 1000)
]}
