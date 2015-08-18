#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(vector-pick-lens [i exact-nonnegative-integer?] ...) lens?]{
  Like @racket[list-refs-lens], but for vectors.
  Equivalent to @racket[(lens-join/vector (vector-ref-lens i) ...)].
  @lenses-examples[
    (define 1-5-6-lens (vector-pick-lens 1 5 6))
    (lens-view 1-5-6-lens #(a b c d e f g))
    (lens-set 1-5-6-lens #(a b c d e f g) #(1 2 3))
]}
