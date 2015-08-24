#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(vector-ref-lens [i exact-nonnegative-integer?]) lens?]{
Returns a lens that views an element of a vector.
@lenses-examples[
  (lens-view (vector-ref-lens 2) #(a b c d))
  (lens-set (vector-ref-lens 2) #(a b c d) "sea")
]}
