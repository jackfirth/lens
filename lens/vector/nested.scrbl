#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(vector-ref-nested-lens [i exact-nonnegative-integer?] ...) lens?]{
  Like @racket[list-ref-nested-lens], but for vectors.
  Equivalent to @racket[(lens-thrush (vector-ref-lens i) ...)]. 
  @lenses-examples[
    (lens-view (vector-ref-nested-lens 2 1) #(a b #(s i) d))
    (lens-set (vector-ref-nested-lens 2 1) #(a b #(s i) d) "eye")
]}
