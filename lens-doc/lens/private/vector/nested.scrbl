#lang scribble/manual

@(require lens/private/doc-util/main)


@defproc[(vector-ref-nested-lens [i exact-nonnegative-integer?] ...) lens?]{
  Like @racket[list-ref-nested-lens], but for vectors.
  Equivalent to @racket[(lens-thrush (vector-ref-lens i) ...)]. 
  @lens-examples[
    (lens-view (vector-ref-nested-lens 2 1) #(a b #(s i) d))
    (lens-set (vector-ref-nested-lens 2 1) #(a b #(s i) d) "eye")
]}
