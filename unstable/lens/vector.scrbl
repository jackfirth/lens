#lang scribble/manual

@(require lens/doc-util/main)

@title{Vector lenses}

@defproc[(vector-ref-lens [i exact-nonnegative-integer?]) lens?]{
Returns a lens that views an element of a vector.
@lenses-unstable-examples[
  (lens-view (vector-ref-lens 2) #(a b c d))
  (lens-set (vector-ref-lens 2) #(a b c d) "sea")
]}

@defproc[(vector-ref-nested-lens [i exact-nonnegative-integer?] ...) lens?]{
Like @racket[list-ref-nested-lens], but for vectors.
Equivalent to @racket[(lens-thrush (vector-ref-lens i) ...)].
@lenses-unstable-examples[
  (lens-view (vector-ref-nested-lens 2 1) #(a b #(s i) d))
  (lens-set (vector-ref-nested-lens 2 1) #(a b #(s i) d) "eye")
]}

@defproc[(vector-pluck-lens [i exact-nonnegative-integer?] ...) lens?]{
Like @racket[list-refs-lens], but for vectors.
Equivalent to @racket[(lens-join/vector (vector-ref-lens i) ...)].
@lenses-unstable-examples[
  (define 1-5-6-lens (vector-pluck-lens 1 5 6))
  (lens-view 1-5-6-lens #(a b c d e f g))
  (lens-set 1-5-6-lens #(a b c d e f g) #(1 2 3))
]}
