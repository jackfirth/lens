#lang scribble/manual

@(require "doc-util/main.rkt")

@title{String Lenses}

@defproc[(string-ref-lens [i exact-nonnegative-integer?]) lens?]{
Returns a lens for viewing the @racket[i]th character of a string.
@lenses-examples[
  (lens-view (string-ref-lens 2) "abcdef")
  (lens-set (string-ref-lens 2) "abcdef" #\C)
]}

@defproc[(string-pick-lens [i exact-nonnegative-integer?]) lens?]{
Like @racket[list-refs-lens], but for strings.
Equivalent to @racket[(lens-join/string (string-ref-lens i) ...)].
@lenses-examples[
  (define 1-5-6-lens (string-pick-lens 1 5 6))
  (lens-view 1-5-6-lens "abcdefg")
  (lens-set 1-5-6-lens "abcdefg" "BFG")
]}
