#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Syntax object source locations}

@defthing[syntax-srcloc-lens (lens/c syntax? srcloc?)]{
A lens that views the source location of a syntax object as a
@racket[srcloc] structure.

@lens-unstable-examples[
  (lens-view syntax-srcloc-lens #'here)
  (lens-set syntax-srcloc-lens #'here (srcloc "a.rkt" 5 8 55 13))
  (syntax-source (lens-set syntax-srcloc-lens #'here (srcloc "a.rkt" 5 8 55 13)))
  (syntax-position (lens-set syntax-srcloc-lens #'here (srcloc "a.rkt" 5 8 55 13)))
]}

@defthing[syntax-source-lens (lens/c syntax? any/c)]{
A lens that views the source field of a syntax object.

@lens-unstable-examples[
  (lens-view syntax-source-lens #'here)
  (lens-set syntax-source-lens #'here "a.rkt")
  (syntax-source (lens-set syntax-source-lens #'here "a.rkt"))
]}

@defthing[syntax-line-lens (lens/c syntax? (or/c exact-positive-integer? #f))]{
A lens that views the line number of a syntax object.

@lens-unstable-examples[
  (lens-view syntax-line-lens #'here)
  (lens-set syntax-line-lens #'here 8)
  (syntax-line (lens-set syntax-line-lens #'here 8))
]}

@defthing[syntax-column-lens (lens/c syntax? (or/c exact-nonnegative-integer? #f))]{
A lens that views the column number of a syntax object within its line.

@lens-unstable-examples[
  (lens-view syntax-column-lens #'here)
  (lens-set syntax-column-lens #'here 13)
  (syntax-column (lens-set syntax-column-lens #'here 13))
]}

@defthing[syntax-position-lens (lens/c syntax? (or/c exact-positive-integer? #f))]{
A lens that views the source position a syntax object.

@lens-unstable-examples[
  (lens-view syntax-position-lens #'here)
  (lens-set syntax-position-lens #'here 21)
  (syntax-position (lens-set syntax-position-lens #'here 21))
]}

@defthing[syntax-span-lens (lens/c syntax? (or/c exact-nonnegative-integer? #f))]{
A lens that views the source span a syntax object.

@lens-unstable-examples[
  (lens-view syntax-span-lens #'here)
  (lens-set syntax-span-lens #'here 34)
  (syntax-span (lens-set syntax-span-lens #'here 34))
]}

