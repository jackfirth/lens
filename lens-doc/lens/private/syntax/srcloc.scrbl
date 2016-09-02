#lang scribble/manual

@(require lens/private/doc-util/main (for-label syntax/srcloc))

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

@deftogether[[
  @defthing[source-location->srcloc-lens (lens/c source-location? srcloc?)]
  @defthing[source-location->list-lens (lens/c source-location? source-location-list?)]
  @defthing[source-location->vector-lens (lens/c source-location? source-location-vector?)]
]]{
Lenses for converting from all the common types of source locations
into @racket[srcloc] structures, lists, and vectors.
}

@deftogether[[
  @defthing[source-location-source-lens (lens/c source-location? any/c)]
  @defthing[source-location-line-lens (lens/c source-location? (or/c exact-positive-integer? #f))]
  @defthing[source-location-column-lens (lens/c source-location? (or/c exact-nonnegative-integer? #f))]
  @defthing[source-location-position-lens (lens/c source-location? (or/c exact-positive-integer? #f))]
  @defthing[source-location-span-lens (lens/c source-location? (or/c exact-nonnegative-integer? #f))]
]]{
Like @racket[syntax-source-lens], @racket[syntax-line-lens], etc, but for all
the common types of source locations.
}

