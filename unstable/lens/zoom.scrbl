#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses that transform subpieces}

@defmodule[unstable/lens/zoom]

@defproc[(lens-zoom [zoom-lens lens?] [transform-lens lens?]) lens?]{
Creates a lens that transforms the subpiece of the target that @racket[zoom-lens]
views with @racket[transform-lens].

@racketblock[(lens-view (lens-zoom zoom-lens transform-lens) target)]
is equivalent to:
@racketblock[(lens-transform zoom-lens target (λ (v) (lens-view transform-lens v)))]

@lens-unstable-examples[
  (define first-zoom-second-lens
    (lens-zoom first-lens second-lens))
  (lens-view first-zoom-second-lens '((1 2 3) b c))
  (lens-set first-zoom-second-lens '((1 2 3) b c) '(2000 b FOO))
]}

@defproc[(lens-zoom* [zoom-lens lens?] [transform-lens lens?] ... ...) lens?]{
A multi-arg version of @racket[lens-zoom], analogous to
@racket[lens-transform/list]. It is equivalent to
@racket[(lens-thrush (lens-zoom zoom-lens transform-lens) ...)].
@lens-unstable-examples[
  (define first-zoom-second/third-zoom-first-lens
    (lens-zoom* first-lens second-lens
                third-lens first-lens))
  (lens-view first-zoom-second/third-zoom-first-lens '((1 2 3) foo (a b c)))
  (lens-set first-zoom-second/third-zoom-first-lens '((1 2 3) foo (a b c)) '(200 FOO asdf))
]}
