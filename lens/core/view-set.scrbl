#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          (for-label lens
                     racket/base
                     racket/contract))


@title{Viewing and Setting}

@defproc[(lens-view [lens lens?] [target target/c]) view/c]{
  Extracts the view of @racket[target] with @racket[lens].
  Essentially a getter function.
  @lenses-examples[
    (lens-view first-lens '(1 2 3))
]}

@defproc[(lens-set [lens lens?] [target target/c] [new-view view/c]) target/c]{
  Sets the view of @racket[target] to @racket[new-view] using
  @racket[lens]. Essentially a setter function.
  @lenses-examples[
    (lens-set first-lens '(1 2 3) 'a)
]}

@defproc[(lens-view* [target target/c] [lens lens?] ...) view/c]{
  Like @racket[lens-view], except that it can take multiple lenses,
  which are combined into a nested lens. The argument order is
  switched, so that the @racket[target] comes first and the
  @racket[lens] arguments come after it.
  @racket[(lens-view* target lens ...)] produces the same value as
  @racket[(lens-view (lens-thrush lens ...) target)], but can be more
  efficient.
  @lenses-examples[
    (lens-view* '(a b ((c d) e f) g) third-lens first-lens second-lens)
]}

@defproc[(lens-set* [target target/c] [lens lens?] [new-view view/c] ... ...) target/c]{
  Like @racket[lens-set], except that it can take multiple
  lenses-value pairs. Like @racket[lens-view*], the argument order is
  switched, so that the @racket[target] comes first and the lens-value
  pairs come after it. @racket[lens-set*] is analogous to @racket[hash-set*].
  @lenses-examples[
    (lens-set* '(1 2 3 4 5)
               first-lens 10
               third-lens 300)
]}
