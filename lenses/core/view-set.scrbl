#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          (for-label lenses
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
