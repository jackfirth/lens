#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          (for-label lens
                     racket/base
                     racket/contract))


@title{Transforming Values With Lenses}

@defproc[(lens-transform [lens lens?]
                         [transformer (-> view/c view/c)]
                         [target target/c])
         target/c]{
  Transforms the view of @racket[target] through the given @racket[lens]
  with the @racket[transformer] function. Equivalent to getting the
  view of @racket[target] through @racket[lens], passing that value
  to @racket[transformer], then setting the view of @racket[target]
  to the return value of calling @racket[transformer] with the old
  view.
  @lenses-examples[
    (lens-transform first-lens number->string '(1 2 3))
]}
