#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          (for-label lens
                     racket/base
                     racket/contract))


@title{Transforming Values With Lenses}

@defproc[(lens-transform [lens lens?]
                         [target target/c]
                         [transformer (-> view/c view/c)])
         target/c]{
  Transforms the view of @racket[target] through the given @racket[lens]
  with the @racket[transformer] function. Equivalent to getting the
  view of @racket[target] through @racket[lens], passing that value
  to @racket[transformer], then setting the view of @racket[target]
  to the return value of calling @racket[transformer] with the old
  view.
  @lenses-examples[
    (lens-transform first-lens '(1 2 3) number->string)
]}

@defproc[(lens-transform/list [target target/c] [lens lens?] [transformer (-> view/c view/c)] ... ...)
         target/c]{
  Like @racket[lens-transform], except that it can take multiple
  lenses-transformer pairs in the same way as @racket[lens-set*]
  and later transformations overwrite earlier ones in the same way.
  @lenses-examples[
    (lens-transform/list '(1 2 3 4 5)
                         first-lens number->string
                         third-lens (λ (x) (* 100 x)))
]}
