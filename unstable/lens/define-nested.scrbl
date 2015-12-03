#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses for nested data}

@defmodule[unstable/lens/define-nested]

@defform[(define-nested-lenses [base-id base-lens-expr] clause ...)
         #:grammar ([clause [sub-id sub-lens-expr]])]{
A shorthand for defining composed lenses for nested data structures.

For example, if there is a @racket[top] struct containing a
@racket[middle] struct, which contains an @racket[x] field and a
@racket[y] field, a form like:
@(racketblock
  (define-nested-lenses [top-middle top-middle-lens]
    [x middle-x-lens]
    [y middle-y-lens]))
Will define @racket[top-middle-x-lens] and @racket[top-middle-y-lens]
as @racket[(lens-thrush top-middle-lens middle-x-lens)] and
@racket[(lens-thrush top-middle-lens middle-y-lens)].
}
