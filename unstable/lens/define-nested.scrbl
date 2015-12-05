#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses for nested data}

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

@lens-unstable-examples[
  (struct/lens ball (mass position velocity) #:transparent)
  (struct/lens position (x y) #:transparent)
  (struct/lens velocity (x y) #:transparent)
  (define-nested-lenses [ball-pos ball-position-lens]
    [x position-x-lens]
    [y position-y-lens])
  (define-nested-lenses [ball-vel ball-velocity-lens]
    [x velocity-x-lens]
    [y velocity-y-lens])
  (lens-view ball-vel-x-lens (ball 1 (position 2 3) (velocity 4 5)))
  (lens-set ball-vel-x-lens (ball 1 (position 2 3) (velocity 4 5)) 1004)
]}
