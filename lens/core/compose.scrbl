#lang scribble/manual

@(require scribble/eval
          "../lenses-examples.rkt"
          (for-label lens
                     racket/base
                     racket/contract))


@title{Composing Lenses}

@defproc[(lens-compose [lens lens?] ...) lens?]{
  Composes the given lenses together into one @italic{compound lens}.
  The compound lens operates similarly to composed functions do in
  that the last @racket[lens] is the first @racket[lens] the compound
  lens's target is viewed through. Each successive lens "zooms in"
  to a more detailed view.
  @lenses-examples[
    (define first-of-second-lens (lens-compose first-lens second-lens))
    (lens-view first-of-second-lens '((1 a) (2 b) (3 c)))
    (lens-set first-of-second-lens '((1 a) (2 b) (3 c)) 200)
]}

@defproc[(lens-thrush [lens lens?] ...) lens?]{
  Like @racket[lens-compose], but each @racket[lens] is combined in the
  opposite order. That is, the first @racket[lens] is the first
  @racket[lens] that the compound lens’s target is viewed through.
  @lenses-examples[
    (define first-of-second-lens (lens-thrush second-lens first-lens))
    (lens-view first-of-second-lens '((1 a) (2 b) (3 c)))
    (lens-set first-of-second-lens '((1 a) (2 b) (3 c)) 200)
]}

@defthing[identity-lens lens?]{
  The identity lens. Performs no destructuring at all - it's view is
  the target itself. For all lenses, both
  @racket[(lens-compose lens identity-lens)] and
  @racket[(lens-compose identity-lens lens)] are equivalent to
  @racket[lens].
  @lenses-examples[
    (lens-view identity-lens 4)
    (lens-set identity-lens 4 'a)
]}
