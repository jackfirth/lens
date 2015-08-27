#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(lens-thrush [lens lens?] ...) lens?]{
  Like @racket[lens-compose], but each @racket[lens] is combined in the
  opposite order. That is, the first @racket[lens] is the first
  @racket[lens] that the compound lens’s target is viewed through.
  @lens-examples[
    (define first-of-second-lens (lens-thrush second-lens first-lens))
    (lens-view first-of-second-lens '((1 a) (2 b) (3 c)))
    (lens-set first-of-second-lens '((1 a) (2 b) (3 c)) 200)
]}
