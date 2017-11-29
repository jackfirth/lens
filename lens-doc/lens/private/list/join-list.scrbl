#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Joining lenses to view lists}

@defproc[(lens-join/list [lens lens?] ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a list of views. This lens can
  be used to view and set a list of values in a single
  target.

  The joined lens only follows the
  @seclink["laws"]{lens laws} if the views of the
  argument lenses don't overlap. Views of the lenses
  overlap when setting one can change the view of
  another lens.

  @lens-examples[
    (define first-third-fifth-lens
      (lens-join/list first-lens
                      third-lens
                      fifth-lens))
    (lens-view first-third-fifth-lens '(a b c d e f))
    (lens-set first-third-fifth-lens '(a b c d e f) '(1 2 3))
]}
