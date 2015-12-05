#lang scribble/manual

@(require lens/private/doc-util/main)

@title{Lenses for membership of a set}

@defproc[(set-member-lens [v any/c]) (lens/c functional-set? boolean?)]{
Creates a lens for telling whether @racket[v] is a member of the target set.
@lens-unstable-examples[
  (define 2-lens (set-member-lens 2))
  (lens-view 2-lens (set 1 2 3))
  (lens-view 2-lens (set 1 3))
  (lens-set 2-lens (set 1 2 3) #t)
  (lens-set 2-lens (set 1 2 3) #f)
  (lens-set 2-lens (set 1 3) #t)
  (lens-set 2-lens (set 1 3) #f)
]}
