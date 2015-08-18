#lang scribble/manual

@(require "../doc-util/main.rkt")


@defproc[(lens-join/hash [key any/c] [lens lens?] ... ...) lens?]{
  Constructs a lens that combines the view of each
  @racket[lens] into a hash of views with @racket[key]s
  as the hash keys. In the same manner as @racket[lens-join/list],
  if lenses share views later lenses take precedence when
  setting.
  @lenses-examples[
    (define a-b-lens (lens-join/hash 'a first-lens
                                     'b third-lens))
    (lens-view a-b-lens '(1 2 3))
    (lens-set a-b-lens '(1 2 3) (hash 'a 100 'b 200))
]}
